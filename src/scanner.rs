mod token;

pub use token::*;

use std::sync::Arc;

use crate::diag::Diagnostic;

pub fn scan_tokens(source_code: &[u8]) -> anyhow::Result<Vec<Token>> {
    let scanner = Scanner {
        input: source_code,
        line: 1,
        reached_eof: false,
    };

    scanner.collect()
}

#[derive(Debug)]
struct Scanner<'a> {
    /// The remaining input that still needs to be scanned
    input: &'a [u8],
    /// The current line number
    line: usize,
    /// true if we've already yielded eof
    reached_eof: bool,
}

impl<'a> Scanner<'a> {
    fn ignore_whitespace_comments(&mut self) {
        while self.ignore_whitespace() || self.ignore_comments() {}
    }

    fn ignore_whitespace(&mut self) -> bool {
        let mut found_whitespace = false;

        while let Some(&byte) = self.input.get(0) {
            if byte.is_ascii_whitespace() {
                if byte == b'\n' {
                    self.line += 1;
                }

                self.input = &self.input[1..];
                found_whitespace = true;

            } else {
                break;
            }
        }

        found_whitespace
    }

    fn ignore_comments(&mut self) -> bool {
        if self.input.get(..2) != Some(b"//") {
            return false;
        }

        while let Some(&byte) = self.input.get(0) {
            self.input = &self.input[1..];

            if byte == b'\n' {
                self.line += 1;

                break;
            }
        }

        true
    }

    /// Matches a string literal, assuming that the first character of the input is currently the
    /// opening quote
    fn str_lit(&mut self) -> anyhow::Result<Arc<[u8]>> {
        // Advance past the opening quote
        self.input = &self.input[1..];
        // Store the input starting at the first byte of the string
        let slice = self.input;
        // Count the number of bytes found inside the quotes
        let mut found = 0;
        // Record the start line for a more useful error message
        let start_line = self.line;

        while let Some(&byte) = self.input.get(0) {
            self.input = &self.input[1..];

            // Keep the line count accurate, even when dealing with multi-line strings
            if byte == b'\n' {
                self.line += 1;
            }

            match byte {
                b'"' => return Ok(slice[..found].into()),
                _ => found += 1,
            }
        }

        Err(Diagnostic {
            line: start_line,
            message: "Unterminated string".to_string(),
        }.into())
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = anyhow::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reached_eof {
            return None;
        }

        self.ignore_whitespace_comments();

        let line = self.line;

        // Match on the token kind as well as a number that indicates how far to advance the input
        let (kind, advance) = match (self.input.get(0), self.input.get(1)) {
            (Some(b'('), _) => (TokenKind::LeftParen, 1),
            (Some(b')'), _) => (TokenKind::RightParen, 1),
            (Some(b'{'), _) => (TokenKind::LeftBrace, 1),
            (Some(b'}'), _) => (TokenKind::RightBrace, 1),
            (Some(b','), _) => (TokenKind::Comma, 1),
            (Some(b'.'), _) => (TokenKind::Dot, 1),
            (Some(b'-'), _) => (TokenKind::Minus, 1),
            (Some(b'+'), _) => (TokenKind::Plus, 1),
            (Some(b';'), _) => (TokenKind::Semicolon, 1),
            (Some(b'*'), _) => (TokenKind::Star, 1),
            (Some(b'/'), _) => (TokenKind::Slash, 1),

            (Some(b'!'), Some(b'=')) => (TokenKind::BangEqual, 2),
            (Some(b'!'), _) => (TokenKind::Bang, 1),
            (Some(b'='), Some(b'=')) => (TokenKind::EqualEqual, 2),
            (Some(b'='), _) => (TokenKind::Equal, 1),
            (Some(b'<'), Some(b'=')) => (TokenKind::LessEqual, 2),
            (Some(b'<'), _) => (TokenKind::Less, 1),
            (Some(b'>'), Some(b'=')) => (TokenKind::GreaterEqual, 2),
            (Some(b'>'), _) => (TokenKind::Greater, 1),

            // Return advance = 0 for dynamically-sized tokens since the matchers for those will
            // advance the input themselves
            (Some(b'"'), _) => match self.str_lit() {
                Ok(lit) => (TokenKind::String(lit), 0),
                Err(err) => return Some(Err(err)),
            },

            (Some(&c), _) => return Some(Err(Diagnostic {
                line: self.line,
                message: format!("Unexpected character: `{}`", c as char),
            }.into())),

            (None, _) => {
                self.reached_eof = true;
                (TokenKind::Eof, 0)
            },
        };

        // Advance the input
        self.input = &self.input[advance..];

        Some(Ok(Token {line, kind}))
    }
}
