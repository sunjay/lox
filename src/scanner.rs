mod token;

pub use token::*;

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

impl<'a> Iterator for Scanner<'a> {
    type Item = anyhow::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reached_eof {
            return None;
        }

        let line = self.line;

        let kind = match self.input.get(0) {
            Some(b'(') => TokenKind::LeftParen,
            Some(b')') => TokenKind::RightParen,
            Some(b'{') => TokenKind::LeftBrace,
            Some(b'}') => TokenKind::RightBrace,
            Some(b',') => TokenKind::Comma,
            Some(b'.') => TokenKind::Dot,
            Some(b'-') => TokenKind::Minus,
            Some(b'+') => TokenKind::Plus,
            Some(b';') => TokenKind::Semicolon,
            Some(b'*') => TokenKind::Star,

            Some(_) => todo!(),

            None => {
                self.reached_eof = true;
                TokenKind::Eof
            },
        };

        // Advance the input
        if !self.input.is_empty() {
            self.input = &self.input[1..];
        }

        Some(Ok(Token {line, kind}))
    }
}
