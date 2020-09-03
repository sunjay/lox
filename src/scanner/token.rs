use std::{fmt, str};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One Or Two Character Tokens

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals

    Identifier(Arc<str>),
    String(Arc<[u8]>),
    Number(f64),

    // Keywords

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End of file
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            LeftParen => write!(f, "`(`"),
            RightParen => write!(f, "`)`"),
            LeftBrace => write!(f, "`{{`"),
            RightBrace => write!(f, "`}}`"),
            Comma => write!(f, "`,`"),
            Dot => write!(f, "`.`"),
            Minus => write!(f, "`-`"),
            Plus => write!(f, "`+`"),
            Semicolon => write!(f, "`;`"),
            Slash => write!(f, "`/`"),
            Star => write!(f, "`*`"),
            Bang => write!(f, "`!`"),
            BangEqual => write!(f, "`!=`"),
            Equal => write!(f, "`=`"),
            EqualEqual => write!(f, "`==`"),
            Greater => write!(f, "`>`"),
            GreaterEqual => write!(f, "`>=`"),
            Less => write!(f, "`<`"),
            LessEqual => write!(f, "`<=`"),

            Identifier(ident) => write!(f, "`{}`", ident),
            String(lit) => match str::from_utf8(lit) {
                Ok(lit) => write!(f, "\"{}\"", lit),
                Err(_) => write!(f, "<invalid utf-8>"),
            },
            Number(lit) => write!(f, "{}", lit),

            And => write!(f, "`and`"),
            Class => write!(f, "`class`"),
            Else => write!(f, "`else`"),
            False => write!(f, "`false`"),
            Fun => write!(f, "`fun`"),
            For => write!(f, "`for`"),
            If => write!(f, "`if`"),
            Nil => write!(f, "`nil`"),
            Or => write!(f, "`or`"),
            Print => write!(f, "`print`"),
            Return => write!(f, "`return`"),
            Super => write!(f, "`super`"),
            This => write!(f, "`this`"),
            True => write!(f, "`true`"),
            Var => write!(f, "`var`"),
            While => write!(f, "`while`"),

            Eof => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line: usize,
    pub kind: TokenKind,
}
