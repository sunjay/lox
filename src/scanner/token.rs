use std::sync::Arc;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Token {
    pub line: usize,
    pub kind: TokenKind,
}
