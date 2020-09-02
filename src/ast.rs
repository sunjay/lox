use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Number(NumLit),
    String(StrLit),
    Bool(BoolLit),
    Ident(Ident),
    Nil(Nil),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: BinaryOp,
    pub right: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumLit {
    pub value: f64,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StrLit {
    pub value: Arc<[u8]>,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLit {
    pub value: bool,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub value: Arc<str>,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Nil {
    pub line: usize,
}
