use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    VarDecl(VarDecl),
    Stmt(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Ident,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print(PrintStmt),
    Expr(Expr),
    Block(Block),
    If(Box<Cond>),
    While(Box<WhileLoop>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintStmt {
    pub print_token_line: usize,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub cond: Expr,
    pub if_body: Stmt,
    pub else_body: Option<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    pub cond: Expr,
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    LogicalAnd(Box<LogicalAnd>),
    LogicalOr(Box<LogicalOr>),
    Assign(Box<Assign>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Number(NumLit),
    String(StrLit),
    Bool(BoolLit),
    Ident(Ident),
    Nil(Nil),
}

impl Expr {
    pub fn line(&self) -> usize {
        use Expr::*;
        match self {
            LogicalAnd(expr) => expr.line(),
            LogicalOr(expr) => expr.line(),
            Assign(expr) => expr.line(),
            Binary(expr) => expr.line(),
            Unary(expr) => expr.line(),
            Number(value) => value.line,
            String(value) => value.line,
            Bool(value) => value.line,
            Ident(value) => value.line,
            Nil(value) => value.line,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalAnd {
    pub left: Expr,
    pub right: Expr,
}

impl LogicalAnd {
    pub fn line(&self) -> usize {
        self.left.line()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalOr {
    pub left: Expr,
    pub right: Expr,
}

impl LogicalOr {
    pub fn line(&self) -> usize {
        self.left.line()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub lvalue: LValue,
    pub rhs: Expr,
}

impl Assign {
    pub fn line(&self) -> usize {
        self.lvalue.line()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Ident(Ident),
}

impl LValue {
    pub fn line(&self) -> usize {
        use LValue::*;
        match self {
            Ident(ident) => ident.line,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: BinaryOp,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn line(&self) -> usize {
        self.left.line()
    }
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Equal => write!(f, "=="),
            NotEqual => write!(f, "!="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Expr,
}

impl UnaryExpr {
    pub fn line(&self) -> usize {
        self.value.line()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryOp::*;
        match self {
            Not => write!(f, "!"),
            Neg => write!(f, "-"),
        }
    }
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
