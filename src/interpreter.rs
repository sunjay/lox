mod value;

pub use value::*;

use crate::ast;
use crate::diag::Diagnostic;

#[derive(Debug, Default)]
pub struct Interpreter {
}

impl Interpreter {
    pub fn eval(&mut self, expr: ast::Expr) -> anyhow::Result<Value> {
        expr.eval(self)
    }
}

pub trait Evaluate {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value>;
}

impl Evaluate for ast::Expr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        use ast::Expr::*;
        match self {
            Binary(expr) => expr.eval(ctx),
            Unary(expr) => expr.eval(ctx),
            Number(expr) => expr.eval(ctx),
            String(expr) => expr.eval(ctx),
            Bool(expr) => expr.eval(ctx),
            Ident(expr) => expr.eval(ctx),
            Nil(expr) => expr.eval(ctx),
        }
    }
}

impl Evaluate for ast::BinaryExpr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {left, op, right} = self;
        let line = left.line();

        let left = left.eval(ctx)?;
        let left_type = left.typ();
        let right = right.eval(ctx)?;
        let right_type = right.typ();

        let unsupported_operator = || Diagnostic {
            line,
            message: format!("unsupported binary operator `{}` for types `{}` and `{}`", op, left_type, right_type),
        };

        use ast::BinaryOp::*;
        Ok(match (left, right) {
            (Value::Number(a), Value::Number(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                Greater => Value::Bool(a > b),
                GreaterEqual => Value::Bool(a >= b),
                Less => Value::Bool(a < b),
                LessEqual => Value::Bool(a <= b),
                Add => Value::Number(a + b),
                Sub => Value::Number(a - b),
                Mul => Value::Number(a * b),
                Div => Value::Number(a / b),
            },

            (Value::Bytes(a), Value::Bytes(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            (Value::Bool(a), Value::Bool(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            (Value::Nil, Value::Nil) => match op {
                Equal => Value::Bool(true),
                NotEqual => Value::Bool(false),
                _ => Err(unsupported_operator())?,
            },

            _ => Err(unsupported_operator())?,
        })
    }
}

impl Evaluate for ast::UnaryExpr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {op, value} = self;
        let line = value.line();

        let value = value.eval(ctx)?;
        let value_type = value.typ();
        let unsupported_operator = || Diagnostic {
            line,
            message: format!("unsupported unary operator `{}` for type `{}`", op, value_type),
        };

        use ast::UnaryOp::*;
        Ok(match value {
            Value::Number(value) => match op {
                Not => Err(unsupported_operator())?,
                Neg => Value::Number(-value),
            },

            Value::Bytes(_) => match op {
                _ => Err(unsupported_operator())?,
            },

            Value::Bool(value) => match op {
                Not => Value::Bool(!value),
                Neg => Err(unsupported_operator())?,
            },

            Value::Nil => match op {
                _ => Err(unsupported_operator())?,
            },
        })
    }
}

impl Evaluate for ast::NumLit {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        Ok(Value::Number(self.value))
    }
}

impl Evaluate for ast::StrLit {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        Ok(Value::Bytes(self.value.iter().copied().collect()))
    }
}

impl Evaluate for ast::BoolLit {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        Ok(Value::Bool(self.value))
    }
}

impl Evaluate for ast::Ident {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        todo!()
    }
}

impl Evaluate for ast::Nil {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        Ok(Value::Nil)
    }
}
