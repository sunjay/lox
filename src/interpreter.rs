mod value;
mod env;

pub use value::*;
pub use env::*;

use crate::ast;
use crate::diag::Diagnostic;

#[derive(Debug, Default)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn eval(&mut self, expr: ast::Program) -> anyhow::Result<Value> {
        expr.eval(self)
    }
}

pub trait Evaluate {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value>;
}

impl Evaluate for ast::Program {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {decls} = self;
        for decl in decls {
            decl.eval(ctx)?;
        }

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Decl {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        use ast::Decl::*;
        match self {
            VarDecl(var_decl) => var_decl.eval(ctx),
            Stmt(stmt) => stmt.eval(ctx),
        }
    }
}

impl Evaluate for ast::VarDecl {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {name, initializer} = self;

        let value = initializer.map(|expr| expr.eval(ctx)).unwrap_or(Ok(Value::Nil))?;
        ctx.env.insert(name.value.clone(), value);

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Stmt {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        use ast::Stmt::*;
        match self {
            Print(stmt) => stmt.eval(ctx),
            Expr(stmt) => stmt.eval(ctx),
            Block(stmt) => stmt.eval(ctx),
            If(stmt) => stmt.eval(ctx),
            While(stmt) => stmt.eval(ctx),
        }
    }
}

impl Evaluate for ast::PrintStmt {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let value = self.value.eval(ctx)?;
        println!("{}", value);

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Block {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {decls} = self;

        ctx.env.push_scope();
        for decl in decls {
            decl.eval(ctx)?;
        }
        ctx.env.pop_scope();

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Cond {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {cond, if_body, else_body} = self;

        if cond.eval(ctx)?.is_truthy() {
            if_body.eval(ctx)?;
        } else if let Some(else_body) = else_body {
            else_body.eval(ctx)?;
        }

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::WhileLoop {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {cond, body} = self;

        while cond.clone().eval(ctx)?.is_truthy() {
            body.clone().eval(ctx)?;
        }

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Expr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        use ast::Expr::*;
        match self {
            LogicalAnd(expr) => expr.eval(ctx),
            LogicalOr(expr) => expr.eval(ctx),
            Assign(expr) => expr.eval(ctx),
            Binary(expr) => expr.eval(ctx),
            Unary(expr) => expr.eval(ctx),
            Call(expr) => expr.eval(ctx),
            Number(expr) => expr.eval(ctx),
            String(expr) => expr.eval(ctx),
            Bool(expr) => expr.eval(ctx),
            Ident(expr) => expr.eval(ctx),
            Nil(expr) => expr.eval(ctx),
        }
    }
}

impl Evaluate for ast::LogicalAnd {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {left, right} = self;
        let left_value = left.eval(ctx)?;

        // short-circuit evaluation
        if left_value.is_truthy() {
            let right_value = right.eval(ctx)?;
            Ok(right_value)
        } else {
            Ok(left_value)
        }
    }
}

impl Evaluate for ast::LogicalOr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {left, right} = self;
        let left_value = left.eval(ctx)?;

        // short-circuit evaluation
        if left_value.is_truthy() {
            Ok(left_value)
        } else {
            let right_value = right.eval(ctx)?;
            Ok(right_value)
        }
    }
}

impl Evaluate for ast::Assign {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        let Self {lvalue, rhs} = self;
        let line = lvalue.line();

        let value = rhs.eval(ctx)?;

        use ast::LValue::*;
        match lvalue {
            Ident(name) => {
                let entry = ctx.env.get_mut(&name.value).ok_or_else(|| Diagnostic {
                    line,
                    message: format!("Undefined variable `{}`", name.value),
                })?;

                *entry = value.clone();

                Ok(value)
            },
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

impl Evaluate for ast::CallExpr {
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        todo!()
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
    fn eval(self, ctx: &mut Interpreter) -> anyhow::Result<Value> {
        ctx.env.get(&self.value).cloned().ok_or_else(|| Diagnostic {
            line: self.line,
            message: format!("Undefined variable `{}`", self.value),
        }.into())
    }
}

impl Evaluate for ast::Nil {
    fn eval(self, _ctx: &mut Interpreter) -> anyhow::Result<Value> {
        Ok(Value::Nil)
    }
}
