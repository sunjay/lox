mod value;
mod env;

pub use value::*;
pub use env::*;

use crate::{ast, prelude};
use crate::diag::Diagnostic;

pub type EvalResult = Result<Value, ControlFlow>;

#[derive(Debug)]
pub enum ControlFlow {
    Return {
        line: usize,
        value: Value,
    },
    RuntimeError(Diagnostic),
}

impl From<Diagnostic> for ControlFlow {
    fn from(diag: Diagnostic) -> Self {
        ControlFlow::RuntimeError(diag)
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    pub(crate) env: Environment,
}

impl Interpreter {
    pub fn with_prelude() -> Self {
        let mut ctx = Self::default();
        prelude::populate_prelude(&mut ctx);
        ctx
    }

    pub fn eval(&mut self, expr: ast::Program) -> anyhow::Result<Value> {
        use ControlFlow::*;
        Ok(expr.eval(self).map_err(|err| match err {
            Return {line, ..} => Diagnostic {
                line,
                message: "return is not allowed outside function".to_string(),
            },

            RuntimeError(diag) => diag,
        })?)
    }
}

pub trait Evaluate {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult;
}

impl Evaluate for ast::Program {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {decls} = self;
        for decl in decls {
            decl.eval(ctx)?;
        }

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Decl {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        use ast::Decl::*;
        match self {
            VarDecl(decl) => decl.eval(ctx),
            FuncDecl(decl) => decl.eval(ctx),
            ClassDecl(decl) => decl.eval(ctx),
            Stmt(stmt) => stmt.eval(ctx),
        }
    }
}

impl Evaluate for ast::VarDecl {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {name, initializer} = self;

        let value = initializer.map(|expr| expr.eval(ctx)).unwrap_or(Ok(Value::Nil))?;
        ctx.env.insert(name.value.clone(), value);

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::FuncDecl {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let name = self.name.value.clone();
        ctx.env.insert(name, Value::Func(self.into()));
        Ok(Value::Nil)
    }
}

impl Evaluate for ast::ClassDecl {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let name = self.name.value.clone();
        ctx.env.insert(name, Value::Class(self.into()));
        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Stmt {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        use ast::Stmt::*;
        match self {
            Print(stmt) => stmt.eval(ctx),
            Expr(stmt) => stmt.eval(ctx),
            Block(stmt) => stmt.eval(ctx),
            If(stmt) => stmt.eval(ctx),
            While(stmt) => stmt.eval(ctx),
            Return(stmt) => stmt.eval(ctx),
        }
    }
}

impl Evaluate for ast::PrintStmt {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let value = self.value.eval(ctx)?;
        println!("{}", value);

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Block {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {decls} = self;

        let mut res = Ok(Value::Nil);

        ctx.env.push_scope();
        for decl in decls {
            if let Err(err) = decl.eval(ctx) {
                res = Err(err);
                break;
            }
        }
        ctx.env.pop_scope();

        res
    }
}

impl Evaluate for ast::Cond {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
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
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {cond, body} = self;

        while cond.clone().eval(ctx)?.is_truthy() {
            body.clone().eval(ctx)?;
        }

        Ok(Value::Nil)
    }
}

impl Evaluate for ast::Return {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {return_line, expr} = self;
        let value = expr.map(|expr| expr.eval(ctx)).unwrap_or(Ok(Value::Nil))?;

        Err(ControlFlow::Return {
            line: return_line,
            value,
        })
    }
}

impl Evaluate for ast::Expr {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        use ast::Expr::*;
        match self {
            LogicalAnd(expr) => expr.eval(ctx),
            LogicalOr(expr) => expr.eval(ctx),
            Assign(expr) => expr.eval(ctx),
            Binary(expr) => expr.eval(ctx),
            Unary(expr) => expr.eval(ctx),
            Call(expr) => expr.eval(ctx),
            FieldAccess(expr) => expr.eval(ctx),
            Number(expr) => expr.eval(ctx),
            String(expr) => expr.eval(ctx),
            Bool(expr) => expr.eval(ctx),
            Ident(expr) => expr.eval(ctx),
            Nil(expr) => expr.eval(ctx),
        }
    }
}

impl Evaluate for ast::LogicalAnd {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
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
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
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
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {lvalue, rhs} = self;
        let line = lvalue.line();

        use ast::LValue::*;
        match lvalue {
            Ident(name) => {
                let value = rhs.eval(ctx)?;

                let entry = ctx.env.get_mut(&name.value).ok_or_else(|| Diagnostic {
                    line,
                    message: format!("Undefined variable `{}`", name.value),
                })?;

                *entry = value.clone();

                Ok(value)
            },

            Field(ast::FieldAccess {expr, field}) => {
                match expr.eval(ctx)? {
                    Value::Instance(instance) => {
                        let value = rhs.eval(ctx)?;

                        instance.set(field.value.clone(), value.clone());

                        Ok(value)
                    },

                    _ => Err(Diagnostic {
                        line,
                        message: "Only instances have properties".to_string(),
                    })?,
                }
            },
        }
    }
}

impl Evaluate for ast::BinaryExpr {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
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
                Add => Value::Bytes(a.iter().copied().chain(b.iter().copied()).collect()),
                _ => Err(unsupported_operator())?,
            },

            (Value::Bool(a), Value::Bool(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            (Value::NativeFunc(a), Value::NativeFunc(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            (Value::Func(a), Value::Func(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            (Value::Class(a), Value::Class(b)) => match op {
                Equal => Value::Bool(a == b),
                NotEqual => Value::Bool(a != b),
                _ => Err(unsupported_operator())?,
            },

            //TODO: Instance could overload operators?
            (Value::Instance(a), Value::Instance(b)) => match op {
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
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
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

            Value::NativeFunc(_) => match op {
                _ => Err(unsupported_operator())?,
            },

            Value::Func(_) => match op {
                _ => Err(unsupported_operator())?,
            },

            Value::Class(_) => match op {
                _ => Err(unsupported_operator())?,
            },

            Value::Instance(_) => match op { //TODO: Instance could support operators maybe?
                _ => Err(unsupported_operator())?,
            },

            Value::Nil => match op {
                _ => Err(unsupported_operator())?,
            },
        })
    }
}

impl Evaluate for ast::CallExpr {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {callee, args} = self;

        let callee_line = callee.line();
        let callee = callee.eval(ctx)?;
        let args = args.into_iter()
            .map(|arg| arg.eval(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        let callable = callee.into_callable().ok_or_else(|| Diagnostic {
            line: callee_line,
            message: format!("Can only call functions and classes"),
        })?;

        if args.len() != callable.arity() {
            Err(Diagnostic {
                line: callee_line,
                message: format!("Expected {} arguments but got {}", callable.arity(), args.len()),
            })?
        }

        match callable.call(ctx, args) {
            Ok(value) |
            Err(ControlFlow::Return {value, ..}) => Ok(value),

            Err(err) => Err(err),
        }
    }
}

impl Evaluate for ast::FieldAccess {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        let Self {expr, field} = self;

        let line = expr.line();
        let value = expr.eval(ctx)?;
        match value {
            Value::Instance(instance) => {
                Ok(instance.get(&field.value).ok_or_else(|| Diagnostic {
                    line: field.line,
                    message: format!("Undefined property `{}`", field.value),
                })?)
            },

            _ => Err(Diagnostic {
                line,
                message: "Only instances have properties".to_string(),
            })?,
        }
    }
}

impl Evaluate for ast::NumLit {
    fn eval(self, _ctx: &mut Interpreter) -> EvalResult {
        Ok(Value::Number(self.value))
    }
}

impl Evaluate for ast::StrLit {
    fn eval(self, _ctx: &mut Interpreter) -> EvalResult {
        Ok(Value::Bytes(self.value))
    }
}

impl Evaluate for ast::BoolLit {
    fn eval(self, _ctx: &mut Interpreter) -> EvalResult {
        Ok(Value::Bool(self.value))
    }
}

impl Evaluate for ast::Ident {
    fn eval(self, ctx: &mut Interpreter) -> EvalResult {
        ctx.env.get(&self.value).cloned().ok_or_else(|| Diagnostic {
            line: self.line,
            message: format!("Undefined variable `{}`", self.value),
        }.into())
    }
}

impl Evaluate for ast::Nil {
    fn eval(self, _ctx: &mut Interpreter) -> EvalResult {
        Ok(Value::Nil)
    }
}
