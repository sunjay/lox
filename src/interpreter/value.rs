use std::fmt;
use std::sync::Arc;

use super::Interpreter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Number,
    Bytes,
    Bool,
    Func,
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
        match self {
            Number => write!(f, "number"),
            Bytes => write!(f, "string"),
            Bool => write!(f, "bool"),
            Func => write!(f, "function"),
            Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bytes(Vec<u8>),
    Bool(bool),
    NativeFunc(SharedNativeFunc),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Number(value) => write!(f, "{}", value),
            Bytes(value) => {
                // Write out bytes as ASCII
                for &byte in value {
                    write!(f, "{}", byte as char)?;
                }
                Ok(())
            },
            Bool(value) => write!(f, "{}", value),
            NativeFunc(_) => write!(f, "<native func>"),
            Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    pub fn typ(&self) -> Type {
        use Value::*;
        match self {
            Number(_) => Type::Number,
            Bytes(_) => Type::Bytes,
            Bool(_) => Type::Bool,
            NativeFunc(_) => Type::Func,
            Nil => Type::Nil,
        }
    }

    pub fn is_truthy(&self) -> bool {
        use Value::*;
        match self {
            Bool(false) |
            Nil => false,

            _ => true,
        }
    }

    pub fn into_callable(self) -> Option<Callable> {
        use Value::*;
        match self {
            Number(_) |
            Bytes(_) |
            Bool(_) |
            Nil => None,

            NativeFunc(func) => Some(Callable::NativeFunc(func)),
        }
    }
}

pub trait NativeFunc {
    fn arity(&self) -> usize;

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> anyhow::Result<Value>;
}

// For functions with greater than zero arity, implement NativeFunc manually on a new type
impl<F> NativeFunc for F
    where F: Fn(&mut Interpreter) -> anyhow::Result<Value>,
{
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> anyhow::Result<Value> {
        debug_assert_eq!(args.len(), 0);
        self(ctx)
    }
}

#[derive(Clone)]
pub struct SharedNativeFunc(Arc<dyn NativeFunc>);

impl<T: NativeFunc + 'static> From<T> for SharedNativeFunc {
    fn from(value: T) -> Self {
        SharedNativeFunc(Arc::new(value))
    }
}

impl SharedNativeFunc {
    fn arity(&self) -> usize {
        self.0.arity()
    }

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> anyhow::Result<Value> {
        self.0.call(ctx, args)
    }
}

impl fmt::Debug for SharedNativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native func>")
    }
}

impl PartialEq for SharedNativeFunc {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    NativeFunc(SharedNativeFunc),
}

impl Callable {
    pub fn arity(&self) -> usize {
        use Callable::*;
        match self {
            NativeFunc(func) => func.arity(),
        }
    }

    pub fn call(self, ctx: &mut Interpreter, args: Vec<Value>) -> anyhow::Result<Value> {
        use Callable::*;
        match self {
            NativeFunc(func) => func.call(ctx, args),
        }
    }
}
