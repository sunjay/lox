use std::fmt;
use std::sync::Arc;
use std::collections::HashMap;

use parking_lot::Mutex;

use crate::ast;

use super::{Interpreter, Evaluate, EvalResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Number,
    Bytes,
    Bool,
    Func,
    Class,
    Instance,
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
            Class => write!(f, "class"),
            Instance => write!(f, "instance"),
            Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bytes(Arc<[u8]>),
    Bool(bool),
    NativeFunc(SharedNativeFunc),
    Func(SharedFunc),
    Class(SharedClass),
    Instance(SharedInstance),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Number(value) => write!(f, "{}", value),
            Bytes(value) => {
                // Write out bytes as ASCII
                for &byte in &**value {
                    write!(f, "{}", byte as char)?;
                }
                Ok(())
            },
            Bool(value) => write!(f, "{}", value),
            NativeFunc(_) => write!(f, "<native func>"),
            Func(func) => write!(f, "{}", func),
            Class(class) => write!(f, "{}", class),
            Instance(instance) => write!(f, "{}", instance),
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
            NativeFunc(_) |
            Func(_) => Type::Func,
            Class(_) => Type::Class,
            Instance(_) => Type::Instance,
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
            Instance(_) | //TODO: instances could be callable maybe?
            Nil => None,

            NativeFunc(func) => Some(Callable::NativeFunc(func)),
            Func(func) => Some(Callable::Func(func)),
            Class(class) => Some(Callable::Class(class)),
        }
    }
}

pub trait NativeFunc {
    fn arity(&self) -> usize;

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> EvalResult;
}

// For functions with greater than zero arity, implement NativeFunc manually on a new type
impl<F> NativeFunc for F
    where F: Fn(&mut Interpreter) -> EvalResult,
{
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> EvalResult {
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

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> EvalResult {
        self.0.call(ctx, args)
    }
}

impl fmt::Debug for SharedNativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for SharedNativeFunc {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug)]
pub struct SharedFunc(Arc<ast::FuncDecl>);

impl From<ast::FuncDecl> for SharedFunc {
    fn from(value: ast::FuncDecl) -> Self {
        SharedFunc(Arc::new(value))
    }
}

impl SharedFunc {
    fn arity(&self) -> usize {
        self.0.params.len()
    }

    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> EvalResult {
        ctx.env.push_scope_global();

        let ast::FuncDecl {name: _, params, body} = &*self.0;
        debug_assert_eq!(params.len(), args.len());
        for (param, arg) in params.iter().zip(args) {
            ctx.env.insert(param.value.clone(), arg);
        }

        let value = body.clone().eval(ctx);

        ctx.env.pop_scope();

        value
    }
}

impl fmt::Display for SharedFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fun {}>", self.0.name.value)
    }
}

impl PartialEq for SharedFunc {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug)]
pub struct SharedClass(Arc<ast::ClassDecl>);

impl From<ast::ClassDecl> for SharedClass {
    fn from(value: ast::ClassDecl) -> Self {
        SharedClass(Arc::new(value))
    }
}

impl SharedClass {
    fn arity(&self) -> usize {
        0 //TODO: user-defined constructors
    }

    fn call(&self, _ctx: &mut Interpreter, _args: Vec<Value>) -> EvalResult {
        let instance = Instance::from(self.0.clone());

        Ok(Value::Instance(instance.into()))
    }
}

impl fmt::Display for SharedClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.0.name.value)
    }
}

impl PartialEq for SharedClass {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    NativeFunc(SharedNativeFunc),
    Func(SharedFunc),
    Class(SharedClass),
}

impl Callable {
    pub fn arity(&self) -> usize {
        use Callable::*;
        match self {
            NativeFunc(func) => func.arity(),
            Func(func) => func.arity(),
            Class(class) => class.arity(),
        }
    }

    pub fn call(self, ctx: &mut Interpreter, args: Vec<Value>) -> EvalResult {
        use Callable::*;
        match self {
            NativeFunc(func) => func.call(ctx, args),
            Func(func) => func.call(ctx, args),
            Class(class) => class.call(ctx, args),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    class: Arc<ast::ClassDecl>,
    fields: HashMap<Arc<str>, Value>,
}

impl From<Arc<ast::ClassDecl>> for Instance {
    fn from(class: Arc<ast::ClassDecl>) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }
}

impl Instance {
    pub fn get(&self, field: &str) -> Option<&Value> {
        self.fields.get(field)
    }
}

#[derive(Clone, Debug)]
pub struct SharedInstance(Arc<Mutex<Instance>>);

impl From<Instance> for SharedInstance {
    fn from(value: Instance) -> Self {
        SharedInstance(Arc::new(Mutex::new(value)))
    }
}

impl SharedInstance {
    pub fn get(&self, field: &str) -> Option<Value> {
        self.0.lock().get(field).cloned()
    }
}

impl fmt::Display for SharedInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<instance {}>", self.0.lock().class.name.value)
    }
}

impl PartialEq for SharedInstance {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
