use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Number,
    Bytes,
    Bool,
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
        match self {
            Number => write!(f, "number"),
            Bytes => write!(f, "string"),
            Bool => write!(f, "bool"),
            Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bytes(Vec<u8>),
    Bool(bool),
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
            Nil => Type::Nil,
        }
    }
}