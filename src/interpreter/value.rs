use std::{cell::RefCell, rc::Rc};

use super::Callable;

/// A value being handled by the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
    Callable(Rc<RefCell<dyn Callable>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    /// Check whether a value is truthy or not.
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(b) => *b,
            _ => true,
        }
    }
}
