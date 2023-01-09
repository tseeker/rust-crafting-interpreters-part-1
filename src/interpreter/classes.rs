use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{Callable, InterpreterState, Value};

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
}

/// Classes are mostly used through references.
pub type ClassRef = Rc<RefCell<Class>>;

/// An instance of a Lox class
#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: HashMap<String, Value>,
}

/* -------------------- *
 * Class implementation *
 * -------------------- */

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<class ")?;
        f.write_str(&self.name)?;
        f.write_str(">")
    }
}

impl Callable for ClassRef {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _itpr_state: &mut InterpreterState,
        _arguments: Vec<Value>,
    ) -> SloxResult<Value> {
        let instance = Instance::new(self.clone());
        Ok(Value::from(instance))
    }
}

/* ----------------------- *
 * Instance implementation *
 * ----------------------- */

impl Instance {
    fn new(class: ClassRef) -> Self {
        Self {
            class,
            fields: HashMap::default(),
        }
    }

    pub(super) fn get(&self, name: &Token) -> SloxResult<Value> {
        match self.fields.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(SloxError::with_token(
                ErrorKind::Runtime,
                name,
                "undefined property".to_owned(),
            )),
        }
    }

    pub(super) fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<Instance of {}>", self.class.borrow(),))
    }
}
