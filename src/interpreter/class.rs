use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::errors::SloxResult;

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

    fn call(&self, _itpr_state: &mut InterpreterState, _arguments: Vec<Value>) -> SloxResult<Value> {
        let instance = Instance::new(self.clone());
        Ok(Value::from(instance))
    }
}

/* ----------------------- *
 * Instance implementation *
 * ----------------------- */

impl Instance {
    fn new(class: ClassRef) -> Self {
        Self { class }
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "<Instance of {}>",
            self.class.borrow(),
        ))
    }
}
