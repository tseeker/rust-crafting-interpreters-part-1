use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    interpreter::EnvironmentRef,
    tokens::{Token, TokenType},
};

use super::{functions::Function, Callable, Environment, InterpreterState, Value};

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, Function>,
}

/// Classes are mostly used through references.
pub type ClassRef = Rc<RefCell<Class>>;

/// An instance of a Lox class
#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: HashMap<String, Value>,
}

/// A method bound to an instance.
#[derive(Debug, Clone)]
pub struct BoundMethod {
    instance: Value,
    method: String,
}

/* -------------------- *
 * Class implementation *
 * -------------------- */

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(name: String, methods: HashMap<String, Function>) -> Self {
        Self { name, methods }
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

    pub(super) fn get(&self, this_value: &Value, name: &Token) -> SloxResult<Value> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(value.clone());
        }
        if let Some(method) = self.class.borrow().methods.get(&name.lexeme) {
            let bound_method = self.bind_method(method, this_value);
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    pub(super) fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }

    fn bind_method(&self, method: &Function, this_value: &Value) -> Function {
        let bm = method.copy_with_child_env();
        bm.env().set("this", this_value.clone());
        bm
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<Instance of {}>", self.class.borrow(),))
    }
}
