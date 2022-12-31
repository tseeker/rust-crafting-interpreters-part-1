use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{errors::InterpreterError, tokens::Token};

use super::{InterpreterResult, Value};

/// A mutable reference to an environment.
pub type EnvironmentRef = Rc<RefCell<Environment>>;

/// The execution environment.
#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    values: HashMap<String, Value>,
}

impl Environment {
    /// Create an environment enclosed in another.
    pub fn create_child(parent: &Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            enclosing: Some(parent.clone()),
            values: HashMap::default(),
        }))
    }

    /// Define a new variable.
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    /// Get the value of a variable.
    pub fn get(&self, name: &Token) -> InterpreterResult {
        match self.values.get(&name.lexeme as &str) {
            None => match &self.enclosing {
                None => Err(InterpreterError::new(
                    name,
                    &format!("undefined variable '{}'", name.lexeme),
                )),
                Some(parent) => parent.borrow().get(name),
            },
            Some(value) => Ok(value.clone()),
        }
    }

    /// Assign a value to an existing variable.
    pub fn assign(&mut self, name: &Token, value: Value) -> InterpreterResult {
        if self.values.contains_key(&name.lexeme as &str) {
            self.values.insert(name.lexeme.clone(), value);
            Ok(Value::Nil)
        } else {
            match &mut self.enclosing {
                None => Err(InterpreterError::new(
                    name,
                    &format!("undefined variable '{}'", name.lexeme),
                )),
                Some(parent) => parent.borrow_mut().assign(name, value),
            }
        }
    }
}
