use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{errors::InterpreterError, tokens::Token};

use super::{InterpreterResult, Value};

/// A mutable reference to an environment.
pub type EnvironmentRef = Rc<RefCell<Environment>>;

/// A variable.
pub type Variable = Option<Value>;

/// The execution environment.
#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    values: HashMap<String, Variable>,
}

impl Environment {
    /// Create an environment enclosed in another.
    pub fn create_child(parent: &EnvironmentRef) -> EnvironmentRef {
        Rc::new(RefCell::new(Self {
            enclosing: Some(parent.clone()),
            values: HashMap::default(),
        }))
    }

    /// Define a new variable.
    pub fn define(&mut self, name: &Token, value: Variable) -> Result<(), InterpreterError> {
        if self.values.contains_key(&name.lexeme as &str) {
            Err(InterpreterError::new(
                name,
                &format!("variables '{}' already defined in scope", name.lexeme),
            ))
        } else {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        }
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
            Some(None) => Err(InterpreterError::new(
                name,
                &format!("variable '{}' has not been initialized", name.lexeme),
            )),
            Some(Some(value)) => Ok(value.clone()),
        }
    }

    /// Assign a value to an existing variable.
    pub fn assign(&mut self, name: &Token, value: Value) -> InterpreterResult {
        if self.values.contains_key(&name.lexeme as &str) {
            self.values.insert(name.lexeme.clone(), Some(value));
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
