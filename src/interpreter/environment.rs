use std::collections::HashMap;

use crate::{errors::InterpreterError, tokens::Token};

use super::{InterpreterResult, Value};

/// The execution environment.
#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    /// Define a new variable.
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    /// Get the value of a variable.
    pub fn get(&self, name: &Token) -> InterpreterResult {
        match self.values.get(&name.lexeme as &str) {
            None => Err(InterpreterError::new(
                name,
                &format!("undefined variable '{}'", name.lexeme),
            )),
            Some(value) => Ok(value.clone()),
        }
    }

    /// Assign a value to an existing variable.
    pub fn assign(&mut self, name: &Token, value: &Value) -> Result<(), InterpreterError> {
        if self.values.contains_key(&name.lexeme as &str) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else {
            Err(InterpreterError::new(
                name,
                &format!("undefined variable '{}'", name.lexeme),
            ))
        }
    }
}
