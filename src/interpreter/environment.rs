use std::collections::HashMap;

use crate::{errors::InterpreterError, tokens::Token};

use super::Value;

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
    pub fn get(&self, name: &Token) -> Result<Value, InterpreterError> {
        match self.values.get(&name.lexeme as &str) {
            None => Err(InterpreterError::new(
                name,
                &format!("undefined variable '{}'", name.lexeme),
            )),
            Some(value) => Ok(value.clone()),
        }
    }
}
