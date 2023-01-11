use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{
    native_fn::{self, NativeFunction},
    Value,
};

/// A mutable reference to an environment.
pub(super) type EnvironmentRef = Rc<RefCell<Environment>>;

/// A variable.
pub(super) type Variable = Option<Value>;

/// The execution environment.
#[derive(Debug)]
pub(super) struct Environment {
    pub(super) enclosing: Option<EnvironmentRef>,
    values: HashMap<String, Variable>,
}

impl Default for Environment {
    /// Create the default global environment. This includes native functions.
    fn default() -> Self {
        let mut env = Self {
            enclosing: None,
            values: HashMap::new(),
        };
        env.add_default_fun(native_fn::clock());
        env
    }
}

impl Environment {
    /// Create an environment enclosed in another.
    pub fn create_child(parent: &EnvironmentRef) -> EnvironmentRef {
        Rc::new(RefCell::new(Self {
            enclosing: Some(parent.clone()),
            values: HashMap::default(),
        }))
    }

    /// Add a default function to the environment.
    fn add_default_fun(&mut self, fun: NativeFunction) {
        let name = fun.name().to_owned();
        let value = Some(Value::from(fun));
        self.values.insert(name, value);
    }

    /// Define a new variable.
    pub fn define(&mut self, name: &Token, value: Variable) -> SloxResult<()> {
        if self.values.contains_key(&name.lexeme as &str) {
            Err(SloxError::with_token(
                ErrorKind::Runtime,
                name,
                format!("variable '{}' already defined in scope", name.lexeme),
            ))
        } else {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        }
    }

    /// Get the value of a variable.
    pub fn get(&self, name: &Token) -> SloxResult<Value> {
        match self.values.get(&name.lexeme as &str) {
            None => match &self.enclosing {
                None => Err(SloxError::with_token(
                    ErrorKind::Runtime,
                    name,
                    format!("undefined variable '{}'", name.lexeme),
                )),
                Some(parent) => parent.borrow().get(name),
            },
            Some(None) => Err(SloxError::with_token(
                ErrorKind::Runtime,
                name,
                format!("variable '{}' has not been initialized", name.lexeme),
            )),
            Some(Some(value)) => Ok(value.clone()),
        }
    }

    /// Access a variable at a specified distance in a parent environment.
    pub fn get_at(&self, distance: usize, name: &Token) -> SloxResult<Value> {
        if distance == 0 {
            self.get(name)
        } else {
            self.ancestor(distance).borrow().get(name)
        }
    }

    /// Assign a value to an existing variable.
    pub fn assign(&mut self, name: &Token, value: Value) -> SloxResult<()> {
        if self.values.contains_key(&name.lexeme as &str) {
            self.values.insert(name.lexeme.clone(), Some(value));
            Ok(())
        } else {
            match &mut self.enclosing {
                None => Err(SloxError::with_token(
                    ErrorKind::Runtime,
                    name,
                    format!("undefined variable '{}'", name.lexeme),
                )),
                Some(parent) => parent.borrow_mut().assign(name, value),
            }
        }
    }

    /// Set a variable at a specified distance in a parent environment.
    pub fn assign_at(&mut self, distance: usize, name: &Token, value: Value) -> SloxResult<()> {
        if distance == 0 {
            self.assign(name, value)
        } else {
            self.ancestor(distance).borrow_mut().assign(name, value)
        }
    }

    /// Set a variable in an environment, directly, without any checks.
    pub fn set(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_owned(), Some(value));
    }

    /// Read a variable from an environment directly. Panics if the symbol
    /// does not exist.
    pub fn read(&self, name: &str) -> Value {
        match self.values.get(name) {
            Some(Some(v)) => v.clone(),
            _ => panic!("Symbol {name} does not exist ({:?})", self.values.keys()),
        }
    }

    /// Read an ancestor from the chain of enclosing environments.
    fn ancestor(&self, distance: usize) -> EnvironmentRef {
        let mut ancestor = self.enclosing.clone().expect("ancestor() called at root");
        for _ in 1..distance {
            let ap = ancestor
                .borrow()
                .enclosing
                .clone()
                .expect("ancestor() called with too high a distance");
            ancestor = ap;
        }
        ancestor
    }
}
