use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::errors::InterpreterError;

use super::{EnvironmentRef, Value};

/// A callable is some object that supports being called.
pub trait Callable: Debug + ToString {
    /// Return the amount of arguments supported by the callable.
    fn arity(&self) -> usize;

    /// Run the callable in the execution environment with the specified
    /// arguments.
    fn call(
        &self,
        environment: &EnvironmentRef,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError>;
}

/// A reference to a callable.
pub type CallableRef = Rc<RefCell<dyn Callable>>;
