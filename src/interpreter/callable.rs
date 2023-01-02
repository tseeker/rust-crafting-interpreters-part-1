use std::fmt::Debug;

use crate::errors::InterpreterError;

use super::{Environment, Value};

/// A callable is some object that supports being called.
pub trait Callable: Debug + ToString {
    /// Return the amount of arguments supported by the callable.
    fn arity(&self) -> usize;

    /// Run the callable in the execution environment with the specified
    /// arguments.
    fn call(
        &self,
        environment: &mut Environment,
        arguments: &Vec<Value>,
    ) -> Result<Value, InterpreterError>;
}
