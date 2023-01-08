use std::fmt::Debug;

use crate::errors::SloxResult;

use super::{InterpreterState, Value};

/// A callable is some object that supports being called.
pub trait Callable: Debug {
    /// Return the amount of arguments supported by the callable.
    fn arity(&self) -> usize;

    /// Run the callable in the execution environment with the specified
    /// arguments.
    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value>;
}
