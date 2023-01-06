use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::errors::SloxResult;

use super::{InterpreterState, Value};

/// A callable is some object that supports being called.
pub trait Callable: Debug + ToString {
    /// Return the amount of arguments supported by the callable.
    fn arity(&self) -> usize;

    /// Run the callable in the execution environment with the specified
    /// arguments.
    fn call(&self, environment: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value>;
}

/// A reference to a callable.
pub type CallableRef = Rc<RefCell<dyn Callable>>;
