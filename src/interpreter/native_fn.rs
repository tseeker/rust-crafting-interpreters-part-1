use std::{
    fmt::{Debug, Display},
    time::{SystemTime, UNIX_EPOCH},
};

use crate::errors::SloxResult;

use super::{Callable, InterpreterState, Value};

/* ----------------------- *
 * Native function support *
 * ----------------------- */

/// A function pointer to the implementation of some native function.
type NativeFunctionHandler = fn(&mut InterpreterState, Vec<Value>) -> SloxResult<Value>;

/// A native function.
#[derive(Clone)]
pub struct NativeFunction {
    /// Name of the function.
    name: String,
    /// Arity of the function.
    arity: usize,
    /// Rust function that actually implements the function.
    handler: NativeFunctionHandler,
}

impl NativeFunction {
    /// Initialize a native function's record.
    fn new(name: &str, arity: usize, handler: NativeFunctionHandler) -> Self {
        Self {
            name: name.to_owned(),
            arity,
            handler,
        }
    }

    /// Access the native function's name
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native function ")?;
        f.write_str(&self.name)?;
        f.write_str(">")
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "NativeFunction {{ name: {:?}, arity: {} }}",
            self.name, self.arity
        ))
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        (self.handler)(itpr_state, arguments)
    }
}

/* ----------- *
 *   clock()   *
 * ----------- */

fn _clock_implementation(
    _environment: &mut InterpreterState,
    _arguments: Vec<Value>,
) -> SloxResult<Value> {
    let now = SystemTime::now();
    let since_epoch = now
        .duration_since(UNIX_EPOCH)
        .expect("looks like it's 2038 already");
    Ok(Value::Number((since_epoch.as_millis() as f64) / 1000.0))
}

pub(super) fn clock() -> NativeFunction {
    NativeFunction::new("clock", 0, _clock_implementation)
}
