use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::errors::SloxResult;

use super::{Callable, CallableRef, InterpreterState, Value};

/* ----------- *
 *   clock()   *
 * ----------- */

#[derive(Debug)]
struct Clock;

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _environment: &mut InterpreterState, _arguments: Vec<Value>) -> SloxResult<Value> {
        let now = SystemTime::now();
        let since_epoch = now
            .duration_since(UNIX_EPOCH)
            .expect("looks like it's 2038 already");
        Ok(Value::Number((since_epoch.as_millis() as f64) / 1000.0))
    }
}

impl ToString for Clock {
    fn to_string(&self) -> String {
        "<native function clock()>".to_owned()
    }
}

pub(super) fn clock() -> CallableRef {
    Rc::new(RefCell::new(Clock {}))
}
