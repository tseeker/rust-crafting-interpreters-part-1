use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{functions::Function, Callable, InterpreterState, Value};

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, Function>,
}

/// Classes are mostly used through references.
pub type ClassRef = Rc<RefCell<Class>>;

/// An instance of a Lox class
#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: RefCell<HashMap<String, Value>>,
}

/* -------------------- *
 * Class implementation *
 * -------------------- */

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(name: String, methods: HashMap<String, Function>) -> Self {
        Self { name, methods }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<class ")?;
        f.write_str(&self.name)?;
        f.write_str(">")
    }
}

impl Callable for ClassRef {
    fn arity(&self) -> usize {
        if let Some(init) = self.borrow().methods.get("init") {
            init.arity()
        } else {
            0
        }
    }

    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        let inst_value = Value::from(Instance::new(self.clone()));
        if let Some(init) = self.borrow().methods.get("init") {
            inst_value.with_instance(
                |instance| {
                    let bound_init = instance.bind_method(init, &inst_value);
                    bound_init.call(itpr_state, arguments)
                },
                || panic!("Instance is not an instance, wtf"),
            )
        } else {
            Ok(inst_value)
        }
    }
}

/* ----------------------- *
 * Instance implementation *
 * ----------------------- */

impl Instance {
    fn new(class: ClassRef) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::default()),
        }
    }

    pub(super) fn get(&self, this_value: &Value, name: &Token) -> SloxResult<Value> {
        if let Some(value) = self.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }
        if let Some(method) = self.class.borrow().methods.get(&name.lexeme) {
            let bound_method = self.bind_method(method, this_value);
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    pub(super) fn set(&self, name: &Token, value: Value) {
        self.fields.borrow_mut().insert(name.lexeme.clone(), value);
    }

    fn bind_method(&self, method: &Function, this_value: &Value) -> Function {
        let bm = method.copy_with_child_env();
        bm.env().set("this", this_value.clone());
        bm
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<Instance of {}>", self.class.borrow(),))
    }
}
