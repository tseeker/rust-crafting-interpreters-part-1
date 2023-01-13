use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{functions::Function, Callable, InterpreterState, Value};

/// This trait represents an object on which getters and setters
/// must be supported.
pub trait PropertyCarrier {
    fn get(&self, name: &Token) -> SloxResult<Value>;
    fn set(&self, name: &Token, value: Value);
}

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, Function>,
    static_methods: HashMap<String, Function>,
    fields: RefCell<HashMap<String, Value>>,
}

/// Classes are mostly used through references.
pub type ClassRef = Rc<RefCell<Class>>;

/// An instance of a Lox class
#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: RefCell<HashMap<String, Value>>,
}

/// Helper type used to refer to instances.
pub type InstanceRef = Rc<RefCell<Instance>>;

/* -------------------- *
 * Class implementation *
 * -------------------- */

fn bind_method(method: &Function, this_value: Value) -> Function {
    let bm = method.copy_with_child_env();
    bm.env().set("this", this_value);
    bm
}

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(
        name: String,
        methods: HashMap<String, Function>,
        static_methods: HashMap<String, Function>,
    ) -> Self {
        Self {
            name,
            methods,
            static_methods,
            fields: RefCell::new(HashMap::default()),
        }
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
                |_| {
                    let bound_init = bind_method(init, inst_value.clone());
                    bound_init.call(itpr_state, arguments)
                },
                || panic!("Instance is not an instance, wtf"),
            )
        } else {
            Ok(inst_value)
        }
    }
}

impl PropertyCarrier for ClassRef {
    fn get(&self, name: &Token) -> SloxResult<Value> {
        let class = self.borrow();
        if let Some(value) = class.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }
        /*
        if let Some(method) = class.methods.get(&name.lexeme) {
            let bound_method = bind_method(method, Value::from(self.clone()));
            return Ok(Value::from(bound_method));
        }
        */

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    fn set(&self, name: &Token, value: Value) {
        let class = self.borrow();
        class.fields.borrow_mut().insert(name.lexeme.clone(), value);
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
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<Instance of {}>", self.class.borrow(),))
    }
}

impl PropertyCarrier for InstanceRef {
    fn get(&self, name: &Token) -> SloxResult<Value> {
        let instance = self.borrow();
        if let Some(value) = instance.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }
        if let Some(method) = instance.class.borrow().methods.get(&name.lexeme) {
            let bound_method = bind_method(method, Value::from(self.clone()));
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    fn set(&self, name: &Token, value: Value) {
        let instance = self.borrow();
        instance
            .fields
            .borrow_mut()
            .insert(name.lexeme.clone(), value);
    }
}
