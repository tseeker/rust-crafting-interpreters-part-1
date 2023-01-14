use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use lazy_static::lazy_static;

use crate::{
    ast::ClassMemberKind,
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{functions::Function, Callable, InterpreterState, Value};

/// This trait represents an object on which getters and setters
/// must be supported.
pub trait PropertyCarrier {
    fn get(&self, itpr_state: &mut InterpreterState, name: &Token) -> SloxResult<Value>;
    fn set(&self, itpr_state: &mut InterpreterState, name: &Token, value: Value);
}

/// The key for the table of class members.
pub type ClassMemberKey = (ClassMemberKind, bool, String);

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    members: HashMap<ClassMemberKey, Function>,
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

lazy_static! {
    static ref INIT_METHOD_KEY: ClassMemberKey =
        (ClassMemberKind::Method, false, String::from("init"));
}

fn bind_method(method: &Function, this_value: Value) -> Function {
    let bm = method.copy_with_child_env();
    bm.env().set("this", this_value);
    bm
}

impl Class {
    /// Create a new class, specifying its name.
    pub fn new(name: String, members: HashMap<ClassMemberKey, Function>) -> Self {
        Self {
            name,
            members,
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
        if let Some(init) = self.borrow().members.get(&INIT_METHOD_KEY) {
            init.arity()
        } else {
            0
        }
    }

    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        let inst_value = Value::from(Instance::new(self.clone()));
        if let Some(init) = self.borrow().members.get(&INIT_METHOD_KEY) {
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
    fn get(&self, itpr_state: &mut InterpreterState, name: &Token) -> SloxResult<Value> {
        let class = self.borrow();

        // Check for a property getter and execute it if found.
        let mut mb_key = (ClassMemberKind::Getter, true, name.lexeme.clone());
        if let Some(getter) = class.members.get(&mb_key) {
            let bound_method = bind_method(getter, Value::from(self.clone()));
            return bound_method.call(itpr_state, vec![]);
        }

        // Check for an actual field.
        if let Some(value) = class.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }

        // Check for a method.
        mb_key.0 = ClassMemberKind::Method;
        if let Some(method) = class.members.get(&mb_key) {
            let bound_method = bind_method(method, Value::from(self.clone()));
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    fn set(&self, itpr_state: &mut InterpreterState, name: &Token, value: Value) {
        let class = self.borrow();

        // Check for a property setter.
        let mb_key = (ClassMemberKind::Setter, false, name.lexeme.clone());
        if let Some(setter) = class.members.get(&mb_key) {
            // Bind and execute the property setter
            let bound_method = bind_method(setter, Value::from(self.clone()));
            bound_method
                .call(itpr_state, vec![value])
                .expect("failed to execute setter");
            return;
        }

        // Set the property directly
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
    fn get(&self, itpr_state: &mut InterpreterState, name: &Token) -> SloxResult<Value> {
        let instance = self.borrow();

        // Check for a property getter and execute it if found.
        let mut mb_key = (ClassMemberKind::Getter, false, name.lexeme.clone());
        if let Some(getter) = instance.class.borrow().members.get(&mb_key) {
            let bound_method = bind_method(getter, Value::from(self.clone()));
            return bound_method.call(itpr_state, vec![]);
        }

        // Check for an actual field.
        if let Some(value) = instance.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }

        // Check for a method.
        mb_key.0 = ClassMemberKind::Method;
        if let Some(method) = instance.class.borrow().members.get(&mb_key) {
            let bound_method = bind_method(method, Value::from(self.clone()));
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    fn set(&self, itpr_state: &mut InterpreterState, name: &Token, value: Value) {
        let instance = self.borrow();

        // Check for a property setter.
        let mb_key = (ClassMemberKind::Setter, false, name.lexeme.clone());
        if let Some(setter) = instance.class.borrow().members.get(&mb_key) {
            // Bind and execute the property setter
            let bound_method = bind_method(setter, Value::from(self.clone()));
            bound_method
                .call(itpr_state, vec![value])
                .expect("failed to execute setter");
            return;
        }

        // Set the property directly
        instance
            .fields
            .borrow_mut()
            .insert(name.lexeme.clone(), value);
    }
}
