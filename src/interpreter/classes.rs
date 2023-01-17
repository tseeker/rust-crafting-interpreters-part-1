use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use lazy_static::lazy_static;

use crate::{
    ast::{ClassMemberKind, SuperExpr},
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::Token,
};

use super::{functions::Function, Callable, InterpreterState, Value};

/// This trait represents an object on which getters and setters
/// must be supported.
pub trait PropertyCarrier {
    fn get(&self, itpr_state: &mut InterpreterState, name: &Token) -> SloxResult<Value>;
    fn set(&self, itpr_state: &mut InterpreterState, name: &Token, value: Value);
    fn get_super(
        &self,
        itpr_state: &mut InterpreterState,
        super_expr: &SuperExpr,
        distance: usize,
    ) -> SloxResult<Value>;
}

/// The key for the table of class members.
pub type ClassMemberKey = (ClassMemberKind, bool, String);

/// A Lox class.
#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    superclass: Option<ClassRef>,
    members: HashMap<ClassMemberKey, Function>,
    fields: RefCell<HashMap<String, Value>>,
}

/// Classes are mostly used through references.
pub type ClassRef = Rc<RefCell<Class>>;

/// An instance of a Lox class
#[derive(Debug, Clone)]
pub struct Instance {
    class: ClassRef,
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
    pub fn new(
        name: String,
        superclass: Option<ClassRef>,
        members: HashMap<ClassMemberKey, Function>,
    ) -> Self {
        Self {
            name,
            superclass,
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

/// Attempt to find some member, identified by a key, inside a class hierarchy. If the member is
/// found, the specified function is exectued with the member as its argument, and its return value
/// is passed back to the caller. If the member is not found, None is returned.
fn with_class_member<F, Rt>(class: &ClassRef, mb_key: &ClassMemberKey, f: F) -> Option<Rt>
where
    F: FnOnce(&Function) -> Rt,
{
    let mut cls = class.clone();
    loop {
        if let Some(member) = cls.borrow().members.get(&mb_key) {
            return Some(f(member));
        }
        let nclr = if let Some(sc) = &cls.borrow().superclass {
            sc.clone()
        } else {
            return None;
        };
        cls = nclr;
    }
}

impl PropertyCarrier for ClassRef {
    fn get(&self, itpr_state: &mut InterpreterState, name: &Token) -> SloxResult<Value> {
        let class = self.borrow();

        // Check for a property getter and execute it if found.
        let mut mb_key = (ClassMemberKind::Getter, true, name.lexeme.clone());
        if let Some(value) = with_class_member(self, &mb_key, |getter| {
            let bound_method = bind_method(getter, Value::from(self.clone()));
            bound_method.call(itpr_state, vec![])
        }) {
            return value;
        }

        // Check for an actual field.
        if let Some(value) = class.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }

        // Check for a method.
        mb_key.0 = ClassMemberKind::Method;
        if let Some(method) = with_class_member(self, &mb_key, |method| {
            let bound_method = bind_method(method, Value::from(self.clone()));
            Ok(Value::from(bound_method))
        }) {
            return method;
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
        let mb_key = (ClassMemberKind::Setter, true, name.lexeme.clone());
        let lookup = with_class_member(self, &mb_key, |setter| {
            let bound_method = bind_method(setter, Value::from(self.clone()));
            bound_method
                .call(itpr_state, vec![value.clone()])
                .expect("failed to execute setter");
        });
        if lookup.is_some() {
            return;
        }

        // Set the property directly
        class.fields.borrow_mut().insert(name.lexeme.clone(), value);
    }

    fn get_super(
        &self,
        itpr_state: &mut InterpreterState,
        super_expr: &SuperExpr,
        distance: usize,
    ) -> SloxResult<Value> {
        let mb_key = (
            ClassMemberKind::Method,
            true,
            super_expr.method.lexeme.clone(),
        );

        let sc_value = itpr_state
            .environment
            .borrow()
            .get_at(distance, &super_expr.keyword.token)?;
        let class = sc_value.as_class_ref().expect("class reference expected");

        if let Some(method) = with_class_member(&class, &mb_key, |method| {
            let bound_method = bind_method(method, Value::from(self.clone()));
            Ok(Value::from(bound_method))
        }) {
            method
        } else {
            Err(SloxError::with_token(
                ErrorKind::Runtime,
                &super_expr.method,
                "undefined property".to_owned(),
            ))
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
        if let Some(value) = with_class_member(&instance.class, &mb_key, |getter| {
            let bound_method = bind_method(getter, Value::from(self.clone()));
            bound_method.call(itpr_state, vec![])
        }) {
            return value;
        }

        // Check for an actual field.
        if let Some(value) = instance.fields.borrow().get(&name.lexeme) {
            return Ok(value.clone());
        }

        // Check for a method.
        mb_key.0 = ClassMemberKind::Method;
        if let Some(method) = with_class_member(&instance.class, &mb_key, |method| {
            let bound_method = bind_method(method, Value::from(self.clone()));
            Ok(Value::from(bound_method))
        }) {
            return method;
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
        let lookup = with_class_member(&instance.class, &mb_key, |setter| {
            let bound_method = bind_method(setter, Value::from(self.clone()));
            bound_method
                .call(itpr_state, vec![value.clone()])
                .expect("failed to execute setter");
        });
        if lookup.is_some() {
            return;
        }

        // Set the property directly
        instance
            .fields
            .borrow_mut()
            .insert(name.lexeme.clone(), value);
    }

    fn get_super(
        &self,
        itpr_state: &mut InterpreterState,
        super_expr: &SuperExpr,
        distance: usize,
    ) -> SloxResult<Value> {
        let mb_key = (
            ClassMemberKind::Method,
            false,
            super_expr.method.lexeme.clone(),
        );

        let sc_value = itpr_state
            .environment
            .borrow()
            .get_at(distance, &super_expr.keyword.token)?;
        let class = sc_value.as_class_ref().expect("class reference expected");

        if let Some(method) = with_class_member(&class, &mb_key, |method| {
            let bound_method = bind_method(method, Value::from(self.clone()));
            Ok(Value::from(bound_method))
        }) {
            method
        } else {
            Err(SloxError::with_token(
                ErrorKind::Runtime,
                &super_expr.method,
                "undefined property".to_owned(),
            ))
        }
    }
}
