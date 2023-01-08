use std::{cell::RefCell, fmt::Display, rc::Rc};

use super::{
    class::{Class, ClassRef, Instance},
    functions::Function,
    native_fn::NativeFunction,
    Callable,
};

/// A value being handled by the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
    Object(Rc<RefCell<Object>>),
}

#[derive(Debug, Clone)]
pub enum Object {
    NativeFunction(NativeFunction),
    LoxFunction(Function),
    Class(ClassRef),
    Instance(Instance),
}

/* -------------------- *
 * Value implementation *
 * -------------------- */

impl Value {
    /// Check whether a value is truthy or not.
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(b) => *b,
            _ => true,
        }
    }

    /// Run some code using the callable object wrapped inside a value.
    /// If the value is not callable, an error function will be called
    /// instead.
    pub fn with_callable<Fok, Ferr, Rt>(&self, fok: Fok, ferr: Ferr) -> Rt
    where
        Fok: FnOnce(&dyn Callable) -> Rt,
        Ferr: FnOnce() -> Rt,
    {
        let obj = match self {
            Value::Object(obj_ref) => obj_ref.borrow(),
            _ => return ferr(),
        };
        match &*obj {
            Object::NativeFunction(func) => fok(func),
            Object::LoxFunction(func) => fok(func),
            Object::Class(class) => fok(class),
            Object::Instance(_) => ferr(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::String(s) => s.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::Object(obj) => obj.borrow().fmt(f),
        }
    }
}

impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::NativeFunction(value))))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::LoxFunction(value))))
    }
}

impl From<Class> for Value {
    fn from(value: Class) -> Self {
        Value::from(Rc::new(RefCell::new(value)))
    }
}

impl From<ClassRef> for Value {
    fn from(value: ClassRef) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Class(value))))
    }
}

impl From<Instance> for Value {
    fn from(value: Instance) -> Self {
        Value::Object(Rc::new(RefCell::new(Object::Instance(value))))
    }
}

/* --------------------- *
 * Object implementation *
 * --------------------- */

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::NativeFunction(func) => func.fmt(f),
            Object::LoxFunction(func) => func.fmt(f),
            Object::Class(cls) => cls.borrow().fmt(f),
            Object::Instance(inst) => inst.fmt(f),
        }
    }
}
