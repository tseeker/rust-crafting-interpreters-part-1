use std::{cell::RefCell, fmt::Display, rc::Rc};

use lazy_static::lazy_static;

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::{Token, TokenType},
};

use super::{
    classes::{Class, ClassRef, Instance, InstanceRef, PropertyCarrier},
    functions::Function,
    native_fn::NativeFunction,
    Callable, InterpreterState,
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
    Instance(InstanceRef),
}

lazy_static! {
    static ref TO_STRING_TOKEN: Token = {
        let name = "to_string".to_owned();
        Token {
            token_type: TokenType::Identifier(name.clone()),
            lexeme: name,
            line: 0,
        }
    };
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

    /// Get the class reference from a value that carries a class reference, or
    /// None from a value that doesn't.
    pub fn as_class_ref(&self) -> Option<ClassRef> {
        match self {
            Value::Object(obj_ref) => match &*obj_ref.borrow() {
                Object::Class(cls_ref) => Some(cls_ref.clone()),
                _ => None,
            },
            _ => None,
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

    /// Run some code against an instance value. If the value does not
    /// contain an instance, an error function will be called instead.
    pub fn with_instance<Fok, Ferr, Rt>(&self, fok: Fok, ferr: Ferr) -> Rt
    where
        Fok: FnOnce(&InstanceRef) -> Rt,
        Ferr: FnOnce() -> Rt,
    {
        let obj = match self {
            Value::Object(obj_ref) => obj_ref.borrow(),
            _ => return ferr(),
        };
        match &*obj {
            Object::Instance(inst) => fok(inst),
            _ => ferr(),
        }
    }

    /// Run some code against a property carrier value (either an instance
    /// or a class). If the value does not contain such an object, an error
    /// function will be called instead.
    pub fn with_property_carrier<Fok, Ferr, Rt>(&self, fok: Fok, ferr: Ferr) -> Rt
    where
        Fok: FnOnce(&dyn PropertyCarrier) -> Rt,
        Ferr: FnOnce() -> Rt,
    {
        let obj = match self {
            Value::Object(obj_ref) => obj_ref.borrow(),
            _ => return ferr(),
        };
        match &*obj {
            Object::Class(cls) => fok(cls),
            Object::Instance(inst) => fok(inst),
            _ => ferr(),
        }
    }

    /// Convert a value into a string. This can only be done fully with an interpreter state,
    /// because it might require calling an instance's to_string method.
    pub fn convert_to_string(
        &self,
        es: &mut InterpreterState,
        at_token: &Token,
    ) -> SloxResult<String> {
        self.with_instance(
            |inst| {
                if let Ok(result) = inst.get(es, &TO_STRING_TOKEN) {
                    result
                        .with_callable(
                            |callable| callable.call(es, vec![]),
                            || {
                                Err(SloxError::with_token(
                                    ErrorKind::Runtime,
                                    at_token,
                                    "to_string() isn't callable".to_owned(),
                                ))
                            },
                        )
                        .map(|r| r.to_string())
                } else {
                    Ok(inst.borrow().to_string())
                }
            },
            || Ok(self.to_string()),
        )
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
        Value::from(Rc::new(RefCell::new(value)))
    }
}

impl From<InstanceRef> for Value {
    fn from(value: InstanceRef) -> Self {
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
            Object::Instance(inst) => inst.borrow().fmt(f),
        }
    }
}
