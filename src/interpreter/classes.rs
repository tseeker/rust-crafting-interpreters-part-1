use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    rc::Rc,
};

use crate::{
    errors::{ErrorKind, SloxError, SloxResult},
    tokens::{Token, TokenType},
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
    fields: HashMap<String, Value>,
}

/// A method bound to an instance.
#[derive(Debug, Clone)]
pub struct BoundMethod {
    instance: Value,
    method: String,
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
        0
    }

    fn call(
        &self,
        _itpr_state: &mut InterpreterState,
        _arguments: Vec<Value>,
    ) -> SloxResult<Value> {
        let instance = Instance::new(self.clone());
        Ok(Value::from(instance))
    }
}

/* ----------------------- *
 * Instance implementation *
 * ----------------------- */

impl Instance {
    fn new(class: ClassRef) -> Self {
        Self {
            class,
            fields: HashMap::default(),
        }
    }

    pub(super) fn get(&self, this_value: &Value, name: &Token) -> SloxResult<Value> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(value.clone());
        }
        if self.class.borrow().methods.get(&name.lexeme).is_some() {
            let bound_method = BoundMethod {
                instance: this_value.clone(),
                method: name.lexeme.clone(),
            };
            return Ok(Value::from(bound_method));
        }

        Err(SloxError::with_token(
            ErrorKind::Runtime,
            name,
            "undefined property".to_owned(),
        ))
    }

    pub(super) fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }

    fn with_method<F, Rt>(&self, name: &str, f: F) -> Rt
    where
        F: FnOnce(&Function) -> Rt,
    {
        let cls = self.class.borrow();
        let method = cls
            .methods
            .get(name)
            .expect(&format!("Method {} not found", name));
        f(method)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<Instance of {}>", self.class.borrow(),))
    }
}

/* --------------------------- *
 * Bound method implementation *
 * --------------------------- */

impl BoundMethod {
    fn with_method<F, Rt>(&self, f: F) -> Rt
    where
        F: FnOnce(&Function) -> Rt,
    {
        self.instance.with_instance(
            |instance| instance.with_method(&self.method, f),
            || panic!("Instance value does not contain an instance"),
        )
    }

    fn this_token(&self) -> Token {
        Token {
            token_type: TokenType::This,
            lexeme: "this".to_owned(),
            line: self.with_method(|method| method.name().expect("Method has no name").line),
        }
    }
}

impl Callable for BoundMethod {
    fn arity(&self) -> usize {
        self.with_method(|m| m.arity())
    }

    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        let mut this_env = InterpreterState::create_child(itpr_state);
        this_env
            .environment
            .borrow_mut()
            .define(&self.this_token(), Some(self.instance.clone()))?;
        println!("{:?}", this_env.locals);
        self.with_method(|m| m.call(&mut this_env, arguments))
    }
}

impl Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.instance.with_instance(
            |instance| {
                f.write_fmt(format_args!(
                    "<Bound method {} of {}>",
                    self.method, instance
                ))
            },
            || panic!("Instance value does not contain an instance"),
        )
    }
}
