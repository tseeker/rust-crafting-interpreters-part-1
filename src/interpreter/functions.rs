use std::{cell::RefCell, rc::Rc};

use itertools::izip;

use crate::{
    ast,
    errors::InterpreterError,
    interpreter::{Environment, Interpretable},
    tokens::Token,
};

use super::{Callable, EnvironmentRef, Value};

/// A function implemented in the Lox-ish language.
#[derive(Debug)]
pub(crate) struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<ast::StmtNode>,
}

impl Function {
    pub(crate) fn new(
        name: &Token,
        params: &Vec<Token>,
        body: &Vec<ast::StmtNode>,
    ) -> Rc<RefCell<Self>> {
        let fun = Self {
            name: name.clone(),
            params: params.clone(),
            body: body.clone(),
        };
        Rc::new(RefCell::new(fun))
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        environment: &EnvironmentRef,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        assert_eq!(arguments.len(), self.arity());
        let param_env = Environment::create_child(environment);
        for (arg, value) in izip!(self.params.iter(), arguments.into_iter()) {
            param_env.borrow_mut().define(&arg, Some(value));
        }

        let child = Environment::create_child(&param_env);
        for stmt in self.body.iter() {
            let result = stmt.interpret(&child)?;
            if result.is_flow_control() {
                panic!("unexpected flow control");
            }
        }
        Ok(Value::Nil)
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        format!("<fun {}>", self.name.lexeme)
    }
}
