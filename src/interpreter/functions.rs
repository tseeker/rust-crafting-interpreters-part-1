use std::{cell::RefCell, rc::Rc};

use itertools::izip;

use super::{
    Callable, Environment, Interpretable, InterpreterFlowControl, InterpreterState, Value,
};
use crate::{ast, errors::SloxResult, tokens::Token};

/// A function implemented in the Lox-ish language.
#[derive(Debug)]
pub(crate) struct Function {
    name: Option<Token>,
    params: Vec<Token>,
    body: Vec<ast::StmtNode>,
}

impl Function {
    pub(crate) fn new(
        name: Option<&Token>,
        params: &[Token],
        body: &[ast::StmtNode],
    ) -> Rc<RefCell<Self>> {
        let fun = Self {
            name: name.cloned(),
            params: params.to_owned(),
            body: body.to_owned(),
        };
        Rc::new(RefCell::new(fun))
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, es: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        assert_eq!(arguments.len(), self.arity());
        let param_env = InterpreterState {
            environment: Environment::create_child(&es.environment),
            globals: es.globals.clone(),
            variables: es.variables,
        };
        for (arg, value) in izip!(self.params.iter(), arguments.into_iter()) {
            param_env
                .environment
                .borrow_mut()
                .define(arg, Some(value))
                .unwrap();
        }

        let mut child = InterpreterState {
            environment: Environment::create_child(&param_env.environment),
            globals: es.globals.clone(),
            variables: es.variables,
        };
        for stmt in self.body.iter() {
            let result = stmt.interpret(&mut child)?;
            match result {
                InterpreterFlowControl::Result(_) => (),
                InterpreterFlowControl::Return(v) => return Ok(v),
                _ => panic!("unexpected flow control {:?}", result),
            }
        }
        Ok(Value::Nil)
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        match &self.name {
            None => "<lambda>".to_owned(),
            Some(token) => format!("<fun {}>", token.lexeme),
        }
    }
}
