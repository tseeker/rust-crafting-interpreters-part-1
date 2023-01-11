use std::{cell::RefMut, fmt::Display};

use itertools::izip;

use super::{
    Callable, Environment, EnvironmentRef, Interpretable, InterpreterFlowControl, InterpreterState,
    Value,
};
use crate::{ast, errors::SloxResult, tokens::Token};

/// A function implemented in the Lox-ish language.
#[derive(Debug, Clone)]
pub struct Function {
    name: Option<Token>,
    params: Vec<Token>,
    body: Vec<ast::StmtNode>,
    env: EnvironmentRef,
    is_initializer: bool,
}

impl Function {
    pub(super) fn new(
        name: Option<&Token>,
        params: &[Token],
        body: &[ast::StmtNode],
        environment: EnvironmentRef,
        is_initializer: bool,
    ) -> Self {
        Self {
            name: name.cloned(),
            params: params.to_owned(),
            body: body.to_owned(),
            env: environment,
            is_initializer,
        }
    }

    pub(super) fn copy_with_child_env(&self) -> Self {
        Self {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            env: Environment::create_child(&self.env),
            is_initializer: self.is_initializer,
        }
    }

    pub(super) fn env(&self) -> RefMut<Environment> {
        self.env.borrow_mut()
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, itpr_state: &mut InterpreterState, arguments: Vec<Value>) -> SloxResult<Value> {
        assert_eq!(arguments.len(), self.arity());
        let param_env = InterpreterState {
            environment: Environment::create_child(&self.env),
            globals: itpr_state.globals.clone(),
            locals: itpr_state.locals,
        };
        for (arg, value) in izip!(self.params.iter(), arguments.into_iter()) {
            param_env
                .environment
                .borrow_mut()
                .define(arg, Some(value))
                .unwrap();
        }

        let mut child = InterpreterState::create_child(&param_env);
        for stmt in self.body.iter() {
            let result = stmt.interpret(&mut child)?;
            match result {
                InterpreterFlowControl::Result(_) => (),
                InterpreterFlowControl::Return(v) if !self.is_initializer => return Ok(v),
                InterpreterFlowControl::Return(_) => {
                    return Ok(itpr_state.environment.borrow().read("this"))
                }
                _ => panic!("unexpected flow control {:?}", result),
            }
        }
        if self.is_initializer {
            Ok(itpr_state.environment.borrow().read("this"))
        } else {
            Ok(Value::Nil)
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            None => f.write_str("<lambda>"),
            Some(token) => {
                f.write_str("<fun ")?;
                f.write_str(&token.lexeme)?;
                f.write_str(">")
            }
        }
    }
}
