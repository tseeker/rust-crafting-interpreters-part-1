use std::fmt::Display;

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
}

impl Function {
    pub(super) fn new(
        name: Option<&Token>,
        params: &[Token],
        body: &[ast::StmtNode],
        environment: EnvironmentRef,
    ) -> Self {
        Self {
            name: name.cloned(),
            params: params.to_owned(),
            body: body.to_owned(),
            env: environment,
        }
    }

    pub(super) fn name(&self) -> Option<&Token> {
        self.name.as_ref()
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
                InterpreterFlowControl::Return(v) => return Ok(v),
                _ => panic!("unexpected flow control {:?}", result),
            }
        }
        Ok(Value::Nil)
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
