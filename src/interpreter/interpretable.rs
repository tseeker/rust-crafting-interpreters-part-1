use std::{cell::RefCell, rc::Rc};

use crate::{
    ast,
    errors::{ErrorKind, SloxError, SloxResult},
    interpreter::{functions::Function, Environment, EnvironmentRef, Value},
    resolver::ResolvedVariables,
    tokens::{Token, TokenType},
};

/// Evaluate an interpretable, returning its value.
pub fn evaluate(ast: &ast::ProgramNode, vars: ResolvedVariables) -> SloxResult<Value> {
    let mut state = InterpreterState::new(&vars);
    ast.interpret(&mut state).map(|v| v.result())
}

/* ------- *
 * HELPERS *
 * ------- */

/// The state of the interpreter.
#[derive(Debug)]
pub struct InterpreterState<'a> {
    pub(super) globals: EnvironmentRef,
    pub(super) environment: EnvironmentRef,
    pub(super) locals: &'a ResolvedVariables,
}

impl<'a> InterpreterState<'a> {
    /// Initialize the interpreter state from the resolved variables map.
    fn new(locals: &'a ResolvedVariables) -> Self {
        let env = Rc::new(RefCell::new(Environment::default()));
        Self {
            environment: env.clone(),
            globals: env,
            locals: &locals,
        }
    }

    /// Create a child state.
    pub(super) fn create_child<'b>(parent: &InterpreterState<'b>) -> Self
    where
        'b: 'a,
    {
        InterpreterState {
            environment: Environment::create_child(&parent.environment),
            globals: parent.globals.clone(),
            locals: parent.locals,
        }
    }

    fn lookup_var(&self, name: &Token, expr_id: &usize) -> SloxResult<Value> {
        match self.locals.get(expr_id) {
            Some(distance) => self.environment.borrow().get_at(*distance, name),
            None => self.globals.borrow().get(name),
        }
    }

    fn assign_var(&self, name: &Token, expr_id: &usize, value: Value) -> SloxResult<()> {
        match self.locals.get(expr_id) {
            Some(distance) => self
                .environment
                .borrow_mut()
                .assign_at(*distance, name, value),
            None => self.globals.borrow_mut().assign(name, value),
        }
    }
}

/// Interpreter flow control, which may be either a value, a loop break or a
/// loop continuation.
#[derive(Debug)]
pub(super) enum InterpreterFlowControl {
    Result(Value),
    Break(Option<String>),
    Continue(Option<String>),
    Return(Value),
}

impl InterpreterFlowControl {
    /// Return the result's value. If the flow control value does not represent
    /// a result, panic.
    pub(super) fn result(self) -> Value {
        match self {
            Self::Result(v) => v,
            other => panic!("Result expected, {:?} found instead", other),
        }
    }

    /// Check whether a flow control value contains actual flow control
    /// information.
    fn is_flow_control(&self) -> bool {
        matches!(self, Self::Break(_) | Self::Continue(_) | Self::Return(_))
    }
}

impl Default for InterpreterFlowControl {
    fn default() -> Self {
        Self::Result(Value::Nil)
    }
}

impl From<Value> for InterpreterFlowControl {
    fn from(value: Value) -> Self {
        Self::Result(value)
    }
}

/// A result returned by some part of the interpreter.
pub(super) type InterpreterResult = SloxResult<InterpreterFlowControl>;

/// An Interpretable can be evaluated and will return a value.
pub(super) trait Interpretable {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult;
}

/// Generate an error with a static message.
fn error<T>(token: &Token, message: &str) -> SloxResult<T> {
    Err(SloxError::with_token(
        ErrorKind::Runtime,
        token,
        message.to_owned(),
    ))
}

/* ----------------------------- *
 * INTERPRETER FOR PROGRAM NODES *
 * ----------------------------- */

impl Interpretable for ast::ProgramNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        for stmt in self.0.iter() {
            stmt.interpret(es)?;
        }
        Ok(InterpreterFlowControl::default())
    }
}

/* ------------------------------- *
 * INTERPRETER FOR STATEMENT NODES *
 * ------------------------------- */

impl Interpretable for ast::StmtNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        match self {
            ast::StmtNode::VarDecl(name, expr) => self.on_var_decl(es, name, expr),
            ast::StmtNode::FunDecl { name, params, body } => {
                self.on_fun_decl(es, name, params, body)
            }
            ast::StmtNode::Expression(expr) => expr.interpret(es),
            ast::StmtNode::Print(expr) => self.on_print(es, expr),
            ast::StmtNode::Block(statements) => self.on_block(es, statements),
            ast::StmtNode::If {
                condition,
                then_branch,
                else_branch,
            } => self.on_if_statement(es, condition, then_branch, else_branch),
            ast::StmtNode::Loop {
                label,
                condition,
                body,
                after_body,
            } => self.on_loop_statement(es, label, condition, body, after_body),
            ast::StmtNode::LoopControl {
                is_break,
                loop_name,
            } => self.on_loop_control_statemement(*is_break, loop_name),
            ast::StmtNode::Return { token: _, value } => self.on_return_statement(es, value),
        }
    }
}

impl ast::StmtNode {
    /// Handle the `print` statement.
    fn on_print(&self, es: &mut InterpreterState, expr: &ast::ExprNode) -> InterpreterResult {
        let value = expr.interpret(es)?.result();
        let output = match value {
            Value::Nil => String::from("nil"),
            Value::Boolean(true) => String::from("true"),
            Value::Boolean(false) => String::from("false"),
            Value::Number(n) => n.to_string(),
            Value::String(s) => s,
            Value::Callable(c) => c.borrow().to_string(),
        };
        println!("{}", output);
        Ok(InterpreterFlowControl::default())
    }

    /// Handle a variable declaration.
    fn on_var_decl(
        &self,
        es: &mut InterpreterState,
        name: &Token,
        initializer: &Option<ast::ExprNode>,
    ) -> InterpreterResult {
        let variable = match initializer {
            Some(expr) => Some(expr.interpret(es)?.result()),
            None => None,
        };
        es.environment.borrow_mut().define(name, variable)?;
        Ok(InterpreterFlowControl::default())
    }

    /// Handle a function declaration.
    fn on_fun_decl(
        &self,
        es: &mut InterpreterState,
        name: &Token,
        params: &[Token],
        body: &[ast::StmtNode],
    ) -> InterpreterResult {
        let fun = Function::new(Some(name), params, body, es.environment.clone());
        es.environment
            .borrow_mut()
            .define(name, Some(Value::Callable(fun)))?;
        Ok(InterpreterFlowControl::default())
    }

    /// Execute the contents of a block.
    fn on_block(&self, es: &mut InterpreterState, stmts: &[ast::StmtNode]) -> InterpreterResult {
        let mut child = InterpreterState::create_child(es);
        for stmt in stmts.iter() {
            let result = stmt.interpret(&mut child)?;
            if result.is_flow_control() {
                return Ok(result);
            }
        }
        Ok(InterpreterFlowControl::default())
    }

    /// Execute an if statement.
    fn on_if_statement(
        &self,
        es: &mut InterpreterState,
        condition: &ast::ExprNode,
        then_branch: &ast::StmtNode,
        else_branch: &Option<Box<ast::StmtNode>>,
    ) -> InterpreterResult {
        if condition.interpret(es)?.result().is_truthy() {
            then_branch.interpret(es)
        } else if let Some(else_stmt) = else_branch {
            else_stmt.interpret(es)
        } else {
            Ok(InterpreterFlowControl::default())
        }
    }

    /// Execute a while statement.
    fn on_loop_statement(
        &self,
        es: &mut InterpreterState,
        label: &Option<Token>,
        condition: &ast::ExprNode,
        body: &ast::StmtNode,
        after_body: &Option<Box<ast::StmtNode>>,
    ) -> InterpreterResult {
        let ln = label.as_ref().map(|token| token.lexeme.clone());
        while condition.interpret(es)?.result().is_truthy() {
            let result = body.interpret(es)?;
            match &result {
                InterpreterFlowControl::Result(_) => (),
                InterpreterFlowControl::Continue(lv) if lv == &ln => (),
                InterpreterFlowControl::Break(lv) if lv == &ln => break,
                _ => return Ok(result),
            }
            if let Some(stmt) = after_body {
                let result = stmt.interpret(es)?;
                match &result {
                    InterpreterFlowControl::Result(_) => (),
                    InterpreterFlowControl::Continue(lv) if lv == &ln => (),
                    InterpreterFlowControl::Break(lv) if lv == &ln => break,
                    _ => return Ok(result),
                }
            }
        }
        Ok(InterpreterFlowControl::default())
    }

    /// Execute a loop control statement.
    fn on_loop_control_statemement(
        &self,
        is_break: bool,
        label: &Option<Token>,
    ) -> InterpreterResult {
        let name = label.as_ref().map(|token| token.lexeme.clone());
        if is_break {
            Ok(InterpreterFlowControl::Break(name))
        } else {
            Ok(InterpreterFlowControl::Continue(name))
        }
    }

    /// Execute a return statement.
    fn on_return_statement(
        &self,
        es: &mut InterpreterState,
        value: &Option<ast::ExprNode>,
    ) -> InterpreterResult {
        let rv = match value {
            None => Value::Nil,
            Some(expr) => expr.interpret(es)?.result(),
        };
        Ok(InterpreterFlowControl::Return(rv))
    }
}

/* -------------------------------- *
 * INTERPRETER FOR EXPRESSION NODES *
 * -------------------------------- */

impl Interpretable for ast::ExprNode {
    fn interpret(&self, es: &mut InterpreterState) -> InterpreterResult {
        match self {
            ast::ExprNode::Assignment { name, value, id } => {
                let value = value.interpret(es)?.result();
                es.assign_var(name, id, value)?;
                Ok(InterpreterFlowControl::default())
            }
            ast::ExprNode::Logical {
                left,
                operator,
                right,
            } => self.on_logic(es, left, operator, right),
            ast::ExprNode::Binary {
                left,
                operator,
                right,
            } => self.on_binary(es, left, operator, right),
            ast::ExprNode::Unary { operator, right } => self.on_unary(es, operator, right),
            ast::ExprNode::Grouping { expression } => expression.interpret(es),
            ast::ExprNode::Litteral { value } => self.on_litteral(value),
            ast::ExprNode::Variable { name, id } => Ok(es.lookup_var(name, id)?.into()),
            ast::ExprNode::Call {
                callee,
                right_paren,
                arguments,
            } => self.on_call(es, callee, right_paren, arguments),
            ast::ExprNode::Lambda { params, body } => {
                Ok(Value::Callable(Function::new(None, params, body, es.environment.clone())).into())
            }
        }
    }
}

impl ast::ExprNode {
    /// Evaluate a logical operator.
    fn on_logic(
        &self,
        es: &mut InterpreterState,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interpret(es)?.result();
        if operator.token_type == TokenType::Or && left_value.is_truthy()
            || operator.token_type == TokenType::And && !left_value.is_truthy()
        {
            Ok(left_value.into())
        } else {
            right.interpret(es)
        }
    }

    /// Evaluate a binary operator.
    fn on_binary(
        &self,
        es: &mut InterpreterState,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interpret(es)?.result();
        let right_value = right.interpret(es)?.result();
        match operator.token_type {
            TokenType::Plus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b).into()),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Minus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Star => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b).into()),
                (Value::String(a), Value::Number(b)) => {
                    Ok(Value::String(a.repeat(b as usize)).into())
                }
                _ => error(operator, "type error"),
            },

            TokenType::Slash => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0. {
                        error(operator, "division by zero")
                    } else {
                        Ok(Value::Number(a / b).into())
                    }
                }
                _ => error(operator, "type error"),
            },

            TokenType::Greater => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::GreaterEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::Less => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::LessEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b).into()),
                _ => error(operator, "type error"),
            },

            TokenType::EqualEqual => Ok(Value::Boolean(left_value == right_value).into()),
            TokenType::BangEqual => Ok(Value::Boolean(left_value != right_value).into()),

            _ => panic!(
                "Unsupported token type for binary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate an unary operator.
    fn on_unary(
        &self,
        es: &mut InterpreterState,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let right_value = right.interpret(es)?.result();
        match operator.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = right_value {
                    Ok(Value::Number(-n).into())
                } else {
                    error(operator, "number expected")
                }
            }

            TokenType::Bang => Ok(Value::Boolean(!right_value.is_truthy()).into()),

            _ => panic!(
                "Unsupported token type for unary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate a litteral.
    fn on_litteral(&self, value: &Token) -> InterpreterResult {
        let out_value = match &value.token_type {
            TokenType::Nil => Value::Nil,
            TokenType::True => Value::Boolean(true),
            TokenType::False => Value::Boolean(false),
            TokenType::Number(n) => Value::Number(*n),
            TokenType::String(s) => Value::String(s.clone()),
            _ => panic!("Unsupported token type for litteral (token {:?})", value),
        };
        Ok(out_value.into())
    }

    /// Evaluate a function call.
    fn on_call(
        &self,
        es: &mut InterpreterState,
        callee: &ast::ExprNode,
        right_paren: &Token,
        arguments: &Vec<ast::ExprNode>,
    ) -> InterpreterResult {
        let callee = callee.interpret(es)?.result();
        let arg_values = {
            let mut v = Vec::with_capacity(arguments.len());
            for argument in arguments.iter() {
                v.push(argument.interpret(es)?.result());
            }
            v
        };
        if let Value::Callable(callable_ref) = &callee {
            let callable = callable_ref.borrow();
            if callable.arity() != arg_values.len() {
                Err(SloxError::with_token(
                    ErrorKind::Runtime,
                    right_paren,
                    format!(
                        "expected {} arguments, found {}",
                        arg_values.len(),
                        callable.arity()
                    ),
                ))
            } else {
                Ok(callable.call(es, arg_values)?.into())
            }
        } else {
            error(right_paren, "can only call functions and classes")
        }
    }
}
