use std::{cell::RefCell, rc::Rc};

use crate::{
    ast,
    errors::{ErrorHandler, InterpreterError},
    interpreter::{Environment, EnvironmentRef, Value},
    tokens::{Token, TokenType},
};

/// Evaluate an interpretable, returning its value.
pub fn evaluate(err_hdl: &mut ErrorHandler, ast: &dyn Interpretable) -> Option<Value> {
    let env = Rc::new(RefCell::new(Environment::default()));
    match ast.interprete(&env) {
        Ok(v) => Some(v.result()),
        Err(e) => {
            e.report(err_hdl);
            None
        }
    }
}

/* ------- *
 * HELPERS *
 * ------- */

/// Interpreter flow control, which may be either a value, a loop break or a
/// loop continuation.
#[derive(Debug)]
pub enum InterpreterFlowControl {
    Result(Value),
    Break(Option<String>),
    Continue(Option<String>),
}

impl InterpreterFlowControl {
    /// Return the result's value. If the flow control value does not represent
    /// a result, panic.
    fn result(self) -> Value {
        match self {
            Self::Result(v) => v,
            other => panic!("Result expected, {:?} found instead", other),
        }
    }

    /// Check whether a flow control value contains actual flow control
    /// information.
    fn is_flow_control(&self) -> bool {
        matches!(self, Self::Break(_) | Self::Continue(_))
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
pub type InterpreterResult = Result<InterpreterFlowControl, InterpreterError>;

/// An Interpretable can be evaluated and will return a value.
pub trait Interpretable {
    fn interprete(&self, environment: &EnvironmentRef) -> InterpreterResult;
}

/* ----------------------------- *
 * INTERPRETER FOR PROGRAM NODES *
 * ----------------------------- */

impl Interpretable for ast::ProgramNode {
    fn interprete(&self, environment: &EnvironmentRef) -> InterpreterResult {
        for stmt in self.0.iter() {
            stmt.interprete(environment)?;
        }
        Ok(InterpreterFlowControl::default())
    }
}

/* ------------------------------- *
 * INTERPRETER FOR STATEMENT NODES *
 * ------------------------------- */

impl Interpretable for ast::StmtNode {
    fn interprete(&self, environment: &EnvironmentRef) -> InterpreterResult {
        match self {
            ast::StmtNode::Expression(expr) => expr.interprete(environment),
            ast::StmtNode::Print(expr) => self.on_print(environment, expr),
            ast::StmtNode::VarDecl(name, expr) => self.on_var_decl(environment, name, expr),
            ast::StmtNode::Block(statements) => self.on_block(environment, statements),
            ast::StmtNode::IfStmt {
                condition,
                then_branch,
                else_branch,
            } => self.on_if_statement(environment, condition, then_branch, else_branch),
            ast::StmtNode::LoopStmt {
                label,
                condition,
                body,
                after_body,
            } => self.on_loop_statement(environment, label, condition, body, after_body),
            ast::StmtNode::LoopControlStmt {
                is_break,
                loop_name,
            } => self.on_loop_control_statemement(*is_break, loop_name),
        }
    }
}

impl ast::StmtNode {
    /// Handle the `print` statement.
    fn on_print(&self, environment: &EnvironmentRef, expr: &ast::ExprNode) -> InterpreterResult {
        let value = expr.interprete(environment)?.result();
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
        environment: &EnvironmentRef,
        name: &Token,
        initializer: &Option<ast::ExprNode>,
    ) -> InterpreterResult {
        let variable = match initializer {
            Some(expr) => Some(expr.interprete(environment)?.result()),
            None => None,
        };
        environment.borrow_mut().define(name, variable)?;
        Ok(InterpreterFlowControl::default())
    }

    /// Execute the contents of a block.
    fn on_block(&self, environment: &EnvironmentRef, stmts: &[ast::StmtNode]) -> InterpreterResult {
        let child = Environment::create_child(environment);
        for stmt in stmts.iter() {
            let result = stmt.interprete(&child)?;
            if result.is_flow_control() {
                return Ok(result);
            }
        }
        Ok(InterpreterFlowControl::default())
    }

    /// Execute an if statement.
    fn on_if_statement(
        &self,
        environment: &EnvironmentRef,
        condition: &ast::ExprNode,
        then_branch: &ast::StmtNode,
        else_branch: &Option<Box<ast::StmtNode>>,
    ) -> InterpreterResult {
        if condition.interprete(environment)?.result().is_truthy() {
            then_branch.interprete(environment)
        } else if let Some(else_stmt) = else_branch {
            else_stmt.interprete(environment)
        } else {
            Ok(InterpreterFlowControl::default())
        }
    }

    /// Execute a while statement.
    fn on_loop_statement(
        &self,
        environment: &EnvironmentRef,
        label: &Option<Token>,
        condition: &ast::ExprNode,
        body: &ast::StmtNode,
        after_body: &Option<Box<ast::StmtNode>>,
    ) -> InterpreterResult {
        let ln = label.as_ref().map(|token| token.lexeme.clone());
        while condition.interprete(environment)?.result().is_truthy() {
            let result = body.interprete(environment)?;
            match &result {
                InterpreterFlowControl::Result(_) => (),
                InterpreterFlowControl::Continue(lv) if lv == &ln => (),
                InterpreterFlowControl::Break(lv) if lv == &ln => break,
                _ => return Ok(result),
            }
            if let Some(stmt) = after_body {
                let result = stmt.interprete(environment)?;
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
}

/* -------------------------------- *
 * INTERPRETER FOR EXPRESSION NODES *
 * -------------------------------- */

impl Interpretable for ast::ExprNode {
    fn interprete(&self, environment: &EnvironmentRef) -> InterpreterResult {
        match self {
            ast::ExprNode::Assignment { name, value } => {
                let value = value.interprete(environment)?.result();
                environment.borrow_mut().assign(name, value)?;
                Ok(InterpreterFlowControl::default())
            }
            ast::ExprNode::Logical {
                left,
                operator,
                right,
            } => self.on_logic(environment, left, operator, right),
            ast::ExprNode::Binary {
                left,
                operator,
                right,
            } => self.on_binary(environment, left, operator, right),
            ast::ExprNode::Unary { operator, right } => self.on_unary(environment, operator, right),
            ast::ExprNode::Grouping { expression } => expression.interprete(environment),
            ast::ExprNode::Litteral { value } => self.on_litteral(value),
            ast::ExprNode::Variable { name } => Ok(environment.borrow().get(name)?.into()),
            ast::ExprNode::Call {
                callee,
                right_paren,
                arguments,
            } => self.on_call(environment, callee, right_paren, arguments),
        }
    }
}

impl ast::ExprNode {
    /// Evaluate a logical operator.
    fn on_logic(
        &self,
        environment: &EnvironmentRef,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interprete(environment)?.result();
        if operator.token_type == TokenType::Or && left_value.is_truthy()
            || operator.token_type == TokenType::And && !left_value.is_truthy()
        {
            Ok(left_value.into())
        } else {
            right.interprete(environment)
        }
    }

    /// Evaluate a binary operator.
    fn on_binary(
        &self,
        environment: &EnvironmentRef,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interprete(environment)?.result();
        let right_value = right.interprete(environment)?.result();
        match operator.token_type {
            TokenType::Plus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b).into()),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Minus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Star => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b).into()),
                (Value::String(a), Value::Number(b)) => {
                    Ok(Value::String(a.repeat(b as usize)).into())
                }
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Slash => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0. {
                        Err(InterpreterError::new(operator, "division by zero"))
                    } else {
                        Ok(Value::Number(a / b).into())
                    }
                }
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Greater => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::GreaterEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Less => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::LessEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b).into()),
                _ => Err(InterpreterError::new(operator, "type error")),
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
        environment: &EnvironmentRef,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let right_value = right.interprete(environment)?.result();
        match operator.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = right_value {
                    Ok(Value::Number(-n).into())
                } else {
                    Err(InterpreterError::new(operator, "number expected"))
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
        environment: &EnvironmentRef,
        callee: &ast::ExprNode,
        right_paren: &Token,
        arguments: &Vec<ast::ExprNode>,
    ) -> InterpreterResult {
        let callee = callee.interprete(environment)?.result();
        let arg_values = {
            let mut v = Vec::with_capacity(arguments.len());
            for argument in arguments.iter() {
                v.push(argument.interprete(environment)?.result());
            }
            v
        };
        if let Value::Callable(callable_ref) = &callee {
            let callable = callable_ref.borrow();
            if callable.arity() != arg_values.len() {
                Err(InterpreterError::new(
                    right_paren,
                    &format!(
                        "expected {} arguments, found {}",
                        arg_values.len(),
                        callable.arity()
                    ),
                ))
            } else {
                Ok(callable.call(environment, arg_values)?.into())
            }
        } else {
            Err(InterpreterError::new(
                right_paren,
                "can only call functions and classes",
            ))
        }
    }
}
