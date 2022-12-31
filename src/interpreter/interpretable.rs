use crate::{
    ast,
    errors::{ErrorHandler, InterpreterError},
    interpreter::Value,
    tokens::{Token, TokenType},
};

use super::Environment;

/// A result returned by some part of the interpreter.
pub type InterpreterResult = Result<Value, InterpreterError>;

/// An Interpretable can be evaluated and will return a value.
pub trait Interpretable {
    fn interprete(&self, environment: &mut Environment) -> InterpreterResult;
}

/// Evaluate an interpretable, returning its value.
pub fn evaluate(err_hdl: &mut ErrorHandler, ast: &dyn Interpretable) -> Option<Value> {
    let mut env = Environment::default();
    match ast.interprete(&mut env) {
        Ok(v) => Some(v),
        Err(e) => {
            e.report(err_hdl);
            None
        }
    }
}

/* ----------------------------- *
 * INTERPRETER FOR PROGRAM NODES *
 * ----------------------------- */

impl Interpretable for ast::ProgramNode {
    fn interprete(&self, environment: &mut Environment) -> InterpreterResult {
        for stmt in self.0.iter() {
            stmt.interprete(environment)?;
        }
        Ok(Value::Nil)
    }
}

/* ------------------------------- *
 * INTERPRETER FOR STATEMENT NODES *
 * ------------------------------- */

impl Interpretable for ast::StmtNode {
    fn interprete(&self, environment: &mut Environment) -> InterpreterResult {
        match self {
            ast::StmtNode::Expression(expr) => expr.interprete(environment),
            ast::StmtNode::Print(expr) => self.on_print(environment, expr),
            ast::StmtNode::VarDecl(name, expr) => self.on_var_decl(environment, name, expr),
        }
    }
}

impl ast::StmtNode {
    /// Handle the `print` statement.
    fn on_print(&self, environment: &mut Environment, expr: &ast::ExprNode) -> InterpreterResult {
        let value = expr.interprete(environment)?;
        let output = match value {
            Value::Nil => String::from("nil"),
            Value::Boolean(true) => String::from("true"),
            Value::Boolean(false) => String::from("false"),
            Value::Number(n) => n.to_string(),
            Value::String(s) => s,
        };
        println!("{}", output);
        Ok(Value::Nil)
    }

    /// Handle a variable declaration.
    fn on_var_decl(
        &self,
        environment: &mut Environment,
        name: &Token,
        initializer: &Option<ast::ExprNode>,
    ) -> InterpreterResult {
        let value = match initializer {
            Some(expr) => expr.interprete(environment)?,
            None => Value::Nil,
        };
        environment.define(name.lexeme.clone(), value);
        Ok(Value::Nil)
    }
}

/* -------------------------------- *
 * INTERPRETER FOR EXPRESSION NODES *
 * -------------------------------- */

impl Interpretable for ast::ExprNode {
    fn interprete(&self, environment: &mut Environment) -> InterpreterResult {
        match self {
            ast::ExprNode::Assignment{ name, value} => {
                let value = value.interprete(environment)?;
                environment.assign(name, value)
            }
            ast::ExprNode::Binary {
                left,
                operator,
                right,
            } => self.on_binary(environment, left, operator, right),
            ast::ExprNode::Unary { operator, right } => self.on_unary(environment, operator, right),
            ast::ExprNode::Grouping { expression } => expression.interprete(environment),
            ast::ExprNode::Litteral { value } => self.on_litteral(value),
            ast::ExprNode::Variable { name } => environment.get(name),
        }
    }
}

impl ast::ExprNode {
    /// Evaluate a binary operator.
    fn on_binary(
        &self,
        environment: &mut Environment,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let left_value = left.interprete(environment)?;
        let right_value = right.interprete(environment)?;
        match operator.token_type {
            TokenType::Plus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Minus => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Star => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                (Value::String(a), Value::Number(b)) => Ok(Value::String(a.repeat(b as usize))),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Slash => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0. {
                        Err(InterpreterError::new(operator, "division by zero"))
                    } else {
                        Ok(Value::Number(a / b))
                    }
                }
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Greater => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::GreaterEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Less => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::LessEqual => match (left_value, right_value) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
                _ => Err(InterpreterError::new(operator, "type error")),
            },

            TokenType::Equal => Ok(Value::Boolean(left_value == right_value)),
            TokenType::BangEqual => Ok(Value::Boolean(left_value != right_value)),

            _ => panic!(
                "Unsupported token type for binary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate an unary operator.
    fn on_unary(
        &self,
        environment: &mut Environment,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> InterpreterResult {
        let right_value = right.interprete(environment)?;
        match operator.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = right_value {
                    Ok(Value::Number(-n))
                } else {
                    Err(InterpreterError::new(operator, "number expected"))
                }
            }

            TokenType::Bang => Ok(Value::Boolean(!right_value.is_truthy())),

            _ => panic!(
                "Unsupported token type for unary operator (token {:?})",
                operator
            ),
        }
    }

    /// Evaluate a litteral.
    fn on_litteral(&self, value: &Token) -> InterpreterResult {
        match &value.token_type {
            TokenType::Nil => Ok(Value::Nil),
            TokenType::True => Ok(Value::Boolean(true)),
            TokenType::False => Ok(Value::Boolean(false)),
            TokenType::Number(n) => Ok(Value::Number(*n)),
            TokenType::String(s) => Ok(Value::String(s.clone())),
            _ => panic!("Unsupported token type for litteral (token {:?})", value),
        }
    }
}
