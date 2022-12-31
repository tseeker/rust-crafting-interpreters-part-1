use crate::{
    ast,
    errors::InterpreterError,
    tokens::{Token, TokenType},
};

/// A value being handled by the interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl Value {
    /// Check whether a value is truthy or not.
    pub fn is_truthy(&self) -> bool {
        if self == &Value::Nil {
            false
        } else if let Value::Boolean(b) = self {
            *b
        } else {
            true
        }
    }
}

/// An Interpretable can be evaluated and will return a value.
pub trait Interpretable {
    fn interprete(&self) -> Result<Value, InterpreterError>;
}


/* -------------------------------- *
 * INTERPRETER FOR EXPRESSION NODES *
 * -------------------------------- */

impl Interpretable for ast::ExprNode {
    fn interprete(&self) -> Result<Value, InterpreterError> {
        match self {
            ast::ExprNode::Binary {
                left,
                operator,
                right,
            } => self.on_binary(left, operator, right),
            ast::ExprNode::Unary { operator, right } => self.on_unary(operator, right),
            ast::ExprNode::Grouping { expression } => expression.interprete(),
            ast::ExprNode::Litteral { value } => self.on_litteral(value),
        }
    }
}

impl ast::ExprNode {
    /// Evaluate a binary operator.
    fn on_binary(
        &self,
        left: &ast::ExprNode,
        operator: &Token,
        right: &ast::ExprNode,
    ) -> Result<Value, InterpreterError> {
        let left_value = left.interprete()?;
        let right_value = right.interprete()?;
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
    fn on_unary(&self, operator: &Token, right: &ast::ExprNode) -> Result<Value, InterpreterError> {
        let right_value = right.interprete()?;
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
    fn on_litteral(&self, value: &Token) -> Result<Value, InterpreterError> {
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
