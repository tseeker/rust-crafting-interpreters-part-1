use crate::tokens::{Token, TokenType};

/// This trait should be implemented by nodes to allow AST dumps.
pub trait AstDumper {
    /// Dump the node as a string.
    fn dump(&self) -> String;
}

/// An AST node that represents an expression.
#[derive(Debug, Clone)]
pub enum ExprNode {
    /// Binary expression.
    Binary {
        left: Box<ExprNode>,
        operator: Token,
        right: Box<ExprNode>,
    },

    /// Unary expression.
    Unary {
        operator: Token,
        right: Box<ExprNode>,
    },

    /// Grouping expression, containing a sub-expression.
    Grouping { expression: Box<ExprNode> },

    /// A litteral value, represented by the corresponding token.
    Litteral { value: Token },
}

impl AstDumper for ExprNode {
    fn dump(&self) -> String {
        match self {
            Self::Binary {
                left,
                operator,
                right,
            } => {
                format!("( {} {} {} )", operator.lexeme, left.dump(), right.dump())
            }
            Self::Unary { operator, right } => {
                format!("( {} {} )", operator.lexeme, right.dump())
            }
            Self::Grouping { expression } => {
                format!("( {} )", expression.dump())
            }
            Self::Litteral { value } => match &value.token_type {
                TokenType::String(s) => s.clone(),
                TokenType::Number(n) => format!("{n}"),
                _ => panic!("Unexpected token type for token {:#?}", value),
            },
        }
    }
}
