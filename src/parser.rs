use crate::{
    ast,
    errors::{ErrorHandler, ParserError},
    tokens::{Token, TokenType},
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self, err_hdl: &mut ErrorHandler) -> Option<ast::ExprNode> {
        match self.parse_expression() {
            Ok(expr) => Some(expr),
            Err(e) => {
                e.report(err_hdl);
                None
            }
        }
    }

    fn parse_expression(&mut self) -> Result<ast::ExprNode, ParserError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<ast::ExprNode, ParserError> {
        let mut expr = self.parse_comparison()?;
        while let Some(operator) = self.expect(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let right = self.parse_comparison()?;
            expr = ast::ExprNode::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<ast::ExprNode, ParserError> {
        let mut expr = self.parse_term()?;
        while let Some(operator) = self.expect(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = self.parse_term()?;
            expr = ast::ExprNode::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<ast::ExprNode, ParserError> {
        let mut expr = self.parse_factor()?;
        while let Some(operator) = self.expect(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.parse_factor()?;
            expr = ast::ExprNode::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<ast::ExprNode, ParserError> {
        let mut expr = self.parse_unary()?;
        while let Some(operator) = self.expect(&[TokenType::Slash, TokenType::Star]) {
            let right = self.parse_unary()?;
            expr = ast::ExprNode::Binary {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<ast::ExprNode, ParserError> {
        if let Some(operator) = self.expect(&[TokenType::Bang, TokenType::Minus]) {
            Ok(ast::ExprNode::Unary {
                operator: operator.clone(),
                right: Box::new(self.parse_unary()?),
            })
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<ast::ExprNode, ParserError> {
        if self.expect(&[TokenType::LeftParen]).is_some() {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RightParen, "expected ')' after expression")?;
            Ok(ast::ExprNode::Grouping {
                expression: Box::new(expr),
            })
        } else if let Some(token) =
            self.expect(&[TokenType::False, TokenType::True, TokenType::Nil])
        {
            Ok(ast::ExprNode::Litteral { value: token })
        } else {
            match &self.peek().token_type {
                TokenType::Number(_) | &TokenType::String(_) => Ok(ast::ExprNode::Litteral {
                    value: self.advance().clone(),
                }),
                _ => Err(ParserError::new(self.peek(), "expected expression")),
            }
        }
    }

    fn expect(&mut self, accepts: &[TokenType]) -> Option<Token> {
        for tt in accepts {
            if self.check(tt) {
                return Some(self.advance().clone());
            }
        }
        None
    }

    fn consume(&mut self, token_type: &TokenType, error: &str) -> Result<&Token, ParserError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParserError::new(self.peek(), error))
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().token_type == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
