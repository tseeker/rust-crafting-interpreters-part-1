use crate::{
    ast,
    errors::{ErrorHandler, ParserError},
    tokens::{Token, TokenType},
};

/// The parser contains the input tokens and the current input position.
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

/// The result of one of the parser's functions.
type ParserResult<T> = Result<T, ParserError>;

impl Parser {
    /// Initialize the parser.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Parse the tokens into an AST and return it, or return nothing if a
    /// parser error occurs.
    pub fn parse(mut self, err_hdl: &mut ErrorHandler) -> Option<ast::ProgramNode> {
        self.parse_program(err_hdl)
    }

    /// Synchronize the parser after an error.
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon
                || matches!(
                    self.peek().token_type,
                    TokenType::Class
                        | TokenType::Fun
                        | TokenType::If
                        | TokenType::Print
                        | TokenType::Return
                        | TokenType::Var
                        | TokenType::While
                )
            {
                return;
            }
            self.current += 1;
        }
    }

    /* ------------------------ *
     * RECURSIVE DESCENT PARSER *
     * ------------------------ */

    /// Parse the following rule:
    /// ```
    /// program := statement*
    /// ```
    fn parse_program(&mut self, err_hdl: &mut ErrorHandler) -> Option<ast::ProgramNode> {
        let mut stmts: Vec<ast::StmtNode> = Vec::new();
        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(node) => stmts.push(node),
                Err(err) => {
                    err.report(err_hdl);
                    self.synchronize()
                }
            }
        }
        if err_hdl.had_error().is_none() {
            Some(ast::ProgramNode(stmts))
        } else {
            None
        }
    }

    /// Parse the following rule:
    /// ```
    /// statement := expression ";"
    /// statement := "print" expression ";"
    /// statement := declaration ";"
    /// ```
    fn parse_statement(&mut self) -> ParserResult<ast::StmtNode> {
        if self.expect(&[TokenType::Var]).is_some() {
            self.parse_declaration()
        } else if self.expect(&[TokenType::Print]).is_some() {
            let expression = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "expected ';' after value")?;
            Ok(ast::StmtNode::Print(expression))
        } else {
            let expression = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "expected ';' after expression")?;
            Ok(ast::StmtNode::Expression(expression))
        }
    }

    /// Parse the following rule:
    /// ```
    /// declaration := "var" IDENTIFIER ";"
    /// declaration := "var" IDENTIFIER "=" expression ";"
    /// ```
    fn parse_declaration(&mut self) -> ParserResult<ast::StmtNode> {
        let name = match self.peek().token_type {
            TokenType::Identifier(_) => self.advance().clone(),
            _ => return Err(ParserError::new(self.peek(), "expected variable name")),
        };
        let initializer: Option<ast::ExprNode> = match self.expect(&[TokenType::Equal]) {
            Some(_) => Some(self.parse_expression()?),
            None => None,
        };
        self.consume(
            &TokenType::Semicolon,
            "expected ';' after variable declaration",
        )?;
        Ok(ast::StmtNode::VarDecl(name, initializer))
    }

    /// Parse the following rule:
    /// ```
    /// expression := assignment
    /// ```
    fn parse_expression(&mut self) -> ParserResult<ast::ExprNode> {
        self.parse_assignment()
    }

    /// Parse the following rule:
    /// ```
    /// assignment := IDENTIFIER "=" equality
    /// assignment := equality
    /// ```
    fn parse_assignment(&mut self) -> ParserResult<ast::ExprNode> {
        let expr = self.parse_equality()?;
        if let Some(equals) = self.expect(&[TokenType::Equal]) {
            let value = self.parse_assignment()?;
            if let ast::ExprNode::Variable { name } = expr {
                Ok(ast::ExprNode::Assignment {
                    name,
                    value: Box::new(value),
                })
            } else {
                Err(ParserError::new(&equals, "invalid assignment target"))
            }
        } else {
            Ok(expr)
        }
    }

    /// Parse the following rule:
    /// ```
    /// equality := comparison "==" comparison
    /// equality := comparison "!=" comparison
    /// ```
    fn parse_equality(&mut self) -> ParserResult<ast::ExprNode> {
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

    /// Parse the following rule:
    /// ```
    /// comparison          := term comparison_operator term
    /// comparison_operator := "<" | "<=" | ">" | ">="
    /// ```
    fn parse_comparison(&mut self) -> ParserResult<ast::ExprNode> {
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

    /// Parse the following rule:
    /// ```
    /// term := factor "+" factor
    /// term := factor "-" factor
    /// ```
    fn parse_term(&mut self) -> ParserResult<ast::ExprNode> {
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

    /// Parse the following rule:
    /// ```
    /// factor := unary "*" unary
    /// factor := unary "/" unary
    /// ```
    fn parse_factor(&mut self) -> ParserResult<ast::ExprNode> {
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

    /// Parse the following rule:
    /// ```
    /// unary := "-" unary
    /// unary := "!" unary
    /// unary := primary
    /// ```
    fn parse_unary(&mut self) -> ParserResult<ast::ExprNode> {
        if let Some(operator) = self.expect(&[TokenType::Bang, TokenType::Minus]) {
            Ok(ast::ExprNode::Unary {
                operator,
                right: Box::new(self.parse_unary()?),
            })
        } else {
            self.parse_primary()
        }
    }

    /// Parse the following rule:
    /// ```
    /// primary := "(" expression ")"
    /// primary := FALSE | TRUE | NIL | STRING | NUMBER
    /// primary := IDENTIFIER
    /// ```
    fn parse_primary(&mut self) -> ParserResult<ast::ExprNode> {
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
                TokenType::Identifier(_) => Ok(ast::ExprNode::Variable {
                    name: self.advance().clone(),
                }),
                _ => Err(ParserError::new(self.peek(), "expected expression")),
            }
        }
    }

    /* -------------- *
     * HELPER METHODS *
     * -------------- */

    /// Expect a token of some types. If a matching token is found, the read
    /// pointer is moved and a clone of the token is returned.
    fn expect(&mut self, accepts: &[TokenType]) -> Option<Token> {
        for tt in accepts {
            if self.check(tt) {
                return Some(self.advance().clone());
            }
        }
        None
    }

    /// Consume a token of a given type. If no matching token is found, a
    /// parse error is returned instead. Otherwise the read pointer is moved.
    fn consume(&mut self, token_type: &TokenType, error: &str) -> ParserResult<&Token> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParserError::new(self.peek(), error))
        }
    }

    /// Check for a token of some type. Returns `false` if the end of the input
    /// has been reached. The read pointer isn't affected.
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().token_type == token_type
        }
    }

    /// Move the read pointer forward if the end hasn't been reached. In all
    /// cases, return the previous element (so either the element that was
    /// current before the pointer moved, or the last, non-`EOF` token).
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    /// Check whether the end of token stream has been reached by checking
    /// for the `EOF` token.
    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    /// Return a reference to the current token in the stream.
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Return a reference to the previous token in the stream.
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
