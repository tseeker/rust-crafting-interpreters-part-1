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
    loop_state: Vec<LoopParsingState>,
}

/// The state of the parser regarding loops. We may be parsing an unnamed or
/// named loop, or we might not be parsing a loop at all.
#[derive(Debug, Clone, PartialEq)]
enum LoopParsingState {
    None,
    Unnamed,
    Named(String),
}

impl From<&Option<Token>> for LoopParsingState {
    fn from(value: &Option<Token>) -> Self {
        match &value {
            None => LoopParsingState::Unnamed,
            Some(name) => LoopParsingState::Named(name.lexeme.clone()),
        }
    }
}

/// The result of one of the parser's functions.
type ParserResult<T> = Result<T, ParserError>;

impl Parser {
    /// Initialize the parser.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            loop_state: Vec::default(),
        }
    }

    /// Parse the tokens into an AST and return it, or return nothing if a
    /// parser error occurs.
    pub fn parse(mut self, err_hdl: &mut ErrorHandler) -> Option<ast::ProgramNode> {
        self.loop_state.push(LoopParsingState::None);
        let result = self.parse_program(err_hdl);
        self.loop_state.pop();
        result
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
    /// statement := block
    /// statement := labelled_loop
    /// statement := if_statement
    /// statement := while_statement
    /// statement := for_statement
    /// statement := loop_control_statement
    /// ```
    fn parse_statement(&mut self) -> ParserResult<ast::StmtNode> {
        if self.expect(&[TokenType::Var]).is_some() {
            self.parse_var_declaration()
        } else if self.expect(&[TokenType::LeftBrace]).is_some() {
            self.parse_block()
        } else if self.expect(&[TokenType::Address]).is_some() {
            self.parse_labelled_loop()
        } else if self.expect(&[TokenType::If]).is_some() {
            self.parse_if_statement()
        } else if self.expect(&[TokenType::While]).is_some() {
            self.parse_while_statement(None)
        } else if self.expect(&[TokenType::For]).is_some() {
            self.parse_for_statement(None)
        } else if let Some(lcs) = self.expect(&[TokenType::Break, TokenType::Continue]) {
            self.parse_loop_control_statement(&lcs)
        } else if self.expect(&[TokenType::Print]).is_some() {
            let expression = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "expected ';' after value")?;
            Ok(ast::StmtNode::Print(expression))
        } else {
            self.parse_expression_stmt()
        }
    }

    /// Parse the following rule:
    /// ```
    /// expression_stmt := expression ";"
    /// ```
    fn parse_expression_stmt(&mut self) -> ParserResult<ast::StmtNode> {
        let expression = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "expected ';' after expression")?;
        Ok(ast::StmtNode::Expression(expression))
    }

    /// Parse the following rule:
    /// ```
    /// declaration := "var" IDENTIFIER ";"
    /// declaration := "var" IDENTIFIER "=" expression ";"
    /// ```
    fn parse_var_declaration(&mut self) -> ParserResult<ast::StmtNode> {
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
    /// block := "{" statement* "}"
    /// ```
    fn parse_block(&mut self) -> ParserResult<ast::StmtNode> {
        let mut stmts: Vec<ast::StmtNode> = Vec::new();
        while !(self.check(&TokenType::RightBrace) || self.is_at_end()) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&TokenType::RightBrace, "expected '}' after block.")?;
        Ok(ast::StmtNode::Block(stmts))
    }

    /// Parse the following rule:
    /// ```
    /// if_statement := "if" "(" expression ")" statement
    /// if_statement := "if" "(" expression ")" statement "else" statement
    /// ```
    fn parse_if_statement(&mut self) -> ParserResult<ast::StmtNode> {
        self.consume(&TokenType::LeftParen, "expected '(' after 'if'")?;
        let expression = self.parse_expression()?;
        self.consume(
            &TokenType::RightParen,
            "expected ')' after condition in 'if' statement",
        )?;
        let then_branch = Box::new(self.parse_statement()?);
        let else_branch = match self.expect(&[TokenType::Else]) {
            Some(_) => Some(Box::new(self.parse_statement()?)),
            None => None,
        };
        Ok(ast::StmtNode::IfStmt {
            condition: expression,
            then_branch,
            else_branch,
        })
    }

    /// Parse the following rule:
    /// ```
    /// labelled_loop := "@" IDENTIFIER while_statement
    /// labelled_loop := "@" IDENTIFIER for_statement
    /// ```
    fn parse_labelled_loop(&mut self) -> ParserResult<ast::StmtNode> {
        let name_token = match self.peek().token_type {
            TokenType::Identifier(_) => self.advance().clone(),
            _ => {
                return Err(ParserError::new(
                    self.peek(),
                    "identifier expected after '@'",
                ))
            }
        };

        if self.expect(&[TokenType::While]).is_some() {
            self.parse_while_statement(Some(name_token))
        } else if self.expect(&[TokenType::For]).is_some() {
            self.parse_for_statement(Some(name_token))
        } else {
            Err(ParserError::new(
                self.peek(),
                "'while' or 'for' expected after loop label",
            ))
        }
    }

    /// Parse the following rule:
    /// ```
    /// while_statement := "while" "(" expression ")" statement
    /// ```
    fn parse_while_statement(&mut self, label: Option<Token>) -> ParserResult<ast::StmtNode> {
        self.consume(&TokenType::LeftParen, "expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(
            &TokenType::RightParen,
            "expected ')' after condition in 'while' statement",
        )?;
        let body = Box::new({
            self.loop_state.push(LoopParsingState::from(&label));
            let result = self.parse_statement();
            self.loop_state.pop();
            result?
        });
        Ok(ast::StmtNode::LoopStmt {
            label,
            condition,
            body,
            after_body: None,
        })
    }

    /// Parse the following rules:
    /// ```
    /// for_statement := "for" "(" for_initializer ";" expression ";" expression ")" statement
    /// for_initializer := declaration
    /// for_initializer := expression
    /// for_initializer :=
    /// ```
    fn parse_for_statement(&mut self, label: Option<Token>) -> ParserResult<ast::StmtNode> {
        self.consume(&TokenType::LeftParen, "expected '(' after 'for'")?;

        let initializer = if self.expect(&[TokenType::Semicolon]).is_some() {
            None
        } else if self.expect(&[TokenType::Var]).is_some() {
            Some(self.parse_var_declaration()?)
        } else {
            Some(self.parse_expression_stmt()?)
        };

        let condition = if self.check(&TokenType::Semicolon) {
            ast::ExprNode::Litteral {
                value: Token {
                    token_type: TokenType::True,
                    lexeme: String::from("true"),
                    line: self.peek().line,
                },
            }
        } else {
            self.parse_expression()?
        };
        self.consume(
            &TokenType::Semicolon,
            "expected ';' after condition in 'for' statement",
        )?;

        let increment = if self.check(&TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(
            &TokenType::RightParen,
            "expected ')' after increment in 'for' statement",
        )?;

        // Generate a while loop, with an optional initializer which may be
        // inside a specific block if the initializer declares a variable.
        let body_stmt = {
            self.loop_state.push(LoopParsingState::from(&label));
            let result = self.parse_statement();
            self.loop_state.pop();
            result?
        };
        let while_stmt = ast::StmtNode::LoopStmt {
            label,
            condition,
            body: Box::new(body_stmt),
            after_body: increment.map(|incr| Box::new(ast::StmtNode::Expression(incr))),
        };
        if let Some(init_stmt) = initializer {
            Ok(ast::StmtNode::Block(vec![init_stmt, while_stmt]))
        } else {
            Ok(while_stmt)
        }
    }

    /// Parse the following rule:
    /// ```
    /// loop_control_statement := "break" ( IDENTIFIER )? ";"
    /// loop_control_statement := "continue" ( IDENTIFIER )? ";"
    /// ```
    fn parse_loop_control_statement(&mut self, stmt_token: &Token) -> ParserResult<ast::StmtNode> {
        if self.loop_state() == &LoopParsingState::None {
            return Err(ParserError::new(
                stmt_token,
                &format!(
                    "'{}' statement found outside of loop body",
                    stmt_token.lexeme
                ),
            ));
        }

        let loop_name = if let TokenType::Identifier(_) = self.peek().token_type {
            let name_token = self.advance().clone();
            if !self.find_named_loop(&name_token.lexeme) {
                self.expect(&[TokenType::Semicolon]);
                return Err(ParserError::new(
                    &name_token,
                    &format!("no reachable loop named '{}'", name_token.lexeme),
                ));
            }
            Some(name_token)
        } else {
            None
        };

        self.consume(
            &TokenType::Semicolon,
            "';' expected after loop control statement",
        )?;
        Ok(ast::StmtNode::LoopControlStmt {
            is_break: stmt_token.token_type == TokenType::Break,
            loop_name,
        })
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
        let expr = self.parse_logic_or()?;
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
    /// logic_or := logic_and ( "or" logic_and )*
    /// ```
    fn parse_logic_or(&mut self) -> ParserResult<ast::ExprNode> {
        let mut expr = self.parse_logic_and()?;
        while let Some(operator) = self.expect(&[TokenType::Or]) {
            let right = self.parse_logic_and()?;
            expr = ast::ExprNode::Logical {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// logic_and := equality ( "and" equality )*
    /// ```
    fn parse_logic_and(&mut self) -> ParserResult<ast::ExprNode> {
        let mut expr = self.parse_equality()?;
        while let Some(operator) = self.expect(&[TokenType::And]) {
            let right = self.parse_equality()?;
            expr = ast::ExprNode::Logical {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            };
        }
        Ok(expr)
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
    /// term := factor ( "+" factor )*
    /// term := factor ( "-" factor )*
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
    /// factor := unary ( "*" unary )*
    /// factor := unary ( "/" unary )*
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
    /// unary := primary call_arguments*
    /// ```
    fn parse_unary(&mut self) -> ParserResult<ast::ExprNode> {
        if let Some(operator) = self.expect(&[TokenType::Bang, TokenType::Minus]) {
            Ok(ast::ExprNode::Unary {
                operator,
                right: Box::new(self.parse_unary()?),
            })
        } else {
            let mut expr = self.parse_primary()?;
            while self.expect(&[TokenType::LeftParen]).is_some() {
                expr = self.parse_call_arguments(expr)?;
            }
            Ok(expr)
        }
    }

    /// Parse the following rule:
    /// ```
    /// primary := "(" expression ")"
    /// primary := FALSE | TRUE | NIL | STRING | NUMBER
    /// primary := IDENTIFIER
    /// primary := call
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

    /// Help parsing the following rules:
    /// ```
    /// call      := expression "(" arguments? ")"
    /// arguments := expression ( "," expression )*
    /// ```
    fn parse_call_arguments(
        &mut self,
        callee: ast::ExprNode,
    ) -> Result<ast::ExprNode, ParserError> {
        let mut arguments = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() == 255 {
                    return Err(ParserError::new(
                        self.peek(),
                        "functions may not have more than 255 arguments",
                    ));
                }
                arguments.push(self.parse_expression()?);
                if self.expect(&[TokenType::Comma]).is_some() {
                    break;
                }
            }
        }
        let right_paren = self
            .consume(&TokenType::RightParen, "')' expected after arguments")?
            .clone();
        Ok(ast::ExprNode::Call {
            callee: Box::new(callee),
            right_paren,
            arguments,
        })
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

    /// Take a peek at the current loop state.
    fn loop_state(&self) -> &LoopParsingState {
        &self.loop_state[self.loop_state.len() - 1]
    }

    /// Find a loop with a given name in the loop state. Stops looking when
    /// the first non-loop entry is reached.
    fn find_named_loop(&self, name: &str) -> bool {
        let mut pos = self.loop_state.len() - 1;
        loop {
            match &self.loop_state[pos] {
                LoopParsingState::None => break,
                LoopParsingState::Unnamed => (),
                LoopParsingState::Named(n) if n == name => return true,
                LoopParsingState::Named(_) => (),
            }
            pos -= 1;
        }
        false
    }
}
