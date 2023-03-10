use std::collections::HashSet;

use crate::{
    ast::{
        BinaryExpr, ClassDecl, ClassMemberDecl, ClassMemberKind, ExprNode, FunDecl, GetExpr,
        ProgramNode, SetExpr, StmtNode, SuperExpr, VariableExpr,
    },
    errors::{ErrorHandler, ErrorKind, SloxError, SloxResult},
    tokens::{Token, TokenType},
};

/// The parser contains the input tokens and the current input position.
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    loop_state: Vec<LoopParsingState>,
    next_id: usize,
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

/// The type of a function that is being parsed.
#[derive(Debug)]
enum FunctionKind {
    Function,
    Lambda,
    Method,
}

impl FunctionKind {
    /// The name of this kind.
    fn name(&self) -> &'static str {
        match self {
            Self::Method => "method",
            Self::Function => "function",
            Self::Lambda => "lambda",
        }
    }

    /// The string that designates what can be found before the parameters
    fn before_params(&self) -> &'static str {
        match self {
            Self::Method => "method name",
            Self::Function => "function name",
            Self::Lambda => "'fun' keyword",
        }
    }

    /// The maximal amount of explicit parameters for a function of this kind.
    fn max_params(&self) -> usize {
        match self {
            Self::Method => 254,
            _ => 255,
        }
    }
}

impl Parser {
    /// Initialize the parser.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            loop_state: Vec::default(),
            next_id: 0,
        }
    }

    /// Parse the tokens into an AST and return it, or return nothing if a
    /// parser error occurs.
    pub fn parse(mut self, err_hdl: &mut ErrorHandler) -> SloxResult<ProgramNode> {
        self.loop_state.push(LoopParsingState::None);
        let result = self.parse_program(err_hdl);
        self.loop_state.pop();
        err_hdl.final_error(result)
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
    fn parse_program(&mut self, err_hdl: &mut ErrorHandler) -> ProgramNode {
        let mut stmts: Vec<StmtNode> = Vec::new();
        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(node) => stmts.push(node),
                Err(err) => {
                    err_hdl.report(err);
                    self.synchronize()
                }
            }
        }
        ProgramNode(stmts)
    }

    /// Parse the following rules:
    /// ```
    /// statement       := expression ";"
    /// statement       := "print" expression ";"
    /// statement       := var_declaration ";"
    /// statement       := "fun" function ";"
    /// statement       := "class" class ";"
    /// statement       := block
    /// statement       := labelled_loop
    /// statement       := if_statement
    /// statement       := while_statement
    /// statement       := for_statement
    /// statement       := loop_control_statement
    /// statement       := return_statement
    /// ```
    fn parse_statement(&mut self) -> SloxResult<StmtNode> {
        if self.expect(&[TokenType::Var]).is_some() {
            self.parse_var_declaration()
        } else if self.expect(&[TokenType::Class]).is_some() {
            self.parse_class()
        } else if self.expect(&[TokenType::Fun]).is_some() {
            if self.check(&TokenType::LeftParen) {
                // This is a lambda.
                self.current -= 1;
                self.parse_expression_stmt()
            } else {
                self.parse_function()
            }
        } else if self.expect(&[TokenType::LeftBrace]).is_some() {
            self.parse_block_contents()
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
        } else if let Some(ret) = self.expect(&[TokenType::Return]) {
            self.parse_return_statement(&ret)
        } else if let Some(print) = self.expect(&[TokenType::Print]) {
            let expression = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "expected ';' after value")?;
            Ok(StmtNode::Print(print, expression))
        } else {
            self.parse_expression_stmt()
        }
    }

    /// Parse the following rule:
    /// ```
    /// expression_stmt := expression ";"
    /// ```
    fn parse_expression_stmt(&mut self) -> SloxResult<StmtNode> {
        let expression = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "expected ';' after expression")?;
        Ok(StmtNode::Expression(expression))
    }

    /// Parse the following rule:
    /// ```
    /// var_declaration := "var" IDENTIFIER ";"
    /// var_declaration := "var" IDENTIFIER "=" expression ";"
    /// ```
    fn parse_var_declaration(&mut self) -> SloxResult<StmtNode> {
        let name = self.consume_identifier("expected variable name")?;
        let initializer: Option<ExprNode> = match self.expect(&[TokenType::Equal]) {
            Some(_) => Some(self.parse_expression()?),
            None => None,
        };
        self.consume(
            &TokenType::Semicolon,
            "expected ';' after variable declaration",
        )?;
        Ok(StmtNode::VarDecl(name, initializer))
    }

    /// Parse the following rules:
    /// ```
    /// class      := IDENTIFIER "{" member* "}"
    /// member     := "static"? gen_member
    /// gen_member := IDENTIFIER function_info
    /// gen_member := IDENTIFIER ">" block
    /// gen_member := IDENTIFIER "<" block
    /// ```
    fn parse_class(&mut self) -> SloxResult<StmtNode> {
        let name = self.consume_identifier("expected class name")?;
        let superclass = match self.expect(&[TokenType::Less]) {
            None => None,
            Some(_) => Some(VariableExpr {
                token: self.consume_identifier("expected superclass name")?,
                id: self.make_id(),
            }),
        };

        self.consume(&TokenType::LeftBrace, "'{' expected")?;
        let mut members = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let static_token = self.expect(&[TokenType::Static]);
            let identifier = self.consume_identifier("member identifier expected")?;
            let accessor = self.expect(&[TokenType::Less, TokenType::Greater]);
            let (kind, params, body) = if let Some(acc_token) = accessor {
                let body = self
                    .parse_block("'{{' expected before accessor body")?
                    .extract_block_statements();
                let (kind, params) = match acc_token.token_type {
                    TokenType::Greater => (ClassMemberKind::Getter, vec![]),
                    TokenType::Less => (ClassMemberKind::Setter, vec![identifier.clone()]),
                    _ => panic!("unexpected accessor token {:?}", acc_token),
                };
                (kind, params, body)
            } else {
                let (params, body) = self.parse_function_info(FunctionKind::Method)?;
                (ClassMemberKind::Method, params, body)
            };
            members.push(ClassMemberDecl {
                kind,
                is_static: static_token.is_some(),
                fun_decl: FunDecl {
                    name: identifier,
                    params,
                    body,
                },
            });
        }
        self.consume(&TokenType::RightBrace, "'}' expected")?;

        Ok(StmtNode::ClassDecl(ClassDecl {
            name,
            superclass,
            members,
        }))
    }

    /// Parse the following rule:
    /// ```
    /// function        := IDENTIFIER function_info
    /// ```
    /// The `kind` parameter is used to generate error messages.
    fn parse_function(&mut self) -> SloxResult<StmtNode> {
        let name = self.consume_identifier("expected function name")?;
        let (params, block) = self.parse_function_info(FunctionKind::Function)?;
        Ok(StmtNode::FunDecl(FunDecl {
            name,
            params,
            body: block,
        }))
    }

    /// Parse the following rules:
    /// ```
    /// function_info   := "(" parameters? ")" block
    /// parameters      := IDENTIFIER ( "," IDENTIFIER )*
    /// ```
    fn parse_function_info(
        &mut self,
        kind: FunctionKind,
    ) -> SloxResult<(Vec<Token>, Vec<StmtNode>)> {
        // Read the list of parameter names
        self.consume(
            &TokenType::LeftParen,
            &format!("expected '(' after {}", kind.before_params()),
        )?;

        let mut params = Vec::new();
        if self.expect(&[TokenType::RightParen]).is_none() {
            let mut names: HashSet<String> = HashSet::new();
            loop {
                if params.len() >= kind.max_params() {
                    return self.error_mv(format!(
                        "{} can't have more than {} parameters",
                        kind.name(),
                        kind.max_params()
                    ));
                }
                let name = self.consume_identifier("parameter name expected")?;
                if names.contains(&name.lexeme) {
                    return self.error_mv(format!("duplicate {} parameter", kind.name()));
                }
                names.insert(name.lexeme.clone());
                params.push(name);
                if self.expect(&[TokenType::Comma]).is_none() {
                    break;
                }
            }
            self.consume(&TokenType::RightParen, "')' expected after parameters")?;
        }

        // Read the function's body
        let block = self.parse_block(&format!("'{{' expected before {} body", kind.name()))?;
        Ok((params, block.extract_block_statements()))
    }

    /// Parse the following rule:
    /// ```
    /// block := "{" block_contents
    /// ```
    fn parse_block(&mut self, err_string: &str) -> Result<StmtNode, SloxError> {
        self.consume(&TokenType::LeftBrace, err_string)?;
        self.loop_state.push(LoopParsingState::None);
        let result = self.parse_block_contents();
        self.loop_state.pop();
        result
    }

    /// Parse the following rule:
    /// ```
    /// block_contents := statement* "}"
    /// ```
    fn parse_block_contents(&mut self) -> SloxResult<StmtNode> {
        let mut stmts: Vec<StmtNode> = Vec::new();
        while !(self.check(&TokenType::RightBrace) || self.is_at_end()) {
            stmts.push(self.parse_statement()?);
        }
        self.consume(&TokenType::RightBrace, "expected '}' after block.")?;
        Ok(StmtNode::Block(stmts))
    }

    /// Parse the following rule:
    /// ```
    /// if_statement := "if" "(" expression ")" statement
    /// if_statement := "if" "(" expression ")" statement "else" statement
    /// ```
    fn parse_if_statement(&mut self) -> SloxResult<StmtNode> {
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
        Ok(StmtNode::If {
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
    fn parse_labelled_loop(&mut self) -> SloxResult<StmtNode> {
        let name_token = self.consume_identifier("identifier expected after '@'")?;
        if self.expect(&[TokenType::While]).is_some() {
            self.parse_while_statement(Some(name_token))
        } else if self.expect(&[TokenType::For]).is_some() {
            self.parse_for_statement(Some(name_token))
        } else {
            self.error("'while' or 'for' expected after loop label")
        }
    }

    /// Parse the following rule:
    /// ```
    /// while_statement := "while" "(" expression ")" statement
    /// ```
    fn parse_while_statement(&mut self, label: Option<Token>) -> SloxResult<StmtNode> {
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
        Ok(StmtNode::Loop {
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
    fn parse_for_statement(&mut self, label: Option<Token>) -> SloxResult<StmtNode> {
        self.consume(&TokenType::LeftParen, "expected '(' after 'for'")?;

        let initializer = if self.expect(&[TokenType::Semicolon]).is_some() {
            None
        } else if self.expect(&[TokenType::Var]).is_some() {
            Some(self.parse_var_declaration()?)
        } else {
            Some(self.parse_expression_stmt()?)
        };

        let condition = if self.check(&TokenType::Semicolon) {
            ExprNode::Litteral {
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
        let while_stmt = StmtNode::Loop {
            label,
            condition,
            body: Box::new(body_stmt),
            after_body: increment.map(|incr| Box::new(StmtNode::Expression(incr))),
        };
        if let Some(init_stmt) = initializer {
            Ok(StmtNode::Block(vec![init_stmt, while_stmt]))
        } else {
            Ok(while_stmt)
        }
    }

    /// Parse the following rule:
    /// ```
    /// loop_control_statement := "break" ( IDENTIFIER )? ";"
    /// loop_control_statement := "continue" ( IDENTIFIER )? ";"
    /// ```
    fn parse_loop_control_statement(&mut self, stmt_token: &Token) -> SloxResult<StmtNode> {
        if self.loop_state() == &LoopParsingState::None {
            return Err(SloxError::with_token(
                ErrorKind::Parse,
                stmt_token,
                format!(
                    "'{}' statement found outside of loop body",
                    stmt_token.lexeme
                ),
            ));
        }

        let loop_name = if self.peek().is_identifier() {
            let name_token = self.advance().clone();
            if !self.find_named_loop(&name_token.lexeme) {
                self.expect(&[TokenType::Semicolon]);
                return Err(SloxError::with_token(
                    ErrorKind::Parse,
                    &name_token,
                    format!("no reachable loop named '{}'", name_token.lexeme),
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
        Ok(StmtNode::LoopControl {
            is_break: stmt_token.token_type == TokenType::Break,
            loop_name,
        })
    }

    /// Parse the following rule:
    /// ```
    /// return_statement := "return" expression? ";"
    /// ```
    fn parse_return_statement(&mut self, ret_token: &Token) -> SloxResult<StmtNode> {
        let value = if self.check(&TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(&TokenType::Semicolon, "';' expected after return statement")?;
        Ok(StmtNode::Return {
            token: ret_token.clone(),
            value,
        })
    }

    /// Parse the following rule:
    /// ```
    /// expression := assignment
    /// ```
    fn parse_expression(&mut self) -> SloxResult<ExprNode> {
        self.parse_assignment()
    }

    /// Parse the following rule:
    /// ```
    /// assignment := IDENTIFIER "=" equality
    /// assignment := equality
    /// ```
    fn parse_assignment(&mut self) -> SloxResult<ExprNode> {
        let expr = self.parse_logic_or()?;
        if let Some(equals) = self.expect(&[TokenType::Equal]) {
            let value = self.parse_assignment()?;
            if let ExprNode::Variable(var) = expr {
                Ok(ExprNode::Assignment {
                    name: var.token,
                    value: Box::new(value),
                    id: self.make_id(),
                })
            } else if let ExprNode::Get(get_expr) = expr {
                Ok(ExprNode::Set(SetExpr {
                    instance: get_expr.instance,
                    name: get_expr.name,
                    value: Box::new(value),
                }))
            } else {
                Err(SloxError::with_token(
                    ErrorKind::Parse,
                    &equals,
                    "invalid assignment target".to_owned(),
                ))
            }
        } else {
            Ok(expr)
        }
    }

    /// Parse the following rule:
    /// ```
    /// logic_or := logic_and ( "or" logic_and )*
    /// ```
    fn parse_logic_or(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_logic_and()?;
        while let Some(operator) = self.expect(&[TokenType::Or]) {
            let right = self.parse_logic_and()?;
            expr = ExprNode::Logical(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// logic_and := equality ( "and" equality )*
    /// ```
    fn parse_logic_and(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_equality()?;
        while let Some(operator) = self.expect(&[TokenType::And]) {
            let right = self.parse_equality()?;
            expr = ExprNode::Logical(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// equality := comparison "==" comparison
    /// equality := comparison "!=" comparison
    /// ```
    fn parse_equality(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_comparison()?;
        while let Some(operator) = self.expect(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let right = self.parse_comparison()?;
            expr = ExprNode::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// comparison          := term comparison_operator term
    /// comparison_operator := "<" | "<=" | ">" | ">="
    /// ```
    fn parse_comparison(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_term()?;
        while let Some(operator) = self.expect(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = self.parse_term()?;
            expr = ExprNode::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// term := factor ( "+" factor )*
    /// term := factor ( "-" factor )*
    /// ```
    fn parse_term(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_factor()?;
        while let Some(operator) = self.expect(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.parse_factor()?;
            expr = ExprNode::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rule:
    /// ```
    /// factor := unary ( "*" unary )*
    /// factor := unary ( "/" unary )*
    /// ```
    fn parse_factor(&mut self) -> SloxResult<ExprNode> {
        let mut expr = self.parse_unary()?;
        while let Some(operator) = self.expect(&[TokenType::Slash, TokenType::Star]) {
            let right = self.parse_unary()?;
            expr = ExprNode::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    /// Parse the following rules:
    /// ```
    /// unary := "-" unary
    /// unary := "!" unary
    /// unary := primary call_or_get*
    /// call_or_get := call_arguments
    /// call_or_get := "." IDENTIFIER
    /// ```
    fn parse_unary(&mut self) -> SloxResult<ExprNode> {
        if let Some(operator) = self.expect(&[TokenType::Bang, TokenType::Minus]) {
            Ok(ExprNode::Unary {
                operator,
                right: Box::new(self.parse_unary()?),
            })
        } else {
            let mut expr = self.parse_primary()?;
            while let Some(token) = self.expect(&[TokenType::LeftParen, TokenType::Dot]) {
                expr = match token.token_type {
                    TokenType::LeftParen => self.parse_call_arguments(expr)?,
                    TokenType::Dot => {
                        if !self.peek().is_identifier() {
                            return self.error("property name expected");
                        }
                        ExprNode::Get(GetExpr {
                            instance: Box::new(expr),
                            name: self.advance().clone(),
                        })
                    }
                    _ => panic!("unexpected token type"),
                }
            }
            Ok(expr)
        }
    }

    /// Parse the following rule:
    /// ```
    /// primary := "(" expression ")"
    /// primary := FALSE | TRUE | NIL | STRING | NUMBER
    /// primary := IDENTIFIER
    /// primary := "fun" function_info
    /// primary := "super" "." IDENTIFIER
    /// ```
    fn parse_primary(&mut self) -> SloxResult<ExprNode> {
        if self.expect(&[TokenType::LeftParen]).is_some() {
            // "(" expression ")"
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RightParen, "expected ')' after expression")?;
            Ok(ExprNode::Grouping {
                expression: Box::new(expr),
            })
        } else if let Some(super_token) = self.expect(&[TokenType::Super]) {
            // "super" "." IDENTIFIER
            self.consume(&TokenType::Dot, "expected '.' after 'super'")?;
            let identifier = self.consume_identifier("expected method name")?;
            Ok(ExprNode::Super(SuperExpr {
                keyword: VariableExpr {
                    token: super_token,
                    id: self.make_id(),
                },
                method: identifier,
            }))
        } else if self.expect(&[TokenType::Fun]).is_some() {
            // "fun" function_info
            let (params, body) = self.parse_function_info(FunctionKind::Lambda)?;
            Ok(ExprNode::Lambda { params, body })
        } else if let Some(token) =
            self.expect(&[TokenType::False, TokenType::True, TokenType::Nil])
        {
            // TRUE | FALSE | NIL
            Ok(ExprNode::Litteral { value: token })
        } else {
            // STRING | NUMBER | IDENTIFIER | "this"
            match &self.peek().token_type {
                TokenType::Number(_) | &TokenType::String(_) => Ok(ExprNode::Litteral {
                    value: self.advance().clone(),
                }),
                TokenType::Identifier(_) => Ok(ExprNode::Variable(self.make_var_expr())),
                TokenType::This => Ok(ExprNode::This(self.make_var_expr())),
                _ => self.error("expected expression"),
            }
        }
    }

    /// Help parsing the following rules:
    /// ```
    /// call      := expression "(" arguments? ")"
    /// arguments := expression ( "," expression )*
    /// ```
    fn parse_call_arguments(&mut self, callee: ExprNode) -> Result<ExprNode, SloxError> {
        let mut arguments = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() == 255 {
                    return self.error("functions may not have more than 255 arguments");
                }
                arguments.push(self.parse_expression()?);
                if self.expect(&[TokenType::Comma]).is_none() {
                    break;
                }
            }
        }
        let right_paren = self
            .consume(&TokenType::RightParen, "')' expected after arguments")?
            .clone();
        Ok(ExprNode::Call {
            callee: Box::new(callee),
            right_paren,
            arguments,
        })
    }

    /* -------------- *
     * HELPER METHODS *
     * -------------- */

    /// Generate a variable reference record based on the current token.
    fn make_var_expr(&mut self) -> VariableExpr {
        VariableExpr {
            token: self.advance().clone(),
            id: self.make_id(),
        }
    }

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
    fn consume(&mut self, token_type: &TokenType, error: &str) -> SloxResult<&Token> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            self.error(error)
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

    /// Advance if the current token is an identifier, returning a clone of
    /// the token. If it isn't, an error with the specified message is
    /// returned instead.
    fn consume_identifier(&mut self, message: &str) -> SloxResult<Token> {
        if self.peek().is_identifier() {
            Ok(self.advance().clone())
        } else {
            self.error(message)
        }
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

    /// Generate an error at the current token.
    fn error<O>(&self, message: &str) -> SloxResult<O> {
        self.error_mv(message.to_owned())
    }

    /// Generate an error at the current token.
    fn error_mv<O>(&self, message: String) -> SloxResult<O> {
        Err(SloxError::with_token(
            ErrorKind::Parse,
            self.peek(),
            message,
        ))
    }

    /// Generate an identifier and return it.
    fn make_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
