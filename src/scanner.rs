use crate::{tokens::TokenType, ErrorHandler};

use super::tokens::Token;

/// The scanner's state, including the source it is scanning.
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    len: usize,
    line: usize,
}

impl Scanner {
    /// Initialize a scanner by specifying the source code to scan.
    pub fn new(source: String) -> Scanner {
        let len = source.chars().count();
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            len,
            line: 1,
        }
    }

    /// Scan the source code, generating the list of tokens and returning it.
    /// The scanner itself is destroyed once the process is complete.
    pub fn scan_tokens(mut self, err_hdl: &mut ErrorHandler) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token(err_hdl);
        }
        self.tokens
    }

    /// Read the next token from the input
    fn scan_token(&mut self, err_hdl: &mut ErrorHandler) {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                if self.is_match('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            },
            '=' => {
                if self.is_match('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            },
            '<' => {
                if self.is_match('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            },
            '>' => {
                if self.is_match('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            },
            ch => err_hdl.error(self.line, &format!("unexpected character '{ch}'")),
        }
    }

    /// Advance to the next character and return it.
    fn advance(&mut self) -> char {
        let ch = self.cur_char();
        self.current += 1;
        ch
    }

    /// Consume the current character if it matches the argument.
    fn is_match(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.cur_char() == expected {
            self.current += 1;
            true
        } else {
            false
        }
    }

    /// Read the current character.
    fn cur_char(&self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }

    /// Check whether the end of the input has been reached.
    fn is_at_end(&self) -> bool {
        self.current >= self.len
    }

    /// Add a token to the output.
    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect::<String>();
        let token = Token {
            token_type,
            lexeme,
            line: self.line,
        };
        self.tokens.push(token)
    }
}
