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
            // Single-character tokens
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
            // Slash is a special case as it may be a line comment
            '/' => {
                if self.is_match('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.current += 1;
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            // Things that may be either alone or followed by '='
            '!' => {
                if self.is_match('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.is_match('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.is_match('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                if self.is_match('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            // String litterals
            '"' => self.string_litteral(err_hdl),
            // Numbers
            '0'..='9' => self.number(err_hdl),
            // Handle whitespace
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
            // Anything else is an error
            ch => {
                err_hdl.error(self.line, &format!("unexpected character {:#?}", ch));
            }
        }
    }

    /// Read the rest of a string litteral
    fn string_litteral(&mut self, err_hdl: &mut ErrorHandler) {
        loop {
            let p = self.peek();
            if p == '"' || self.is_at_end() {
                break;
            }
            if p == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_at_end() {
            err_hdl.error(self.line, "unterminated string");
        } else {
            self.current += 1; // Last '"'
            let value = self.get_substring(self.start + 1, self.current - 1);
            self.add_token(TokenType::String(value));
        }
    }

    /// Read the rest of a number.
    fn number(&mut self, err_hdl: &mut ErrorHandler) {
        while self.peek().is_digit(10) {
            self.current += 1;
        }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.current += 1;
            while self.peek().is_digit(10) {
                self.current += 1;
            }
        }

        let tok_string = self.get_substring(self.start, self.current);
        match tok_string.parse::<f64>() {
            Err(e) => {
                err_hdl.error(
                    self.line,
                    &format!(
                        "Could not parse {} as a floating point number: {:?}",
                        tok_string, e
                    ),
                );
            }
            Ok(value) => {
                self.add_token(TokenType::Number(value));
            }
        };
    }

    /// Check whether the end of the input has been reached.
    fn is_at_end(&self) -> bool {
        self.current >= self.len
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

    /// Returns the current character, or a NULL character if the end has been
    /// reached.
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.cur_char()
        }
    }

    /// Returns the next character, or a NULL character if the end has been
    /// reached.
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.chars().count() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    /// Read the current character.
    fn cur_char(&self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }

    /// Add a token to the output.
    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self.get_substring(self.start, self.current);
        let token = Token {
            token_type,
            lexeme,
            line: self.line,
        };
        self.tokens.push(token)
    }

    /// Get a substring from the source.
    fn get_substring(&self, start: usize, end: usize) -> String {
        assert!(start <= end);
        self.source
            .chars()
            .skip(start)
            .take(end - start)
            .collect::<String>()
    }
}
