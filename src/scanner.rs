use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::{
    errors::{SloxError, SloxResult},
    tokens::{Token, TokenType},
    ErrorHandler,
};

lazy_static! {
    /// A map of keywords to token types.
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut keywords = HashMap::new();
        keywords.insert("and",      TokenType::And);
        keywords.insert("break",    TokenType::Break);
        keywords.insert("class",    TokenType::Class);
        keywords.insert("continue", TokenType::Continue);
        keywords.insert("else",     TokenType::Else);
        keywords.insert("false",    TokenType::False);
        keywords.insert("for",      TokenType::For);
        keywords.insert("fun",      TokenType::Fun);
        keywords.insert("if",       TokenType::If);
        keywords.insert("nil",      TokenType::Nil);
        keywords.insert("or",       TokenType::Or);
        keywords.insert("print",    TokenType::Print);
        keywords.insert("return",   TokenType::Return);
        keywords.insert("static",   TokenType::Static);
        keywords.insert("super",    TokenType::Super);
        keywords.insert("this",     TokenType::This);
        keywords.insert("true",     TokenType::True);
        keywords.insert("var",      TokenType::Var);
        keywords.insert("while",    TokenType::While);
        keywords
    };
}

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
    pub fn scan_tokens(mut self, err_hdl: &mut ErrorHandler) -> SloxResult<Vec<Token>> {
        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                err_hdl.report(e);
            }
        }
        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::from(""),
            line: self.line,
        });
        err_hdl.final_error(self.tokens)
    }

    /// Read the next token from the input
    fn scan_token(&mut self) -> Result<(), SloxError> {
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
            '@' => self.add_token(TokenType::Address),
            // Slash is a special case as it may be a line comment
            '/' => {
                if self.is_match('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.current += 1;
                    }
                    Ok(())
                } else if self.is_match('*') {
                    self.block_comment()
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
            '"' => self.string_litteral(),
            // Handle whitespace
            ' ' | '\r' | '\t' => Ok(()),
            '\n' => {
                self.line += 1;
                Ok(())
            }
            // Numbers
            ch if ch.is_ascii_digit() => self.number(),
            // Identifiers
            ch if ch.is_ascii_alphabetic() || ch == '_' => self.identifier(),
            // Anything else is an error
            _ => self.error("unexpected character".to_owned()),
        }
    }

    /// Read the rest of a string litteral
    fn string_litteral(&mut self) -> SloxResult<()> {
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
            self.error("unterminated string".to_owned())
        } else {
            self.current += 1; // Last '"'
            let value = self.get_substring(self.start + 1, self.current - 1);
            self.add_token(TokenType::String(value))
        }
    }

    /// Read the rest of a number.
    fn number(&mut self) -> Result<(), SloxError> {
        while self.peek().is_ascii_digit() {
            self.current += 1;
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.current += 1;
            while self.peek().is_ascii_digit() {
                self.current += 1;
            }
        }

        let tok_string = self.get_substring(self.start, self.current);
        match tok_string.parse::<f64>() {
            Ok(value) => {
                self.add_token(TokenType::Number(value))
            }
            Err(e) => self.error(format!(
                "Could not parse {} as a floating point number: {:?}",
                tok_string, e
            )),
        }
    }

    /// Read the rest of an identifier or keyword.
    fn identifier(&mut self) -> Result<(), SloxError> {
        while is_word_char(self.peek()) {
            self.current += 1;
        }
        let word = self.get_substring(self.start, self.current);
        match KEYWORDS.get(&word as &str) {
            Some(tt) => self.add_token(tt.clone()),
            None => self.add_token(TokenType::Identifier(word)),
        }
    }

    /// Read (and ignore) a block comment. Block comments may be nested.
    fn block_comment(&mut self) -> Result<(), SloxError> {
        let mut depth = 1;
        loop {
            if self.is_at_end() {
                return self.error("unterminated block comment".to_owned());
            }

            let cur = self.advance();
            let next = self.peek();
            if cur == '*' && next == '/' {
                depth -= 1;
                self.current += 1;
                if depth == 0 {
                    return Ok(());
                }
            } else if cur == '/' && next == '*' {
                depth += 1;
                self.current += 1;
            } else if cur == '\n' {
                self.line += 1;
            }
        }
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
    fn add_token(&mut self, token_type: TokenType) -> SloxResult<()> {
        let lexeme = self.get_substring(self.start, self.current);
        let token = Token {
            token_type,
            lexeme,
            line: self.line,
        };
        self.tokens.push(token);
        Ok(())
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

    /// Generate an error at the current position, with the specified message.
    fn error(&self, message: String) -> SloxResult<()> {
        Err(SloxError::scanner_error(
            self.line,
            if self.is_at_end() {
                None
            } else {
                Some(self.peek())
            },
            message,
        ))
    }
}

/// Check whether a character is either alphanumeric or an underscore.
fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
