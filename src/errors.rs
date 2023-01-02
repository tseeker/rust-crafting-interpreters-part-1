use crate::tokens::{Token, TokenType};

/// The type of an error.
#[derive(Clone, Copy, Debug)]
pub enum ErrorType {
    /// The error occurred while parsing the source code.
    Parse,
    /// The error occurred while trying to run the program.
    Runtime,
}

/// Error handler. Can be used to print error messages; will also retain the
/// current error status.
#[derive(Default, Debug)]
pub struct ErrorHandler {
    had_error: Option<ErrorType>,
}

impl ErrorHandler {
    /// Check whether this handler reported an error.
    pub fn had_error(&self) -> Option<ErrorType> {
        self.had_error
    }

    /// Report an error.
    pub fn error(&mut self, err_type: ErrorType, line: usize, message: &str) {
        self.report(err_type, line, "", message)
    }

    fn report(&mut self, err_type: ErrorType, line: usize, pos: &str, message: &str) {
        if self.had_error.is_none() {
            self.had_error = Option::Some(err_type);
        }
        println!("[line {line}] Error{pos}: {message}")
    }
}

/// An error that occurred while trying to parse the input once it has been
/// scanned.
#[derive(Debug, Clone)]
pub struct ParserError {
    line: usize,
    pos: String,
    message: String,
}

impl ParserError {
    /// Initialize a parser error.
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            line: token.line,
            pos: if token.token_type == TokenType::Eof {
                String::from(" at end of input")
            } else {
                format!(" at '{}'", token.lexeme)
            },
            message: String::from(message),
        }
    }

    /// Report the error to an error handler.
    pub fn report(&self, err_hdl: &mut ErrorHandler) {
        err_hdl.report(ErrorType::Parse, self.line, &self.pos, &self.message);
    }
}

/// An error that occurred while trying to evaluate the code.
#[derive(Debug, Clone)]
pub struct InterpreterError {
    line: usize,
    pos: String,
    message: String,
}

impl InterpreterError {
    /// Initialize an interpreter error.
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            line: token.line,
            pos: format!(" at '{}'", token.lexeme),
            message: String::from(message),
        }
    }

    /// Report the error to an error handler.
    pub fn report(&self, err_hdl: &mut ErrorHandler) {
        err_hdl.report(ErrorType::Runtime, self.line, &self.pos, &self.message);
    }
}
