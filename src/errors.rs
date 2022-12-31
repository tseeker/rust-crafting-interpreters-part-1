use crate::tokens::{Token, TokenType};

/// Error handler. Can be used to print error messages; will also retain the
/// current error status.
#[derive(Default, Debug)]
pub struct ErrorHandler {
    had_error: bool,
}

impl ErrorHandler {
    /// Check whether this handler reported an error.
    pub fn had_error(&self) -> bool {
        self.had_error
    }

    /// Report an error.
    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message)
    }

    fn report(&mut self, line: usize, pos: &str, message: &str) {
        self.had_error = true;
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
        err_hdl.report(self.line, &self.pos, &self.message);
    }
}


/// An error that occurred while trying to evaluate the code.
#[derive(Debug, Clone)]
pub struct InterpreterError {
    line: usize,
    message: String,
}

impl InterpreterError {
    /// Initialize an interpreter error.
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            line: token.line,
            message: String::from(message),
        }
    }

    /// Report the error to an error handler.
    pub fn report(&self, err_hdl: &mut ErrorHandler) {
        err_hdl.report(self.line, "", &self.message);
    }
}
