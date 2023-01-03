use core::fmt;
use std::{error::Error, fmt::Display};

use crate::tokens::{Token, TokenType};

/// The type of an error.
#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    /// The error occurred while parsing the source code.
    Parse,
    /// The error occurred while trying to run the program.
    Runtime,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ErrorKind::Parse => "Parse",
            ErrorKind::Runtime => "Runtime",
        })
    }
}

/// An error that occurred while trying to parse the input once it has been
/// scanned.
#[derive(Debug, Clone)]
pub struct SloxError {
    kind: ErrorKind,
    line: usize,
    pos: String,
    message: String,
}

impl SloxError {
    /// Initialize an error record.
    pub fn new(kind: ErrorKind, token: &Token, message: String) -> Self {
        Self {
            kind,
            line: token.line,
            pos: if token.token_type == TokenType::Eof {
                "at end of input".to_owned()
            } else {
                format!("near '{}'", token.lexeme)
            },
            message,
        }
    }
}

impl Error for SloxError {}

impl Display for SloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] {} error {}: {}",
            self.line, self.kind, self.pos, self.message
        )
    }
}

/// Error handler. Can be used to print error messages; will also retain the
/// current error status.
#[derive(Default, Debug)]
pub struct ErrorHandler {
    had_error: Option<ErrorKind>,
}

impl ErrorHandler {
    /// Check whether this handler reported an error.
    pub fn had_error(&self) -> Option<ErrorKind> {
        self.had_error
    }

    /// Report an error.
    pub fn report(&mut self, error: &SloxError) {
        if self.had_error.is_none() {
            self.had_error = Some(error.kind);
        }
        println!("{error}");
    }
}
