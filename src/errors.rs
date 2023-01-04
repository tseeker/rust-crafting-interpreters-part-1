use core::fmt;
use std::{error::Error, fmt::Display};

use crate::tokens::{Token, TokenType};

/// The type of an error.
#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    /// The error occurred while scanning the source code.
    Scan,
    /// The error occurred while parsing the source code.
    Parse,
    /// The error occurred while trying to run the program.
    Runtime,
}

impl ErrorKind {
    /// Program exit code based on the kind of error.
    pub fn exit_code(self) -> u8 {
        match self {
            ErrorKind::Scan | ErrorKind::Parse => 65,
            ErrorKind::Runtime => 70,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ErrorKind::Scan | ErrorKind::Parse => "Parse",
            ErrorKind::Runtime => "Runtime",
        })
    }
}

/// An error that occurred while trying to parse the input once it has been
/// scanned.
#[derive(Debug, Clone)]
pub struct SloxError {
    kind: ErrorKind,
    line: Option<usize>,
    pos: String,
    message: String,
}

/// Return type for SLox functions.
pub type SloxResult<T> = Result<T, SloxError>;

impl SloxError {
    /// Initialize a record for a scanner error.
    pub fn scanner_error(line: usize, ch: Option<char>, message: String) -> Self {
        Self {
            kind: ErrorKind::Scan,
            line: Some(line),
            pos: match ch {
                None => " at end of input".to_owned(),
                Some(ch) => format!(" near {:?}", ch),
            },
            message,
        }
    }

    /// Initialize an error record using a token.
    pub fn with_token(kind: ErrorKind, token: &Token, message: String) -> Self {
        Self {
            kind,
            line: Some(token.line),
            pos: if token.token_type == TokenType::Eof {
                " at end of input".to_owned()
            } else {
                format!(" near '{}'", token.lexeme)
            },
            message,
        }
    }

    /// Create an error indicating a stage failed.
    fn stage_failed(kind: ErrorKind) -> Self {
        Self {
            kind,
            line: None,
            pos: "".to_owned(),
            message: "exiting...".to_owned(),
        }
    }

    /// Return the type of error
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl Error for SloxError {}

impl Display for SloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = match self.line {
            None => "".to_owned(),
            Some(l) => format!("[line {}] ", l),
        };
        write!(
            f,
            "{}{} error{}: {}",
            line, self.kind, self.pos, self.message
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
    /// Report an error.
    pub fn report(&mut self, error: SloxError) {
        self.had_error = Some(error.kind);
        println!("{error}");
    }

    /// Transmit the last value returned by some component, or report its error
    /// and generate the final error.
    pub fn report_or_continue<T>(&mut self, result: SloxResult<T>) -> SloxResult<T> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => {
                let fe = SloxError::stage_failed(e.kind);
                self.report(e);
                println!("{fe}");
                Err(fe)
            }
        }
    }

    /// Generate an error that corresponds to the last error encountered.
    pub fn final_error<T>(&self, result: T) -> SloxResult<T> {
        if let Some(err_kind) = self.had_error {
            let err = SloxError::stage_failed(err_kind);
            println!("{err}");
            Err(err)
        } else {
            Ok(result)
        }
    }
}
