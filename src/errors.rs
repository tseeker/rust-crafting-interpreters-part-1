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
