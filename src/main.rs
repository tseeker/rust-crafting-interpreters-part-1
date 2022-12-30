use std::{env, fs, io::{self, Write}, process::ExitCode};

/// Error handler. Can be used to print error messages; will also retain the
/// current error status.
#[derive(Default, Debug)]
struct ErrorHandler {
    had_error: bool
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

/// Execute a script.
fn run(_source: String) -> ErrorHandler {
    let error_handler = ErrorHandler::default();
    /*
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    for token in tokens {
        println!("{}", token);
    }
    */
    error_handler
}

/// Run the REPL.
fn run_prompt() {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("slox> ");
        stdout.flush().unwrap();
        let n_read = stdin.read_line(&mut buffer).expect("Failed to read from stdin");
        let _ = match n_read {
            0 => return,
            _ => run(buffer.clone()),
        };
    }
}

/// Load a file and run the script it contains.
fn run_file(file: &str) -> ErrorHandler {
    let contents = fs::read_to_string(file).expect(&format!("Could not load {}", file));
    run(contents)
}

/// Main program. Either load a script from a file, or start the REPL.
fn main() -> Result<(), ExitCode> {
    let args: Vec<String> = env::args().skip(1).collect();
    let n_args = args.len();
    if n_args == 0 {
        run_prompt()
    } else if n_args == 1 {
        if run_file(&args[0]).had_error() {
            return Err(ExitCode::from(65));
        }
    } else {
        println!("Usage: slox [script]");
    }
    Ok(())
}
