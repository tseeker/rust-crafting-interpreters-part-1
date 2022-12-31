mod ast;
mod errors;
mod interpreter;
mod parser;
mod scanner;
mod tokens;

use std::{
    env, fs,
    io::{self, Write},
    process::ExitCode,
};

#[cfg(feature = "dump_ast")]
use ast::AstDumper;
use errors::{ErrorHandler, ErrorType};
use interpreter::evaluate;
use parser::Parser;
use scanner::Scanner;

/// Execute a script.
fn run(source: String) -> ErrorHandler {
    let mut error_handler = ErrorHandler::default();

    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens(&mut error_handler);

    #[cfg(feature = "dump_tokens")]
    for token in tokens.iter() {
        println!("{:#?}", token);
    }

    let parser = Parser::new(tokens);
    match parser.parse(&mut error_handler) {
        None => (),
        Some(ast) => {
            #[cfg(feature = "dump_ast")]
            println!("AST generated ! {}", ast.dump());
            evaluate(&mut error_handler, &ast);
        }
    }

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
        buffer.clear();
        let n_read = stdin
            .read_line(&mut buffer)
            .expect("Failed to read from stdin");
        let _ = match n_read {
            0 => return,
            _ => run(buffer.clone()),
        };
    }
}

/// Load a file and run the script it contains.
fn run_file(file: &str) -> ErrorHandler {
    let contents = fs::read_to_string(file).unwrap_or_else(|_| panic!("Could not load {}", file));
    run(contents)
}

/// Main program. Either load a script from a file, or start the REPL.
fn main() -> Result<(), ExitCode> {
    let args: Vec<String> = env::args().skip(1).collect();
    let n_args = args.len();
    if n_args == 0 {
        run_prompt();
        Ok(())
    } else if n_args == 1 {
        match run_file(&args[0]).had_error() {
            None => Ok(()),
            Some(ErrorType::Parse) => Err(ExitCode::from(65)),
            Some(ErrorType::Runtime) => Err(ExitCode::from(70)),
        }
    } else {
        println!("Usage: slox [script]");
        Err(ExitCode::from(1))
    }
}
