mod ast;
mod errors;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod tokens;

use std::{
    env, fs,
    io::{self, Write},
    process::ExitCode,
};

#[cfg(feature = "dump_ast")]
use ast::AstDumper;
use errors::{ErrorHandler, SloxResult};
use interpreter::{evaluate, Value};
use parser::Parser;
use resolver::resolve_variables;
use scanner::Scanner;

/// Execute a script.
fn run(source: String) -> SloxResult<()> {
    let mut error_handler = ErrorHandler::default();

    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens(&mut error_handler)?;
    #[cfg(feature = "dump_tokens")]
    for token in tokens.iter() {
        println!("{:#?}", token);
    }

    let parser = Parser::new(tokens);
    let ast = parser.parse(&mut error_handler)?;
    #[cfg(feature = "dump_ast")]
    println!("AST generated ! {}", ast.dump());

    let resolved_vars = error_handler.report_or_continue(resolve_variables(&ast))?;
    let value = error_handler.report_or_continue(evaluate(&ast, resolved_vars))?;
    match value {
        Value::Nil => (),
        _ => println!("{value}"),
    }
    Ok(())
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
fn run_file(file: &str) -> SloxResult<()> {
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
        match run_file(&args[0]) {
            Ok(()) => Ok(()),
            Err(err) => Err(ExitCode::from(err.kind().exit_code())),
        }
    } else {
        println!("Usage: slox [script]");
        Err(ExitCode::from(1))
    }
}
