use std::{env, fs, io::{self, Write}};

/// Execute a script.
fn run(_source: String) {
    todo!();
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
        match n_read {
            0 => return,
            _ => run(buffer.clone()),
        }
    }
}

/// Load a file and run the script it contains.
fn run_file(file: &str) {
    let contents = fs::read_to_string(file).expect(&format!("Could not load {}", file));
    run(contents)
}

/// Main program. Either load a script from a file, or start the REPL.
fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let n_args = args.len();
    if n_args == 0 {
        run_prompt()
    } else if n_args == 1 {
        run_file(&args[0]);
    } else {
        println!("Usage: slox [script]");
    }
}
