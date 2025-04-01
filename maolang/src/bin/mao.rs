//! Main Mao CLI runtime >:)

use std::{env, fs::File, io::Read, process};

use maolang_core::{
    interp::Interpretter,
    parser::{Parser, ast::ParseError},
    tokenizer::{Tokenizable, TokenizeError},
};
use rand::SeedableRng;
use rand_chacha::ChaCha8Rng;
use sha2::{Digest, Sha256};

/// The current version
const VERSION: &str = "0.1.1";

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help();
        process::exit(0);
    }

    match args[1].as_str() {
        "--help" | "-h" => print_help(),
        "--version" | "-v" => print_version(),
        file_path => run_file(file_path),
    }
}

/// Prints some "useful" help info
fn print_help() {
    println!("Mao Interpreter {}", VERSION);
    println!("Usage: who knows?");
    println!();
    println!("Options:");
    println!("  -h, --help       find out what this does");
    println!("  -v, --version    I'm not telling you the rules");
    println!();
    println!("Provide a file to interpret Mao code. Maybe");
}

/// Prints the version
fn print_version() {
    println!("Mao Interpreter {}", VERSION);
}

/// Runs a file for Mao interpretation
fn run_file(file_path: &str) {
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            process::exit(1);
        }
    };

    let mut buf = String::new();
    if let Err(err) = file.read_to_string(&mut buf) {
        eprintln!("Error reading file: {}", err);
        process::exit(1);
    }

    // Prevents people from being able to insert whitespace until their syntax is right
    let no_ws_buf: Vec<_> = buf
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .map(|ch| ch as u8)
        .collect();

    let mut hasher = Sha256::new();
    hasher.update(&no_ws_buf);
    let result = hasher.finalize();

    let mut seed = [0u8; 32];

    for (idx, s) in seed.iter_mut().enumerate() {
        *s = result[idx]
    }

    let mut rng = ChaCha8Rng::from_seed(seed);

    let tokens = match buf.tokenize(&mut rng) {
        Ok(tokens) => tokens,
        Err(err) => {
            let TokenizeError { message, line, col } = err;
            eprintln!("{message}");
            eprintln!(" -> {}:{}:{} ", file_path, line, col);
            if let Some(line) = buf.lines().nth(line - 1) {
                eprintln!(" | {line}");
                eprintln!(" | {}^", "-".repeat(col - 1));
            }
            process::exit(1);
        }
    };

    let mut parser = Parser::from_rng(&mut rng).with_tokens(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            let ParseError {
                message,
                len,
                col,
                line,
            } = err;

            eprintln!("{message}");
            eprintln!(" -> {}:{}:{} ", file_path, line, col);
            if let Some(line) = buf.lines().nth(line - 1) {
                eprintln!(" | {line}");
                eprintln!(" | {}{}", " ".repeat(col - len), "~".repeat(len));
            }

            process::exit(1);
        }
    };

    let mut interp = Interpretter::default();
    for node in ast {
        if let Err(err) = interp.eval(&node) {
            eprintln!("Runtime error occurred: {}", err);
            process::exit(1);
        }
    }
}
