mod diag;
mod scanner;
mod ast;
mod parser;
mod interpreter;
mod prelude;

use std::fs;
use std::path::{PathBuf, Path};

use structopt::StructOpt;
use rustyline::{error::ReadlineError, Editor};

use crate::interpreter::{Interpreter, Value};

#[derive(Debug, StructOpt)]
#[structopt(name = "lox", about = "An implementation of the lox programming language")]
struct CmdArgs {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let CmdArgs {input} = CmdArgs::from_args();

    match input {
        Some(input) => run_file(&input),
        None => run_prompt(),
    }
}

fn run_file(input: &Path) -> anyhow::Result<()> {
    let source_code = fs::read(input)?;
    let mut interpreter = Interpreter::with_prelude();
    run(&mut interpreter, &source_code)
}

fn run_prompt() -> anyhow::Result<()> {
    let mut interpreter = Interpreter::with_prelude();
    let mut reader = Editor::<()>::new();

    loop {
        let readline = reader.readline("> ");
        match readline {
            Ok(line) => {
                reader.add_history_entry(line.as_str());
                match run(&mut interpreter, line.as_bytes()) {
                    Ok(()) => {},
                    Err(err) => println!("{}", err),
                }
            },

            Err(ReadlineError::Interrupted) => {
                println!("^C");
            },

            Err(ReadlineError::Eof) => {
                break;
            },

            Err(err) => Err(err)?,
        }
    }

    Ok(())
}

fn run(interpreter: &mut Interpreter, source_code: &[u8]) -> anyhow::Result<()> {
    let tokens = scanner::scan_tokens(source_code)?;
    let program = parser::parse_program(&tokens)?;

    let value = interpreter.eval(program)?;
    match value {
        Value::Nil => {},
        _ => println!("{}", value),
    }

    Ok(())
}
