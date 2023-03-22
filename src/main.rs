mod commands;
mod eval;
mod parse;
mod prompt;

use std::fs::OpenOptions;
use std::io::{BufRead, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::commands::{print_stack, run_if_command, CommandError};
use crate::eval::EvalError;
use crate::parse::ParseError;
use crate::{
    eval::{eval_stmts, Env},
    parse::parse_toplevel_from_str,
    prompt::prompt_symbol,
};
use clap::{Parser, Subcommand};
use owo_colors::OwoColorize;
use parse::Statement;
use rustyline::Editor;
use serde::{Deserialize, Serialize};

#[derive(Debug, Parser)]
#[command(name = "git")]
#[command(about = "A programming language for growing programs", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Start a session directly in the CLI.
    Repl,
    /// Start a session over JSON RPC.
    Json,
}

fn repl(interrupted: &Arc<AtomicBool>) {
    println!(
        "{} {}{}",
        "Garden".bold().green(),
        env!("CARGO_PKG_VERSION").bold(),
        ": good programs take time to grow.".bold()
    );
    println!("Type {} if you're new here.", ":help".bold().green(),);

    let mut env = Env::default();
    let mut complete_src = String::new();

    let mut rl: Editor<()> = Editor::new().unwrap();
    // TODO: put this in the home directory rather than the current directory.
    let _ = rl.load_history(".history");

    loop {
        println!();

        match rl.readline(&prompt_symbol(0)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let _ = rl.save_history(".history");

                match run_if_command(&input, &env, &complete_src) {
                    Ok(()) => {
                        continue;
                    }
                    Err(CommandError::NotACommand) => {}
                    Err(CommandError::Abort) => {
                        // Nothing to do, we're in the top level.
                    }
                }

                match read_multiline_syntax(&input, &mut rl) {
                    Ok((src, stmts)) => {
                        complete_src.push_str(&src);
                        complete_src.push('\n');
                        log_src(input).unwrap();

                        match eval_stmts(&stmts, &mut env, &complete_src, &interrupted) {
                            Ok(result) => match result {
                                eval::Value::Void => {}
                                v => {
                                    println!("{}", v)
                                }
                            },
                            Err(EvalError::Aborted) => {}
                            Err(EvalError::UserError(e)) => {
                                println!("{}: {}", "Error".bright_red(), e);
                                print_stack(&env);
                            }
                        }
                    }
                    Err(ParseError::Incomplete(e)) => {
                        println!("Parsing failed: {}", e);
                    }
                    Err(ParseError::OtherError(e)) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(_) => break,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct EvalRequest {
    input: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Response {
    error: bool,
    message: String,
}

fn json_session() {
    let response = Response {
        error: false,
        message: "ready".into(),
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut line = String::new();
    let stdin = std::io::stdin();
    stdin
        .lock()
        .read_line(&mut line)
        .expect("Could not read line");

    match serde_json::from_str::<EvalRequest>(&line) {
        Ok(req) => println!("{:?}", req),
        Err(_) => {
            let response = Response {
                error: true,
                message: format!("Could not parse request: {}", line),
            };
            let serialized = serde_json::to_string(&response).unwrap();
            println!("{}", serialized);
        }
    };
}

fn main() {
    let interrupted = Arc::new(AtomicBool::new(false));

    let i = interrupted.clone();
    ctrlc::set_handler(move || {
        i.store(true, Ordering::SeqCst);
    })
    .expect("Error setting Ctrl-C handler");

    let args = Cli::parse();
    match args.command {
        Commands::Repl => repl(&interrupted),
        Commands::Json => json_session(),
    }
}

fn read_multiline_syntax(
    first_line: &str,
    rl: &mut Editor<()>,
) -> Result<(String, Vec<Statement>), ParseError> {
    let mut src = first_line.to_string();

    loop {
        match parse_toplevel_from_str(&src) {
            Ok(stmts) => {
                return Ok((src, stmts));
            }
            Err(e @ ParseError::Incomplete(_)) => match rl.readline(&prompt_symbol(1)) {
                Ok(input) => {
                    src.push('\n');
                    src.push_str(&input);
                }
                Err(_) => return Err(e),
            },
            Err(e @ ParseError::OtherError(_)) => {
                return Err(e);
            }
        }
    }
}

fn log_src(src: String) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("log.gdn")?;

    write!(file, "\n{}", src)
}
