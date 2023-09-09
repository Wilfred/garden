// Looks like a false positive from rust 1.71.
// TODO: File an issue.
#![allow(suspicious_double_ref_op)]

mod ast;
mod colors;
mod commands;
mod eval;
mod interactive_session;
mod json_session;
mod lex;
mod parse;
mod prompt;

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::{Parser, Subcommand};
use eval::{eval_def_or_exprs, Env, EvalError, Session};
use parse::parse_def_or_expr_from_str;

use crate::eval::{escape_string_literal, ErrorMessage};
use crate::parse::{format_error, format_error_with_stack, format_parse_error};

#[derive(Debug, Parser)]
#[command(author, version, name="Garden", about = "A programming language for growing programs", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

// TODO: if a user accidentally writes `garden foo.gdn`, suggest
// `garden run foo.gdn`.
#[derive(Debug, Subcommand)]
enum Commands {
    /// Start a session directly in the CLI.
    Repl,
    /// Start a session over JSON RPC.
    Json,
    /// Print an example JSON request that's valid in JSON sessions.
    JsonExample,
    /// Execute a Garden program at the path specified. Additional
    /// arguments are passed as a list to the `main()` function.
    Run {
        path: PathBuf,
        arguments: Vec<String>,
    },
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
        Commands::Repl => interactive_session::repl(&interrupted),
        Commands::Json => json_session::json_session(&interrupted),
        Commands::Run { path, arguments } => match std::fs::read(&path) {
            Ok(src_bytes) => run_file(src_bytes, &path, &arguments, &interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
    }
}

fn run_file(
    src_bytes: Vec<u8>,
    path: &PathBuf,
    arguments: &[String],
    interrupted: &Arc<AtomicBool>,
) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_def_or_expr_from_str(path, &src) {
            Ok(exprs) => {
                let mut env = Env::default();
                let mut session = Session {
                    history: src.clone(),
                    interrupted,
                    has_attached_stdout: true,
                };

                // TODO: files should only contain defs, not
                // expressions. Ignore the expressions.
                match eval_def_or_exprs(&exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, e)) => {
                        eprintln!("{}", &format_error(&e, &position, &src));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                    Err(EvalError::Stop(_)) => {
                        eprintln!("Error (stopped)");
                    }
                }

                let call_src = call_to_main_src(arguments);
                let call_exprs =
                    parse_def_or_expr_from_str(&PathBuf::from("__main_fun__"), &call_src).unwrap();
                match eval_def_or_exprs(&call_exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, msg)) => {
                        eprintln!("{}", &format_error_with_stack(&msg, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                    Err(EvalError::Stop(_)) => {
                        eprintln!("Error (stopped)");
                    }
                }
            }
            Err(parse::ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        &src
                    )
                );
            }
            Err(parse::ParseError::Incomplete(e)) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}

fn call_to_main_src(cli_args: &[String]) -> String {
    let arg_literals: Vec<_> = cli_args.iter().map(|s| escape_string_literal(s)).collect();
    format!("main([{}]);", arg_literals.join(", "))
}
