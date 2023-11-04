// Looks like a false positive from rust 1.71.
// TODO: File an issue.
#![allow(suspicious_double_ref_op)]
#![warn(clippy::todo)]
#![warn(clippy::dbg_macro)]

mod ast;
mod check;
mod cli_session;
mod colors;
mod commands;
mod diagnostics;
mod env;
mod eval;
mod json_session;
mod lex;
mod parse;
mod prompt;
mod values;
mod version;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use env::Env;
use eval::{eval_toplevel_defs, eval_toplevel_items, EvalError, Session};
use parse::{parse_toplevel_item, parse_toplevel_items};

use crate::ast::SourceString;
use crate::diagnostics::{format_error_with_stack, format_parse_error};
use crate::eval::{eval_toplevel_tests, ErrorMessage};
use crate::values::escape_string_literal;

#[derive(Debug, Parser)]
#[command(author, version=version::VERSION.as_str(), name="Garden", about = "A programming language for growing programs", long_about = None)]
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
    /// Run all the tests in the Garden program at the path specified.
    Test { path: PathBuf },
    /// Check the Garden program at the path specified for syntax
    /// issues.
    Check { path: PathBuf },
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
        Commands::Repl => cli_session::repl(&interrupted),
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
        Commands::Check { path } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = String::from_utf8(src_bytes).expect("TODO: handle invalid bytes");
                check::check(&path, &src);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::Test { path } => match std::fs::read(&path) {
            Ok(src_bytes) => run_tests_in_file(src_bytes, &path, &interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
    }
}

// TODO: Much of this logic is duplicated with run_file.
fn run_tests_in_file(src_bytes: Vec<u8>, path: &Path, interrupted: &Arc<AtomicBool>) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    history: src.clone(),
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                };

                eval_toplevel_defs(&items, &mut env);

                match eval_toplevel_tests(&items, &mut env, &mut session) {
                    Ok(summary) => {
                        // TODO: should we allow tests to keep going
                        // after the first failure?
                        // TODO: print incremental progress as tests run.
                        println!("All {} test(s) passed.", summary.tests_passed);
                    }
                    Err(EvalError::ResumableError(position, e)) => {
                        eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
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
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(parse::ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}

fn run_file(src_bytes: Vec<u8>, path: &Path, arguments: &[String], interrupted: &Arc<AtomicBool>) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    history: src.clone(),
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                };

                eval_toplevel_defs(&items, &mut env);

                let call_src = call_to_main_src(arguments);
                let call_exprs =
                    vec![parse_toplevel_item(&PathBuf::from("__main_fun__"), &call_src).unwrap()];
                match eval_toplevel_items(&call_exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, msg)) => {
                        eprintln!("{}", &format_error_with_stack(&msg, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
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
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(parse::ParseError::Incomplete { message: e, .. }) => {
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

#[cfg(test)]
mod tests {
    use assert_cmd::Command;

    #[test]
    fn test_hello_world() {
        let mut cmd = Command::cargo_bin("garden").unwrap();

        cmd.arg("run").arg("sample_programs/hello_world.gdn");
        cmd.assert().success().stdout("Hello, World!\n");
    }
}
