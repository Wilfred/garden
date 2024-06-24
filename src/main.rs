// Looks like a false positive from rust 1.71.
// TODO: File an issue.
#![allow(suspicious_double_ref_op)]
#![warn(clippy::todo)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::str_to_string)]
// Garden is too much of a prototype for this to be an issue.
#![allow(clippy::too_many_arguments)]
// Occurs in WIP code, and it's too obvious to be worth linting
// against.
#![allow(clippy::needless_if)]

mod checks;
mod cli_session;
mod colors;
mod commands;
mod diagnostics;
mod env;
mod eval;
mod garden_type;
mod hover;
mod json_session;
mod prompt;
mod syntax_check;
mod types;
mod values;
mod version;
mod visitor;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use hover::show_type;
use owo_colors::OwoColorize as _;

use crate::diagnostics::{format_error_with_stack, format_parse_error, Level};
use crate::env::Env;
use crate::eval::eval_toplevel_tests;
use crate::eval::{eval_all_toplevel_items, eval_toplevel_defs, EvalError, Session};
use crate::values::escape_string_literal;
use garden_lang_parser::ast::{SourceString, ToplevelItem};
use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_item, parse_toplevel_items, ParseError};

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
    /// Check the Garden program at the path specified for issues.
    Check {
        path: PathBuf,
        #[clap(long, action)]
        json: bool,
    },
    /// Show the type of the expression at the position given.
    ShowType { offset: usize, path: PathBuf },
    /// Parse the Garden program at the path specified and print the
    /// AST.
    DumpAst { path: PathBuf },
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
        Commands::Check { path, json } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = String::from_utf8(src_bytes).expect("TODO: handle invalid bytes");
                syntax_check::check(&path, &src, json);
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
        Commands::DumpAst { path } => match std::fs::read(&path) {
            Ok(src_bytes) => dump_ast(src_bytes, &path),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::ShowType { path, offset } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = String::from_utf8(src_bytes).expect("TODO: handle invalid bytes");
                show_type(&src, &path, offset);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
    }
}

fn dump_ast(src_bytes: Vec<u8>, path: &Path) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                for item in items {
                    match item {
                        ToplevelItem::Def(d) => {
                            println!("{:#?}", d.2);
                        }
                        ToplevelItem::Expr(e) => {
                            println!("{:#?}", e.0.expr_);
                        }
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}

// TODO: Much of this logic is duplicated with run_file.
fn run_tests_in_file(src_bytes: Vec<u8>, path: &Path, interrupted: &Arc<AtomicBool>) {
    let mut succeeded = false;

    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                    trace_exprs: false,
                };

                eval_toplevel_defs(&items, &mut env);

                match eval_toplevel_tests(&items, &mut env, &mut session) {
                    Ok(summary) => {
                        // TODO: should we allow tests to keep going
                        // after the first failure?
                        // TODO: print incremental progress as tests run.
                        println!("All {} test(s) {}.", summary.tests_passed, "passed".green());
                        succeeded = true;
                    }
                    Err(EvalError::ResumableError(position, e)) => {
                        eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }

    if !succeeded {
        std::process::exit(1);
    }
}

fn run_file(src_bytes: Vec<u8>, path: &Path, arguments: &[String], interrupted: &Arc<AtomicBool>) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                    trace_exprs: false,
                };

                eval_toplevel_defs(&items, &mut env);

                let call_src = call_to_main_src(arguments);
                let call_exprs =
                    vec![parse_toplevel_item(&PathBuf::from("__main_fun__"), &call_src).unwrap()];
                match eval_all_toplevel_items(&call_exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, msg)) => {
                        eprintln!("{}", &format_error_with_stack(&msg, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
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
    use assert_cmd::prelude::*;
    use std::process::Command;

    use goldentests::{TestConfig, TestResult};

    #[test]
    fn test_parser() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/parser_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_checks() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/check_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_runtime() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/runtime_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_prelude_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test").arg("src/prelude.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_prelude_check() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("check").arg("src/prelude.gdn");
        cmd.assert().success();
    }
}
