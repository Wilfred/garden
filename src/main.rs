// Looks like a false positive from rust 1.71.
// TODO: File an issue.
#![allow(suspicious_double_ref_op)]
// This tends to trigger on larger tuples of simple types, and naming
// them would probably be worse for readability.
#![allow(clippy::type_complexity)]
#![warn(clippy::todo)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::str_to_string)]
// Garden is too much of a prototype for this to be an issue.
#![allow(clippy::too_many_arguments)]
// Occurs in WIP code, and it's too obvious to be worth linting
// against.
#![allow(clippy::needless_if)]
// Occurs in WIP code when you plan to match on more cases later on.
#![allow(clippy::single_match)]
// Sometimes explicit if statements are clearer.
#![allow(clippy::collapsible_else_if)]

mod caret_finder;
mod checks;
mod cli_session;
mod colors;
mod commands;
mod completions;
mod diagnostics;
mod env;
mod eval;
mod garden_type;
mod go_to_def;
mod hover;
mod json_session;
mod pos_to_id;
mod prompt;
mod syntax_check;
mod types;
mod values;
mod version;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use eval::eval_up_to;
use go_to_def::print_pos;
use hover::show_type;
use json_session::handle_request;
use owo_colors::OwoColorize as _;

use crate::diagnostics::{format_error_with_stack, format_parse_error, Level};
use crate::env::Env;
use crate::eval::eval_tests;
use crate::eval::{eval_call_main, eval_toplevel_defs, EvalError, Session};
use garden_lang_parser::ast::{SourceString, SyntaxIdGenerator, ToplevelItem};
use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_items, ParseError};

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
    /// Run the tests associated with the definition at this offset,
    /// but give up if the program exceeds a time limit or attempts
    /// I/O.
    SandboxedTest { path: PathBuf },
    /// Run the program specified, calling its main() function, then
    /// run eval-up-to at the position specified and print the result.
    ///
    /// Used for testing the eval-up-to feature.
    TestEvalUpTo { path: PathBuf },
    /// Evaluate all the entries in the .jsonl file as if they were in
    /// a JSON session.
    ///
    /// Lines starting `//` are ignored.
    TestJson { path: PathBuf },
    /// Check the Garden program at the path specified for issues.
    Check {
        path: PathBuf,
        #[clap(long, action)]
        json: bool,
        #[clap(long)]
        override_path: Option<PathBuf>,
    },
    /// Show the type of the expression at the position given.
    ShowType { offset: usize, path: PathBuf },
    /// Show the definition position of the value at the position
    /// given.
    DefinitionPosition {
        offset: usize,
        path: PathBuf,
        #[clap(long)]
        override_path: Option<PathBuf>,
    },
    /// Show possible completions at the position given.
    Complete { offset: usize, path: PathBuf },
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
        Commands::Repl => cli_session::repl(interrupted),
        Commands::Json => json_session::json_session(interrupted),
        Commands::Run { path, arguments } => match std::fs::read(&path) {
            Ok(src_bytes) => run_file(src_bytes, &path, &arguments, interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
        Commands::Check {
            path,
            json,
            override_path,
        } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = from_utf8_or_die(src_bytes, &path);
                let src_path = override_path.unwrap_or(path);
                syntax_check::check(&src_path, &src, json);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::Test { path } => match std::fs::read(&path) {
            Ok(src_bytes) => run_tests_in_file(src_bytes, &path, interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::SandboxedTest { path } => match std::fs::read(&path) {
            Ok(src_bytes) => run_sandboxed_tests_in_file(src_bytes, &path, interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::TestEvalUpTo { path } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = from_utf8_or_die(src_bytes, &path);
                let offset = caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.");
                test_eval_up_to(&src, &path, offset, interrupted);
            }
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
                let src = from_utf8_or_die(src_bytes, &path);
                show_type(&src, &path, offset);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::DefinitionPosition {
            path,
            offset,
            override_path,
        } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = from_utf8_or_die(src_bytes, &path);
                let src_path = override_path.unwrap_or(path);
                print_pos(&src, &src_path, offset);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::Complete { offset, path } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = from_utf8_or_die(src_bytes, &path);
                completions::complete(&src, &path, offset);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::TestJson { path } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = from_utf8_or_die(src_bytes, &path);

                let mut env = Env::default();
                let mut complete_src = String::new();
                let mut session = Session {
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                    trace_exprs: false,
                    stop_at_expr_id: None,
                };

                let json_lines = src
                    .lines()
                    .filter(|line| !line.starts_with("//") && !line.is_empty());
                for line in json_lines {
                    let response = handle_request(line, &mut env, &mut session, &mut complete_src);
                    println!("{}", serde_json::to_string_pretty(&response).unwrap());
                }
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
    }
}

/// Evaluate a garden file, then run eval-up-to and print the result.
fn test_eval_up_to(src: &str, path: &Path, offset: usize, interrupted: Arc<AtomicBool>) {
    let mut env = Env::default();
    let mut session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
        stop_at_expr_id: None,
    };

    let items = parse_toplevel_items_or_die(path, src, &mut env);

    eval_toplevel_defs(&items, &mut env);
    if let Err(e) = eval_call_main(&[], &mut env, &mut session) {
        match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::ResumableError(_, msg) => eprintln!("{}", msg.0),
            EvalError::ReachedTickLimit => eprintln!("Reached the tick limit."),
            EvalError::ForbiddenInSandbox => {
                eprintln!("Tried to execute unsafe code in sandboxed mode.")
            }
        }
        return;
    }

    match eval_up_to(&mut env, &mut session, &items, offset) {
        Some(eval_res) => match eval_res {
            Ok((v, pos)) => println!("{}: {}", pos.as_ide_string(), v.display(&env)),
            Err(e) => match e {
                EvalError::Interrupted => eprintln!("Interrupted."),
                EvalError::ResumableError(_, msg) => eprintln!("{}", msg.0),
                EvalError::ReachedTickLimit => eprintln!("Reached the tick limit."),
                EvalError::ForbiddenInSandbox => {
                    eprintln!("Tried to execute unsafe code in sandboxed mode.")
                }
            },
        },
        None => eprintln!("Could not find anything to execute"),
    }
}

fn from_utf8_or_die(src_bytes: Vec<u8>, path: &Path) -> String {
    match String::from_utf8(src_bytes) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
            std::process::exit(1);
        }
    }
}

fn dump_ast(src_bytes: Vec<u8>, path: &Path) {
    let src = from_utf8_or_die(src_bytes, path);
    let mut id_gen = SyntaxIdGenerator::default();
    let (items, errors) = parse_toplevel_items(path, &src, &mut id_gen);

    for error in errors.into_iter() {
        match error {
            ParseError::Invalid {
                position,
                message: e,
                additional: _,
            } => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString {
                            src: src.clone(),
                            offset: 0
                        }
                    )
                );
            }
            ParseError::Incomplete { message: e, .. } => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        }
    }

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

fn run_sandboxed_tests_in_file(src_bytes: Vec<u8>, path: &Path, interrupted: Arc<AtomicBool>) {
    let src = from_utf8_or_die(src_bytes, path);
    let mut env = Env::default();
    let items = parse_toplevel_items_or_die(path, &src, &mut env);

    let mut session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
        stop_at_expr_id: None,
    };

    // TODO: for real IDE usage we'll want to use the environment of
    // the current session.
    eval_toplevel_defs(&items, &mut env);

    let mut succeeded = false;

    env.tick_limit = Some(1_000);
    env.enforce_sandbox = true;

    match eval_tests(&items, &mut env, &mut session) {
        Ok(summary) => {
            if summary.tests_passed == 1 {
                println!("The test {}.", "passed".green());
            } else {
                println!("All {} test(s) {}.", summary.tests_passed, "passed".green());
            }

            succeeded = true;
        }
        Err(EvalError::ResumableError(position, e)) => {
            eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack.0));
        }
        Err(EvalError::Interrupted) => {
            eprintln!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit) => {
            eprintln!("Reached the tick limit.");
        }
        Err(EvalError::ForbiddenInSandbox) => {
            eprintln!("Tried to execute unsafe code in sandboxed mode.");
        }
    }

    if !succeeded {
        std::process::exit(1);
    }
}

fn run_tests_in_file(src_bytes: Vec<u8>, path: &Path, interrupted: Arc<AtomicBool>) {
    let mut succeeded = false;

    let src = from_utf8_or_die(src_bytes, path);
    let mut env = Env::default();
    let items = parse_toplevel_items_or_die(path, &src, &mut env);

    let mut session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
        stop_at_expr_id: None,
    };

    eval_toplevel_defs(&items, &mut env);

    match eval_tests(&items, &mut env, &mut session) {
        Ok(summary) => {
            if summary.tests_passed == 1 {
                println!("The test {}.", "passed".green());
            } else {
                println!("All {} test(s) {}.", summary.tests_passed, "passed".green());
            }

            // TODO: should we allow tests to keep going
            // after the first failure?
            // TODO: print incremental progress as tests run.
            succeeded = true;
        }
        Err(EvalError::ResumableError(position, e)) => {
            eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack.0));
        }
        Err(EvalError::Interrupted) => {
            eprintln!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit) => {
            eprintln!("Reached the tick limit.");
        }
        Err(EvalError::ForbiddenInSandbox) => {
            eprintln!("Tried to execute unsafe code in sandboxed mode.");
        }
    }

    if !succeeded {
        std::process::exit(1);
    }
}

fn parse_toplevel_items_or_die(path: &Path, src: &str, env: &mut Env) -> Vec<ToplevelItem> {
    let (items, errors) = parse_toplevel_items(path, src, &mut env.id_gen);

    if !errors.is_empty() {
        for error in errors.into_iter() {
            match error {
                ParseError::Invalid {
                    position,
                    message: e,
                    additional: _,
                } => eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString {
                            src: src.to_owned(),
                            offset: 0
                        }
                    )
                ),
                ParseError::Incomplete { message: e, .. } => {
                    eprintln!("Parse error (incomplete input): {}", e.0)
                }
            }
        }

        std::process::exit(1);
    }

    items
}

fn run_file(src_bytes: Vec<u8>, path: &Path, arguments: &[String], interrupted: Arc<AtomicBool>) {
    let src = from_utf8_or_die(src_bytes, path);

    let mut env = Env::default();
    let items = parse_toplevel_items_or_die(path, &src, &mut env);

    let mut session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
        stop_at_expr_id: None,
    };

    eval_toplevel_defs(&items, &mut env);

    match eval_call_main(arguments, &mut env, &mut session) {
        Ok(_) => {}
        Err(EvalError::ResumableError(position, msg)) => {
            eprintln!(
                "{}",
                &format_error_with_stack(&msg, &position, &env.stack.0)
            );
        }
        Err(EvalError::Interrupted) => {
            eprintln!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit) => {
            eprintln!("Reached the tick limit.");
        }
        Err(EvalError::ForbiddenInSandbox) => {
            eprintln!("Tried to execute unsafe code in sandboxed mode.");
        }
    }
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
    fn test_hover() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/hover_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_go_to_def() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/go_to_def_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_completion() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/complete_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_eval_up_to() -> TestResult<()> {
        let mut config =
            TestConfig::new("target/debug/garden", "src/eval_up_to_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_sandboxed_test() -> TestResult<()> {
        let mut config = TestConfig::new(
            "target/debug/garden",
            "src/sandboxed_test_test_files",
            "// ",
        )?;
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
    fn test_json_session() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/json_test_files", "// ")?;
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
