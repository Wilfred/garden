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
// I'm not convinced it's always clearer to use match with cmp.
#![allow(clippy::comparison_chain)]
// Garden has much bigger perf issues, let's not worry about this.
#![allow(clippy::expect_fun_call)]
// Distracting when refactoring multithreaded code, and ultimately
// harmless.
#![allow(clippy::arc_with_non_send_sync)]

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
mod rename;
mod syntax_check;
mod types;
mod values;
mod version;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;
use std::time::Instant;

use clap::{Parser, Subcommand};
use eval::eval_up_to;
use go_to_def::print_pos;
use hover::show_type;
use json_session::handle_request;

use crate::diagnostics::{format_diagnostic, format_error_with_stack, Level};
use crate::env::Env;
use crate::eval::eval_tests;
use crate::eval::{eval_call_main, load_toplevel_items, EvalError, Session};
use garden_lang_parser::ast::{Definition_, SourceString, SyntaxIdGenerator, ToplevelItem};
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
    SandboxedTest {
        path: PathBuf,
        offset: Option<usize>,
    },
    /// Rename the local variable at this offset to the new name
    /// specified.
    Rename {
        path: PathBuf,
        offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
        #[clap(long)]
        new_name: String,
    },
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
    ShowType {
        path: PathBuf,
        offset: Option<usize>,
    },
    /// Show the definition position of the value at the position
    /// given.
    DefinitionPosition {
        path: PathBuf,
        offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
    },
    /// Show possible completions at the position given.
    Complete {
        path: PathBuf,
        offset: Option<usize>,
    },
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

    let mut thread_handles: Vec<JoinHandle<()>> = vec![];

    let args = Cli::parse();
    match args.command {
        Commands::Repl => cli_session::repl(interrupted),
        Commands::Json => json_session::json_session(interrupted, &mut thread_handles),
        Commands::Run { path, arguments } => {
            let src = read_utf8_or_die(&path);
            run_file(&src, &path, &arguments, interrupted)
        }
        Commands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
        Commands::Check {
            path,
            json,
            override_path,
        } => {
            let src = read_utf8_or_die(&path);
            let src_path = override_path.unwrap_or(path);
            syntax_check::check(&src_path, &src, json)
        }
        Commands::Test { path } => {
            let src = read_utf8_or_die(&path);
            run_tests_in_file(&src, &path, interrupted)
        }
        Commands::SandboxedTest { path, offset } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            run_sandboxed_tests_in_file(&src, &path, offset, interrupted)
        }
        Commands::TestEvalUpTo { path } => {
            let src = read_utf8_or_die(&path);
            let offset = caret_finder::find_caret_offset(&src)
                .expect("Could not find comment containing `^` in source.");
            test_eval_up_to(&src, &path, offset, interrupted);
        }
        Commands::DumpAst { path } => {
            let src = read_utf8_or_die(&path);
            dump_ast(&src, &path)
        }
        Commands::ShowType { path, offset } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            show_type(&src, &path, offset)
        }
        Commands::DefinitionPosition {
            path,
            offset,
            override_path,
        } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });

            let src_path = override_path.unwrap_or(path);
            print_pos(&src, &src_path, offset)
        }
        Commands::Complete { offset, path } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            completions::complete(&src, &path, offset);
        }
        Commands::TestJson { path } => {
            let src = read_utf8_or_die(&path);

            let env = Arc::new(Mutex::new(Env::default()));
            let session = Arc::new(Mutex::new(Session {
                interrupted: Arc::clone(&interrupted),
                has_attached_stdout: true,
                start_time: Instant::now(),
                trace_exprs: false,
            }));

            let json_lines = src
                .lines()
                .filter(|line| !line.starts_with("//") && !line.is_empty());
            for line in json_lines {
                handle_request(
                    line,
                    true,
                    Arc::clone(&env),
                    Arc::clone(&session),
                    Arc::clone(&interrupted),
                    &mut thread_handles,
                );
            }
        }
        Commands::Rename {
            path,
            new_name,
            offset,
            override_path,
        } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });

            let src_path = override_path.unwrap_or(path);
            rename::rename(&src, &src_path, offset, &new_name)
        }
    }

    for handle in thread_handles {
        handle.join().unwrap();
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
    };

    let items = parse_toplevel_items_or_die(path, src, &mut env);

    load_toplevel_items(&items, &mut env);
    if let Err(e) = eval_call_main(&[], &mut env, &session) {
        match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::ResumableError(_, msg) => eprintln!("{}", msg.0),
            EvalError::AssertionFailed(_) => eprintln!("Assertion failed"),
            EvalError::ReachedTickLimit => eprintln!("Reached the tick limit."),
            EvalError::ForbiddenInSandbox(_) => {
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
                EvalError::AssertionFailed(_) => eprintln!("Assertion failed"),
                EvalError::ReachedTickLimit => eprintln!("Reached the tick limit."),
                EvalError::ForbiddenInSandbox(_) => {
                    eprintln!("Tried to execute unsafe code in sandboxed mode.")
                }
            },
        },
        None => eprintln!("Could not find anything to execute"),
    }
}

fn read_utf8_or_die(path: &Path) -> String {
    match std::fs::read(path) {
        Ok(src_bytes) => from_utf8_or_die(src_bytes, path),
        Err(e) => {
            eprintln!("Error: Could not read file {}: {}", path.display(), e);
            std::process::exit(1);
        }
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

fn dump_ast(src: &str, path: &Path) {
    let mut id_gen = SyntaxIdGenerator::default();
    let (items, errors) = parse_toplevel_items(path, src, &mut id_gen);

    for error in errors.into_iter() {
        match error {
            ParseError::Invalid {
                position,
                message: e,
                additional: _,
            } => {
                eprintln!(
                    "{}",
                    &format_diagnostic(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString {
                            src: src.to_owned(),
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

fn run_sandboxed_tests_in_file(
    src: &str,
    path: &Path,
    offset: usize,
    interrupted: Arc<AtomicBool>,
) {
    let mut env = Env::default();
    let (items, errors) = parse_toplevel_items(path, src, &mut env.id_gen);
    if !errors.is_empty() {
        println!("Parse error");
        return;
    }

    let session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
    };

    // TODO: for real IDE usage we'll want to use the environment of
    // the current session.
    load_toplevel_items(&items, &mut env);

    // TODO: allow users to choose this value.
    //
    // Currently it's chosen by bumping it if writing a sample file
    // that hits the limit.
    env.tick_limit = Some(10_000);
    env.enforce_sandbox = true;

    let mut contained_items = vec![];
    for item in items.iter() {
        let ToplevelItem::Def(def) = &item else {
            continue;
        };
        if def.1.contains_offset(offset) && matches!(def.2, Definition_::Test(_)) {
            contained_items.push(item.clone());
        }
    }

    let relevant_items = if contained_items.is_empty() {
        items
    } else {
        contained_items
    };

    match eval_tests(&relevant_items, &mut env, &session) {
        Ok(summary) => {
            let mut num_failed = 0;
            let mut num_errored = 0;
            let mut num_sandboxed = 0;
            let mut num_timed_out = 0;

            for err in &summary.tests_failed {
                match err {
                    EvalError::Interrupted => num_errored += 1,
                    EvalError::ResumableError(_, _) => num_errored += 1,
                    EvalError::AssertionFailed(_) => num_failed += 1,
                    EvalError::ReachedTickLimit => num_timed_out += 1,
                    EvalError::ForbiddenInSandbox(_) => num_sandboxed += 1,
                }
            }

            let mut parts = vec![];
            if summary.tests_passed > 0 {
                parts.push(format!("{} passed", summary.tests_passed));
            }
            if num_failed > 0 {
                parts.push(format!("{} failed", num_failed));
            }
            if num_errored > 0 {
                parts.push(format!("{} errored", num_errored));
            }
            if num_timed_out > 0 {
                parts.push(format!("{} timed out", num_timed_out));
            }
            if num_sandboxed > 0 {
                parts.push(format!("{} sandboxed", num_sandboxed));
            }

            if parts.is_empty() {
                parts.push("No tests".to_owned());
            }

            println!("{}", parts.join(", "));
        }
        Err(EvalError::ResumableError(_, _)) => {
            println!("Error")
        }
        Err(EvalError::AssertionFailed(_)) => {
            println!("Failed")
        }
        Err(EvalError::Interrupted) => {
            println!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit) => {
            println!("Timeout");
        }
        Err(EvalError::ForbiddenInSandbox(_)) => {
            println!("Sandbox");
        }
    }
}

fn run_tests_in_file(src: &str, path: &Path, interrupted: Arc<AtomicBool>) {
    let mut succeeded = false;

    let mut env = Env::default();
    let items = parse_toplevel_items_or_die(path, src, &mut env);

    let session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
    };

    load_toplevel_items(&items, &mut env);

    match eval_tests(&items, &mut env, &session) {
        Ok(summary) => {
            if summary.tests_passed == 0 && summary.tests_failed.is_empty() {
                println!("No tests found.");
            } else {
                println!(
                    "{} passed, {} failed.",
                    summary.tests_passed,
                    summary.tests_failed.len()
                );
            }

            // TODO: support printing back traces from every test failure.
            // TODO: print incremental progress as tests run.
            succeeded = true;
        }
        Err(EvalError::ResumableError(position, e)) => {
            eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack.0));
        }
        Err(EvalError::AssertionFailed(position)) => {
            let msg = ErrorMessage("Assertion failed".to_owned());
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
        Err(EvalError::ForbiddenInSandbox(position)) => {
            eprintln!(
                "{}: Tried to execute unsafe code in sandboxed mode.",
                position.as_ide_string()
            );
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
                    &format_diagnostic(
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

fn run_file(src: &str, path: &Path, arguments: &[String], interrupted: Arc<AtomicBool>) {
    let mut env = Env::default();
    let items = parse_toplevel_items_or_die(path, src, &mut env);

    let session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
    };

    load_toplevel_items(&items, &mut env);

    match eval_call_main(arguments, &mut env, &session) {
        Ok(_) => {}
        Err(EvalError::ResumableError(position, msg)) => {
            eprintln!(
                "{}",
                &format_error_with_stack(&msg, &position, &env.stack.0)
            );
        }
        Err(EvalError::AssertionFailed(position)) => {
            let msg = ErrorMessage("Assertion failed".to_owned());
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
        Err(EvalError::ForbiddenInSandbox(position)) => {
            eprintln!(
                "{}: Tried to execute unsafe code in sandboxed mode.",
                position.as_ide_string()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_cmd::prelude::*;
    use std::process::Command;

    use goldentests::{TestConfig, TestResult};

    #[test]
    fn run_test_files() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/test_files", "// ")?;
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
