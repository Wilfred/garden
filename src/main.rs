// This tends to trigger on larger tuples of simple types, and naming
// them would probably be worse for readability.
#![allow(clippy::type_complexity)]
// Catch unfinished code.
#![warn(clippy::todo)]
// Catch unfinished code.
#![warn(clippy::dbg_macro)]
// Preferred style of making strings owned.
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
// TODO: fix this.
#![allow(clippy::result_large_err)]
// TODO: fix this.
#![allow(clippy::large_enum_variant)]

mod caret_finder;
mod checks;
mod cli_session;
mod colors;
mod commands;
mod completions;
mod destructure;
mod diagnostics;
mod env;
mod eval;
mod extract_function;
mod extract_variable;
mod garden_type;
mod go_to_def;
mod hover;
mod json_session;
mod namespaces;
mod parser;
mod pos_to_id;
mod prompt;
mod rename;
mod syntax_check;
mod test_runner;
mod types;
mod values;
mod version;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use eval::{eval_up_to, EvalUpToErr, StdoutMode};
use go_to_def::print_pos;
use hover::show_type;
use json_session::{handle_request, start_eval_thread};
use parser::vfs::Vfs;
use test_runner::{run_sandboxed_tests_in_file, run_tests_in_file};

use crate::diagnostics::{format_diagnostic, format_error_with_stack, Level};
use crate::env::Env;
use crate::eval::{eval_toplevel_items, load_toplevel_items, EvalError, Session};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::{parse_toplevel_items, ParseError};

pub(crate) const BAD_CLI_REQUEST_EXIT_CODE: i32 = 10;

#[derive(Debug, Parser)]
#[command(author, version=version::VERSION.as_str(), name="Garden", about = "A programming language for growing programs", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: CliCommands,
}

// TODO: if a user accidentally writes `garden foo.gdn`, suggest
// `garden run foo.gdn`.
#[derive(Debug, Subcommand)]
enum CliCommands {
    /// Start a session directly in the CLI.
    Repl,
    /// Start a session over JSON RPC.
    Json,
    /// Print an example JSON request that's valid in JSON sessions.
    JsonExample,
    /// Execute a Garden program at the path specified.
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
    /// Replace the expression at this offset with a variable called
    /// `name`, and use the expression as the variables's definition.
    ExtractVariable {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
        #[clap(long)]
        name: String,
    },
    /// Replace the expression at this offset with a function called
    /// `name`, and use the expression as the function's body.
    ExtractFunction {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
        #[clap(long)]
        name: String,
    },
    /// Wrap the expression at the offset specified in a `match`.
    Destructure {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
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
        /// Treat the file as if it was at this path.
        ///
        /// This allows you to write the state of a file to somewhere
        /// in /tmp, and check it as if it was in the original path,
        /// without requiring the user to save.
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

    let args = Cli::parse();
    match args.command {
        CliCommands::Repl => cli_session::repl(interrupted),
        CliCommands::Json => json_session::json_session(interrupted),
        CliCommands::Run { path, arguments } => {
            let src = read_utf8_or_die(&path);
            run_file(&src, &path, &arguments, interrupted)
        }
        CliCommands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
        CliCommands::Check {
            path,
            json,
            override_path,
        } => {
            let src = read_utf8_or_die(&path);
            let src_path = override_path.unwrap_or(path);
            syntax_check::check(&src_path, &src, json)
        }
        CliCommands::Test { path } => {
            let src = read_utf8_or_die(&path);
            run_tests_in_file(&src, &path, interrupted)
        }
        CliCommands::SandboxedTest { path, offset } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            run_sandboxed_tests_in_file(&src, &path, offset, interrupted)
        }
        CliCommands::TestEvalUpTo { path } => {
            let src = read_utf8_or_die(&path);
            let offset = caret_finder::find_caret_offset(&src)
                .expect("Could not find comment containing `^` in source.");
            test_eval_up_to(&src, &path, offset, interrupted);
        }
        CliCommands::DumpAst { path } => {
            let src = read_utf8_or_die(&path);
            dump_ast(&src, &path)
        }
        CliCommands::ShowType { path, offset } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            show_type(&src, &path, offset)
        }
        CliCommands::DefinitionPosition {
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
        CliCommands::Complete { offset, path } => {
            let src = read_utf8_or_die(&path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            completions::complete(&src, &path, offset);
        }
        CliCommands::TestJson { path } => {
            let src = read_utf8_or_die(&path);

            let session = Session {
                interrupted: Arc::clone(&interrupted),
                stdout_mode: StdoutMode::WriteJson,
                start_time: Instant::now(),
                trace_exprs: false,
                pretty_print_json: true,
            };

            let json_lines = src
                .lines()
                .filter(|line| !line.starts_with("//") && !line.is_empty());

            let (sender, receiver) = channel::<String>();

            let handle = start_eval_thread(session, receiver);

            for line in json_lines {
                handle_request(line, true, Arc::clone(&interrupted), sender.clone());
            }

            drop(sender);
            handle.join().unwrap();
        }
        CliCommands::Rename {
            path,
            new_name,
            offset,
            override_path,
        } => {
            let mut src = read_utf8_or_die(&path);

            let offset = offset.unwrap_or_else(|| {
                src = remove_testing_footer(&src);
                let offset = caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment region containing `^^` in source.");
                src = caret_finder::remove_caret(&src);

                offset
            });

            let src_path = override_path.unwrap_or(path);
            rename::rename(&src, &src_path, offset, &new_name)
        }
        CliCommands::ExtractVariable {
            path,
            offset,
            end_offset,
            override_path,
            name,
        } => {
            let mut src = read_utf8_or_die(&path);
            let (offset, end_offset) = match (offset, end_offset) {
                (Some(offset), Some(end_offset)) => (offset, end_offset),
                _ => {
                    src = remove_testing_footer(&src);
                    let region = caret_finder::find_caret_region(&src)
                        .expect("Could not find comment region containing `^^` in source.");
                    src = caret_finder::remove_caret(&src);

                    region
                }
            };

            let src_path = override_path.unwrap_or(path);
            extract_variable::extract_variable(&src, &src_path, offset, end_offset, &name);
        }
        CliCommands::ExtractFunction {
            path,
            offset,
            end_offset,
            override_path,
            name,
        } => {
            let mut src = read_utf8_or_die(&path);
            let (offset, end_offset) = match (offset, end_offset) {
                (Some(offset), Some(end_offset)) => (offset, end_offset),
                _ => {
                    src = remove_testing_footer(&src);
                    let region = caret_finder::find_caret_region(&src)
                        .expect("Could not find comment region containing `^^` in source.");
                    src = caret_finder::remove_caret(&src);

                    region
                }
            };

            let src_path = override_path.unwrap_or(path);
            extract_function::extract_function(&src, &src_path, offset, end_offset, &name);
        }
        CliCommands::Destructure {
            path,
            offset,
            end_offset,
            override_path,
        } => {
            let mut src = read_utf8_or_die(&path);
            let (offset, end_offset) = match (offset, end_offset) {
                (Some(offset), Some(end_offset)) => (offset, end_offset),
                _ => {
                    src = remove_testing_footer(&src);
                    let region = caret_finder::find_caret_region(&src)
                        .expect("Could not find comment region containing `^^` in source.");
                    src = caret_finder::remove_caret(&src);

                    region
                }
            };

            let src_path = override_path.unwrap_or(path);
            destructure::destructure(&src, &src_path, offset, end_offset);
        }
    }
}

/// Evaluate a garden file, then run eval-up-to and print the result.
fn test_eval_up_to(src: &str, path: &Path, offset: usize, interrupted: Arc<AtomicBool>) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let items = parse_toplevel_items_or_die(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: true,
    };

    if let Err(e) = eval_toplevel_items(path, &items, &mut env, &session) {
        match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::ResumableError(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::AssertionFailed(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::ReachedTickLimit(_) => eprintln!("Reached the tick limit."),
            EvalError::ForbiddenInSandbox(_) => {
                eprintln!("Tried to execute unsafe code in sandboxed mode.")
            }
        }
        return;
    }

    match eval_up_to(path, &mut env, &session, &items, offset) {
        Ok((v, pos)) => println!("{}: {}", pos.as_ide_string(), v.display(&env)),
        Err(EvalUpToErr::EvalError(e)) => match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::ResumableError(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::AssertionFailed(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::ReachedTickLimit(_) => eprintln!("Reached the tick limit."),
            EvalError::ForbiddenInSandbox(_) => {
                eprintln!("Tried to execute unsafe code in sandboxed mode.")
            }
        },
        Err(EvalUpToErr::NoExpressionFound) => eprintln!("Could not find anything to execute"),
        Err(EvalUpToErr::NoValueAvailable) => {
            eprintln!("No previous value saved for this expression")
        }
    }
}

/// Drop the `// args: ` and `// expected stdout:` footer, otherwise
/// we make the comment longer on every run of the test suite.
fn remove_testing_footer(src: &str) -> String {
    let mut new_src = String::with_capacity(src.len());
    for line in src.lines() {
        if line.starts_with("// args: ") {
            break;
        }
        new_src.push_str(line);
        new_src.push('\n');
    }

    new_src
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
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

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
                        &ErrorMessage(vec![Text(format!("Parse error: {}", e.as_string()))]),
                        &position,
                        Level::Error,
                        &vfs,
                    )
                );
            }
            ParseError::Incomplete { message: e, .. } => {
                eprintln!("Parse error (incomplete input): {}", e.as_string());
            }
        }
    }

    for item in items {
        match item {
            ToplevelItem::Expr(e) => {
                println!("{:#?}", e.0.expr_);
            }
            d => {
                println!("{:#?}", d);
            }
        }
    }
}

fn parse_toplevel_items_or_die(
    path: &Path,
    src: &str,
    vfs: &mut Vfs,
    id_gen: &mut IdGenerator,
) -> Vec<ToplevelItem> {
    let (items, errors) = parse_toplevel_items(path, src, vfs, id_gen);

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
                        &ErrorMessage(vec![Text(format!("Parse error: {}", e.as_string()))]),
                        &position,
                        Level::Error,
                        vfs,
                    )
                ),
                ParseError::Incomplete { message: e, .. } => {
                    eprintln!("Parse error (incomplete input): {}", e.as_string())
                }
            }
        }

        std::process::exit(1);
    }

    items
}

fn run_file(src: &str, path: &Path, arguments: &[String], interrupted: Arc<AtomicBool>) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let items = parse_toplevel_items_or_die(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    env.cli_args = Vec::from(arguments);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    match eval_toplevel_items(path, &items, &mut env, &session) {
        Ok(_) => {}
        Err(EvalError::ResumableError(position, msg)) => {
            eprintln!(
                "{}",
                &format_error_with_stack(&msg, &position, &env.stack.0, &env.vfs)
            );
        }
        Err(EvalError::AssertionFailed(position, msg)) => {
            eprintln!(
                "{}",
                &format_error_with_stack(&msg, &position, &env.stack.0, &env.vfs)
            );
        }
        Err(EvalError::Interrupted) => {
            eprintln!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit(position)) => {
            eprintln!("{}: Reached the tick limit.", position.as_ide_string());
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

    #[test]
    fn test_builtins_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test").arg("src/builtins.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_builtins_check() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("check").arg("src/builtins.gdn");
        cmd.assert().success();
    }
}
