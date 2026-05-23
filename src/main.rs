//! Main entry point for the Garden binary.

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
#![allow(clippy::needless_ifs)]
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
// TODO: fix this.
#![allow(clippy::map_entry)]
// PLs are complicated, it can be helpful to have docs.
#![warn(missing_docs)]
// This seems to give false positives and suggest changes that don't
// compile.
#![allow(clippy::cmp_owned)]

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
mod format;
mod garden_type;
mod go_to_def;
mod highlight;
mod hover;
mod json_session;
mod lsp;
mod namespaces;
mod nrepl;
mod parser;
mod pos_to_id;
mod prompt;
mod rename;
mod run_code_blocks;
mod sandboxed_playground;
mod syntax_check;
mod syntax_highlighter;
mod test_runner;
mod type_defs;
mod values;
mod version;

use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use eval::{eval_up_to, EvalUpToErr, StdoutMode};
use go_to_def::print_pos;
use hover::reftest_hover;
use json_session::{handle_request, start_eval_thread};
use parser::vfs::{to_abs_path, Vfs, VfsPathBuf};
use test_runner::{run_sandboxed_tests_in_file, run_tests_in_files};

use crate::diagnostics::{format_diagnostic, format_exception_with_stack, Severity};
use crate::env::Env;
use crate::eval::{
    eval_toplevel_items, load_toplevel_items, EvalError, ExceptionInfo, Session, StdoutJsonFormat,
};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::{parse_toplevel_items, ParseError};

pub(crate) const BAD_CLI_REQUEST_EXIT_CODE: i32 = 10;

#[derive(Debug, Parser)]
#[command(author, version=version::VERSION.as_str(), name="Garden", about = "A programming language for growing programs", long_about = None)]
struct Cli {
    /// Print each expression before evaluating it.
    #[clap(long, global = true)]
    trace: bool,

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
        /// Evaluate the given code instead of reading from a file.
        #[clap(short)]
        c: Option<String>,
        path: Option<PathBuf>,
        arguments: Vec<String>,
    },
    /// Run all the tests in the Garden files specified.
    Test {
        /// If provided, only run tests whose name contains this
        /// substring.
        #[clap(short, long, value_name = "SUBSTRING")]
        name_contains: Option<String>,
        paths: Vec<PathBuf>,
    },
    /// Run the tests associated with the definition at this offset,
    /// but give up if the program exceeds a time limit or attempts
    /// I/O.
    SandboxedTest {
        path: PathBuf,
        offset: Option<usize>,
        #[clap(long)]
        override_path: Option<PathBuf>,
    },
    /// Format a Garden file and print the re-indented source code.
    Format {
        path: PathBuf,
        /// Check if the file is already formatted. Exits with a
        /// non-zero status if formatting would change the file, and
        /// does not print the formatted output.
        #[clap(long, action)]
        check: bool,
    },
    /// Check the Garden program at the path specified for issues.
    Check {
        path: PathBuf,
        /// Display results as machine-readable JSON.
        #[clap(long, action)]
        json: bool,
        /// Treat the file as if it was at this path.
        ///
        /// This allows you to write the state of a file to somewhere
        /// in /tmp, and check it as if it was in the original path,
        /// without requiring the user to save.
        #[clap(long)]
        override_path: Option<PathBuf>,
        /// Apply all available quickfixes to the file.
        #[clap(long, action)]
        fix: bool,
        /// Print the fixed file to stdout instead of modifying in place.
        /// Only valid with --fix.
        #[clap(long, action)]
        stdout: bool,
    },
    /// Run Garden code blocks in markdown and .gdn files.
    RunCodeBlocks { paths: Vec<PathBuf> },
    /// Parse the Garden program at the path specified and print the
    /// AST.
    ReftestAst { path: PathBuf },
    /// Show possible completions at the position given.
    ReftestComplete {
        path: PathBuf,
        offset: Option<usize>,
    },
    /// Wrap the expression at the offset specified in a `match`.
    ReftestDestructure {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
    },
    /// Run the program specified, calling its main() function, then
    /// run eval-up-to at the position specified and print the result.
    ///
    /// Used for testing the eval-up-to feature.
    ReftestEvalUpTo { path: PathBuf },
    /// Replace the expression at this offset with a function called
    /// `name`, and use the expression as the function's body.
    ReftestExtractFunction {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
        #[clap(long)]
        name: String,
    },
    /// Replace the expression at this offset with a variable called
    /// `name`, and use the expression as the variables's definition.
    ReftestExtractVariable {
        path: PathBuf,
        offset: Option<usize>,
        end_offset: Option<usize>,
        #[clap(long)]
        name: String,
    },
    /// Print the positions of all occurrences of the local variable
    /// at the position given. One JSON-encoded position per line.
    ReftestHighlight { path: PathBuf },
    /// Show the type of the expression at the position given.
    ReftestHover { path: PathBuf },
    /// Evaluate all the entries in the .jsonl file as if they were in
    /// a JSON session.
    ///
    /// Lines starting `//` are ignored.
    ReftestJsonSession { path: PathBuf },
    /// Replay LSP messages from a JSONL file and pretty-print the
    /// responses the LSP server would emit.
    ///
    /// Lines starting `//` are ignored.
    ReftestLsp { path: PathBuf },
    /// Replay nREPL requests from a JSON file against an in-process
    /// nREPL connection and print the responses.
    ///
    /// Each non-blank, non-comment line is a JSON object representing
    /// a single nREPL request. Lines starting `//` are ignored.
    ReftestNrepl { path: PathBuf },
    /// Show the definition position of the value at the position
    /// given.
    ReftestPosition {
        path: PathBuf,
        offset: Option<usize>,
    },
    /// Rename the local variable at this offset to the new name
    /// specified.
    ReftestRename {
        path: PathBuf,
        offset: Option<usize>,
        #[clap(long)]
        new_name: String,
    },
    /// Run a Garden snippet in a sandbox and return the output as
    /// JSON.
    PlaygroundRun { path: PathBuf },
    /// Start the Language Server Protocol (LSP) server.
    Lsp,
    /// Start an nREPL server, listening for clients over TCP.
    Nrepl {
        /// Port to listen on. Use 0 to let the operating system pick
        /// a free port.
        #[clap(long, default_value_t = 7888)]
        port: u16,
        /// Host to bind to.
        #[clap(long, default_value = "127.0.0.1")]
        host: String,
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
    let trace_exprs = args.trace;
    match args.command {
        CliCommands::Repl => cli_session::repl(interrupted, trace_exprs),
        CliCommands::Json => json_session::json_session(interrupted),
        CliCommands::Run { c, path, arguments } => {
            let (src, abs_path) = match (c, path) {
                (Some(code), None) => {
                    // Use a synthetic absolute path for code provided via -c
                    (code, PathBuf::from("/<cli>"))
                }
                (None, Some(path)) => {
                    let abs_path = to_abs_path(&path);
                    let src = read_utf8_or_die(&abs_path);
                    (src, abs_path)
                }
                (Some(_), Some(_)) => {
                    eprintln!("Error: Cannot specify both -c and a file path.");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
                (None, None) => {
                    eprintln!("Error: Either -c or a file path must be specified.");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            };
            run_file(&src, &abs_path, &arguments, interrupted, trace_exprs)
        }
        CliCommands::RunCodeBlocks { paths } => {
            run_code_blocks::run_code_blocks(&paths, interrupted);
        }
        CliCommands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
        CliCommands::Check {
            path,
            json,
            override_path,
            fix,
            stdout,
        } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
            src = remove_testing_footer(&src);
            let src_path = to_abs_path(&override_path.unwrap_or(path.clone()));
            syntax_check::check(&src_path, &src, json, fix, stdout, &abs_path)
        }
        CliCommands::Test {
            paths,
            name_contains,
        } => {
            let mut paths_and_srcs = vec![];
            for path in paths.into_iter() {
                let abs_path = to_abs_path(&path);
                let src = read_utf8_or_die(&abs_path);
                paths_and_srcs.push((src, abs_path));
            }

            run_tests_in_files(&paths_and_srcs, name_contains.as_ref(), interrupted)
        }
        CliCommands::SandboxedTest {
            path,
            offset,
            override_path,
        } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });

            let src_path = to_abs_path(&override_path.unwrap_or(path));
            run_sandboxed_tests_in_file(&src, &src_path, offset, interrupted)
        }
        CliCommands::ReftestEvalUpTo { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            let offset = caret_finder::find_caret_offset(&src)
                .expect("Could not find comment containing `^` in source.");
            reftest_eval_up_to(&src, &abs_path, offset, interrupted, trace_exprs);
        }
        CliCommands::ReftestAst { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            reftest_ast(&src, &abs_path)
        }
        CliCommands::ReftestHover { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            let offset = caret_finder::find_caret_offset(&src)
                .expect("Could not find comment containing `^` in source.");
            reftest_hover(&src, &abs_path, offset)
        }
        CliCommands::ReftestPosition { path, offset } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);

            let has_caret = offset.is_none();
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });

            print_pos(&src, &abs_path, offset, has_caret)
        }
        CliCommands::ReftestHighlight { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);

            let offset = caret_finder::find_caret_offset(&src)
                .expect("Could not find comment containing `^` in source.");

            let project_root = std::env::current_dir().unwrap_or(PathBuf::from("/"));

            for pos in highlight::highlight_occurrences(&src, &abs_path, offset) {
                let mut placeholder_pos = pos.clone();
                let mut path = PathBuf::from("GDN_TEST_ROOT");
                path.push(parser::vfs::to_project_relative(&pos.path, &project_root));
                placeholder_pos.path = Rc::new(path);
                println!("{}", serde_json::to_string(&placeholder_pos).unwrap());
            }
        }
        CliCommands::ReftestComplete { offset, path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            let offset = offset.unwrap_or_else(|| {
                caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment containing `^` in source.")
            });
            let items = completions::complete(&src, &abs_path, offset);
            for item in items {
                println!("{}", serde_json::to_string(&item).unwrap());
            }
        }
        CliCommands::ReftestNrepl { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            nrepl::reftest_nrepl(&src);
        }
        CliCommands::ReftestJsonSession { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);

            let session = Session {
                interrupted: Arc::clone(&interrupted),
                stdout_mode: StdoutMode::WriteJson(StdoutJsonFormat::ReplSession),
                start_time: Instant::now(),
                trace_exprs,
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
        CliCommands::ReftestRename {
            path,
            new_name,
            offset,
        } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);

            let offset = offset.unwrap_or_else(|| {
                src = remove_testing_footer(&src);
                let offset = caret_finder::find_caret_offset(&src)
                    .expect("Could not find comment region containing `^^` in source.");
                src = caret_finder::remove_caret(&src);

                offset
            });

            match rename::rename(&src, &abs_path, offset, &new_name) {
                Ok(new_src) => {
                    print!("{new_src}");
                }
                Err(msg) => {
                    eprintln!("{msg}");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            }
        }
        CliCommands::ReftestExtractVariable {
            path,
            offset,
            end_offset,
            name,
        } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
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

            match extract_variable::extract_variable(&src, &abs_path, offset, end_offset, &name) {
                Ok(new_src) => {
                    print!("{new_src}");
                }
                Err(msg) => {
                    eprintln!("{msg}");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            }
        }
        CliCommands::ReftestExtractFunction {
            path,
            offset,
            end_offset,
            name,
        } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
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

            match extract_function::extract_function(&src, &abs_path, offset, end_offset, &name) {
                Ok(new_src) => {
                    print!("{new_src}");
                }
                Err(msg) => {
                    eprintln!("{msg}");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            }
        }
        CliCommands::ReftestDestructure {
            path,
            offset,
            end_offset,
        } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
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

            match destructure::destructure(&src, &abs_path, offset, end_offset) {
                Ok(new_src) => {
                    print!("{new_src}");
                }
                Err(msg) => {
                    eprintln!("{msg}");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            }
        }
        CliCommands::Format { path, check } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
            src = remove_testing_footer(&src);
            let formatted = format::format(&src, &abs_path);
            if check {
                if src != formatted {
                    eprintln!("{}: not formatted", path.display());
                    std::process::exit(1);
                }
            } else {
                print!("{formatted}");
            }
        }
        CliCommands::PlaygroundRun { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            sandboxed_playground::run_sandboxed_playground(
                &src,
                &abs_path,
                interrupted,
                trace_exprs,
            );
        }
        CliCommands::Lsp => {
            init_tracing();
            lsp::run_lsp();
        }
        CliCommands::ReftestLsp { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            lsp::reftest_lsp(&src);
        }
        CliCommands::Nrepl { port, host } => {
            init_tracing();
            nrepl::run_nrepl(&host, port, interrupted);
        }
    }
}

/// Initialize the tracing subscriber, writing logs to stderr.
///
/// Honours the `GARDEN_LOG` environment variable for filtering; defaults
/// to `info` if unset.
fn init_tracing() {
    use tracing_subscriber::EnvFilter;

    let filter = EnvFilter::try_from_env("GARDEN_LOG").unwrap_or_else(|_| EnvFilter::new("info"));

    let _ = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .try_init();
}

/// Evaluate a garden file, then run eval-up-to and print the result.
fn reftest_eval_up_to(
    src: &str,
    path: &Path,
    offset: usize,
    interrupted: Arc<AtomicBool>,
    trace_exprs: bool,
) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let vfs_path = vfs.insert(Rc::new(path.to_owned()), src.to_owned());

    let items = parse_toplevel_items_or_die(&vfs_path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_or_create_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs,
        pretty_print_json: true,
    };

    if let Err(e) = eval_toplevel_items(&vfs_path, &items, &mut env, &session) {
        match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::Exception(ExceptionInfo {
                position: _,
                message,
            }) => eprintln!("{}", message.as_string()),
            EvalError::AssertionFailed(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::ReachedTickLimit(_) => eprintln!("Reached the tick limit."),
            EvalError::ReachedStackLimit(_) => eprintln!("Reached the stack limit."),
            EvalError::ForbiddenInSandbox(_) => {
                eprintln!("Tried to execute unsafe code in sandboxed mode.")
            }
        }
        return;
    }

    match eval_up_to(&vfs_path, &mut env, &session, &items, offset) {
        Ok((v, pos)) => println!(
            "{}: {}",
            pos.as_ide_string(&env.project_root),
            v.display(&env)
        ),
        Err(EvalUpToErr::EvalError(e)) => match e {
            EvalError::Interrupted => eprintln!("Interrupted."),
            EvalError::Exception(ExceptionInfo {
                position: _,
                message,
            }) => eprintln!("{}", message.as_string()),
            EvalError::AssertionFailed(_, msg) => eprintln!("{}", msg.as_string()),
            EvalError::ReachedTickLimit(_) => eprintln!("Reached the tick limit."),
            EvalError::ReachedStackLimit(_) => eprintln!("Reached the stack limit."),
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

fn reftest_ast(src: &str, path: &Path) {
    let project_root = std::env::current_dir().unwrap_or(PathBuf::from("/"));

    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let (items, errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    for error in errors.into_iter() {
        match error {
            ParseError::Invalid {
                position,
                message: e,
                notes,
            } => {
                eprintln!(
                    "{}",
                    format_diagnostic(
                        &ErrorMessage(vec![Text(format!("Parse error: {}", e.as_string()))]),
                        &position,
                        &project_root,
                        Severity::Error,
                        &notes,
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
                println!("{d:#?}");
            }
        }
    }
}

fn parse_toplevel_items_or_die(
    vfs_path: &VfsPathBuf,
    src: &str,
    vfs: &mut Vfs,
    id_gen: &mut IdGenerator,
) -> Vec<ToplevelItem> {
    let (items, errors) = parse_toplevel_items(vfs_path, src, id_gen);

    let project_root = std::env::current_dir().unwrap_or(PathBuf::from("/"));

    if !errors.is_empty() {
        for error in errors.into_iter() {
            match error {
                ParseError::Invalid {
                    position,
                    message: e,
                    notes,
                } => eprintln!(
                    "{}",
                    format_diagnostic(
                        &ErrorMessage(vec![Text(format!("Parse error: {}", e.as_string()))]),
                        &position,
                        &project_root,
                        Severity::Error,
                        &notes,
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

fn run_file(
    src: &str,
    path: &Path,
    arguments: &[String],
    interrupted: Arc<AtomicBool>,
    trace_exprs: bool,
) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let vfs_path = vfs.insert(Rc::new(path.to_owned()), src.to_owned());

    let items = parse_toplevel_items_or_die(&vfs_path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    env.cli_args = Vec::from(arguments);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_or_create_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs,
        pretty_print_json: false,
    };

    match eval_toplevel_items(&vfs_path, &items, &mut env, &session) {
        Ok(_) => {}
        Err(EvalError::Exception(ExceptionInfo { position, message })) => {
            eprintln!(
                "{}",
                format_exception_with_stack(
                    &message,
                    &position,
                    &env.stack.0,
                    &env.vfs,
                    &env.project_root
                )
            );
        }
        Err(EvalError::AssertionFailed(position, msg)) => {
            eprintln!(
                "{}",
                format_exception_with_stack(
                    &msg,
                    &position,
                    &env.stack.0,
                    &env.vfs,
                    &env.project_root
                )
            );
        }
        Err(EvalError::Interrupted) => {
            eprintln!("Interrupted");
        }
        Err(EvalError::ReachedTickLimit(position)) => {
            eprintln!(
                "{}: Reached the tick limit.",
                position.as_ide_string(&env.project_root)
            );
        }
        Err(EvalError::ReachedStackLimit(position)) => {
            eprintln!(
                "{}: Reached the stack limit.",
                position.as_ide_string(&env.project_root)
            );
        }
        Err(EvalError::ForbiddenInSandbox(position)) => {
            eprintln!(
                "{}: Tried to execute unsafe code in sandboxed mode.",
                position.as_ide_string(&env.project_root)
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_cmd::prelude::*;
    use std::process::Command;

    use goldentests::{TestConfig, TestResult};

    /// Run the reftests in the given subdirectory of `src/test_files/`.
    ///
    /// A reftest is a test that uses a human readable file to show
    /// correct behaviour: the file contains a sample program along
    /// with the expected output, and the test runner checks that the
    /// program still produces that output.
    ///
    /// When the functionality under test does not have a natural text
    /// representation (e.g. hover info, go-to-definition, AST dumps),
    /// the binary exposes a dedicated `reftest-foo` subcommand that
    /// prints a textual rendering, which the reftest file then
    /// captures as its expected output.
    fn run_reftests(subdir: &str) -> TestResult<()> {
        // Build the binary to ensure we're testing the latest code.
        let bin_path = escargot::CargoBuild::new()
            .bin("garden")
            .run()
            .expect("Failed to build garden binary")
            .path()
            .to_path_buf();

        let test_path = format!("src/test_files/{}", subdir);
        let mut config = TestConfig::new(bin_path, &test_path, "// ");
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn reftest_check() -> TestResult<()> {
        run_reftests("check")
    }

    #[test]
    fn reftest_check_fix() -> TestResult<()> {
        run_reftests("check_fix")
    }

    #[test]
    fn reftest_run_code_blocks() -> TestResult<()> {
        run_reftests("run_code_blocks")
    }

    #[test]
    fn reftest_complete() -> TestResult<()> {
        run_reftests("complete")
    }

    #[test]
    fn reftest_destructure() -> TestResult<()> {
        run_reftests("destructure")
    }

    #[test]
    fn reftest_eval_up_to() -> TestResult<()> {
        run_reftests("eval_up_to")
    }

    #[test]
    fn reftest_extract_function() -> TestResult<()> {
        run_reftests("extract_function")
    }

    #[test]
    fn reftest_extract_variable() -> TestResult<()> {
        run_reftests("extract_variable")
    }

    #[test]
    fn reftest_format() -> TestResult<()> {
        run_reftests("format")
    }

    #[test]
    fn reftest_position() -> TestResult<()> {
        run_reftests("go_to_def")
    }

    #[test]
    fn reftest_highlight() -> TestResult<()> {
        run_reftests("highlight")
    }

    #[test]
    fn reftest_hover() -> TestResult<()> {
        run_reftests("hover")
    }

    #[test]
    fn reftest_json_session() -> TestResult<()> {
        run_reftests("json_session")
    }

    #[test]
    fn reftest_lsp() -> TestResult<()> {
        run_reftests("lsp")
    }

    #[test]
    fn reftest_nrepl() -> TestResult<()> {
        run_reftests("nrepl")
    }

    #[test]
    fn reftest_parser() -> TestResult<()> {
        run_reftests("parser")
    }

    #[test]
    fn reftest_playground() -> TestResult<()> {
        run_reftests("playground")
    }

    #[test]
    fn reftest_rename() -> TestResult<()> {
        run_reftests("rename")
    }

    #[test]
    fn reftest_runtime() -> TestResult<()> {
        run_reftests("runtime")
    }

    #[test]
    fn reftest_sandboxed_test() -> TestResult<()> {
        run_reftests("sandboxed_test")
    }

    #[test]
    fn reftest_test() -> TestResult<()> {
        run_reftests("test")
    }

    #[test]
    fn test_prelude_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test")
            .arg("src/__prelude.gdn")
            .arg("src/__fs.gdn")
            .arg("src/__random.gdn")
            .arg("src/__reflect.gdn")
            .arg("src/__shell.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_prelude_check() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("check").arg("src/__prelude.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_site_builder_check() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("check").arg("website/build_site.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_site_builder_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test")
            .arg("website/build_site.gdn")
            .arg("website/markdown.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_destructure_range() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("reftest-destructure")
            .arg("src/unit_test_files/destructure.gdn")
            .arg("49")
            .arg("54");
        cmd.assert().success();
    }

    #[test]
    fn test_run_with_c_flag() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("run").arg("-c").arg(r#"println("hello from -c")"#);
        cmd.assert().success().stdout("hello from -c\n");
    }

    #[test]
    fn test_run_c_flag_with_path_errors() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("run")
            .arg("-c")
            .arg(r#"println("test")"#)
            .arg("some_file.gdn");
        cmd.assert()
            .code(super::BAD_CLI_REQUEST_EXIT_CODE)
            .stderr(predicates::str::contains(
                "Cannot specify both -c and a file path",
            ));
    }

    #[test]
    fn test_run_without_c_or_path_errors() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("run");
        cmd.assert()
            .code(super::BAD_CLI_REQUEST_EXIT_CODE)
            .stderr(predicates::str::contains(
                "Either -c or a file path must be specified",
            ));
    }
}
