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
mod types;
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
    Format { path: PathBuf },
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
        #[clap(long)]
        override_path: Option<PathBuf>,
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
        override_path: Option<PathBuf>,
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
        override_path: Option<PathBuf>,
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
        override_path: Option<PathBuf>,
        #[clap(long)]
        new_name: String,
    },
    /// Parse the Garden program at the path specified and print the
    /// AST.
    DumpAst { path: PathBuf },
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
        CliCommands::DumpAst { path } => {
            let abs_path = to_abs_path(&path);
            let src = read_utf8_or_die(&abs_path);
            dump_ast(&src, &abs_path)
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
            override_path,
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

            let src_path = to_abs_path(&override_path.unwrap_or(path));
            match rename::rename(&src, &src_path, offset, &new_name) {
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
            override_path,
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

            let src_path = to_abs_path(&override_path.unwrap_or(path));
            match extract_variable::extract_variable(&src, &src_path, offset, end_offset, &name) {
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
            override_path,
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

            let src_path = to_abs_path(&override_path.unwrap_or(path));
            match extract_function::extract_function(&src, &src_path, offset, end_offset, &name) {
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
            override_path,
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

            let src_path = to_abs_path(&override_path.unwrap_or(path));
            match destructure::destructure(&src, &src_path, offset, end_offset) {
                Ok(new_src) => {
                    print!("{new_src}");
                }
                Err(msg) => {
                    eprintln!("{msg}");
                    std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
                }
            }
        }
        CliCommands::Format { path } => {
            let abs_path = to_abs_path(&path);
            let mut src = read_utf8_or_die(&abs_path);
            src = remove_testing_footer(&src);
            let formatted = format::format(&src, &abs_path);
            print!("{formatted}");
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

fn dump_ast(src: &str, path: &Path) {
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

    /// Helper function to run golden tests for a specific subdirectory.
    fn run_golden_tests(subdir: &str) -> TestResult<()> {
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
    fn test_golden_check() -> TestResult<()> {
        run_golden_tests("check")
    }

    #[test]
    fn test_golden_check_fix() -> TestResult<()> {
        run_golden_tests("check_fix")
    }

    #[test]
    fn test_golden_run_code_blocks() -> TestResult<()> {
        run_golden_tests("run_code_blocks")
    }

    #[test]
    fn reftest_complete() -> TestResult<()> {
        run_golden_tests("complete")
    }

    #[test]
    fn reftest_destructure() -> TestResult<()> {
        run_golden_tests("destructure")
    }

    #[test]
    fn test_golden_eval_up_to() -> TestResult<()> {
        run_golden_tests("eval_up_to")
    }

    #[test]
    fn reftest_extract_function() -> TestResult<()> {
        run_golden_tests("extract_function")
    }

    #[test]
    fn reftest_extract_variable() -> TestResult<()> {
        run_golden_tests("extract_variable")
    }

    #[test]
    fn test_golden_format() -> TestResult<()> {
        run_golden_tests("format")
    }

    #[test]
    fn reftest_position() -> TestResult<()> {
        run_golden_tests("go_to_def")
    }

    #[test]
    fn reftest_highlight() -> TestResult<()> {
        run_golden_tests("highlight")
    }

    #[test]
    fn reftest_hover() -> TestResult<()> {
        run_golden_tests("hover")
    }

    #[test]
    fn reftest_json_session() -> TestResult<()> {
        run_golden_tests("json_session")
    }

    #[test]
    fn test_golden_parser() -> TestResult<()> {
        run_golden_tests("parser")
    }

    #[test]
    fn test_golden_playground() -> TestResult<()> {
        run_golden_tests("playground")
    }

    #[test]
    fn reftest_rename() -> TestResult<()> {
        run_golden_tests("rename")
    }

    #[test]
    fn test_golden_runtime() -> TestResult<()> {
        run_golden_tests("runtime")
    }

    #[test]
    fn test_golden_sandboxed_test() -> TestResult<()> {
        run_golden_tests("sandboxed_test")
    }

    #[test]
    fn test_golden_test() -> TestResult<()> {
        run_golden_tests("test")
    }

    #[test]
    fn test_prelude_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test")
            .arg("src/__prelude.gdn")
            .arg("src/__fs.gdn")
            .arg("src/__random.gdn")
            .arg("src/__reflect.gdn");
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
    fn test_lsp_initialize_shutdown() {
        use std::io::Write;
        use std::process::Stdio;

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        // Prepare LSP messages
        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;
        let shutdown_request = r#"{"jsonrpc":"2.0","id":2,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        // Write to stdin
        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        // Wait for the process to complete and get output
        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        // Verify the command succeeded
        assert!(output.status.success());

        // Verify the output contains expected LSP responses
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );
        assert!(
            stdout.contains(r#""name":"garden-lsp""#),
            "Should contain server name"
        );
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain shutdown response"
        );
    }

    #[test]
    fn test_lsp_goto_definition() {
        use std::io::Write;
        use std::process::Stdio;

        // Create a temporary test file
        let test_content = r#"fun add_one(x: Int): Int {
  x + 1
}

fun main() {
  add_one(5)
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_goto_def.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        // Prepare LSP messages
        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Request definition of "add_one" at line 5, character 2
        // Line 5 is "  add_one(5)" and character 2 is the start of "add_one"
        let goto_def_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"{}"}},"position":{{"line":5,"character":2}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            goto_def_request.len(), goto_def_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        // Write to stdin
        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        // Wait for the process to complete and get output
        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        // Verify the command succeeded
        assert!(output.status.success());

        // Verify the output contains expected LSP responses
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should contain initialize response
        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );

        // Should contain go-to-definition response with id 2
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain goto-definition response"
        );

        // Should contain the URI in the response
        assert!(
            stdout.contains(&file_uri),
            "Should contain file URI in definition response"
        );

        // Should contain a range in the response
        assert!(stdout.contains(r#""range""#), "Should contain range");

        // The definition should point to line 0 (where add_one is defined)
        assert!(
            stdout.contains(r#""line":0"#),
            "Should point to line 0 where add_one is defined"
        );

        // Should contain shutdown response
        assert!(
            stdout.contains(r#""id":3"#),
            "Should contain shutdown response"
        );

        // Clean up the temporary file
        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_hover() {
        use std::io::Write;
        use std::process::Stdio;

        let test_content = r#"/// Add one to the given integer.
fun add_one(x: Int): Int {
  x + 1
}

fun main() {
  add_one(5)
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_hover.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Hover on "add_one" at the call site (line 6, character 2).
        let hover_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{{"textDocument":{{"uri":"{}"}},"position":{{"line":6,"character":2}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            hover_request.len(), hover_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        assert!(output.status.success());

        let stdout = String::from_utf8_lossy(&output.stdout);

        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain hover response"
        );

        // The response should include hover contents with the function type.
        assert!(
            stdout.contains(r#""contents""#),
            "Should contain hover contents"
        );

        // The doc comment should be included in the hover.
        assert!(
            stdout.contains("Add one to the given integer."),
            "Should contain doc comment in hover. Output: {stdout}"
        );

        // The capabilities response should advertise hover support.
        assert!(
            stdout.contains(r#""hoverProvider":true"#),
            "Should advertise hover capability"
        );

        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_completion() {
        use std::io::Write;
        use std::process::Stdio;

        // Create a temporary test file with String methods
        let test_content = r#"fun main() {
  let s = "hello"
  s.len
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_completion.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        // Prepare LSP messages
        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Request completions in the middle of "len" on line 2, character 5
        // Line 2 is "  s.len" where:
        // - characters 0-1 are spaces
        // - character 2 is 's'
        // - character 3 is '.'
        // - characters 4-6 are "len"
        // Character 5 is in the middle of "len"
        let completion_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"{}"}},"position":{{"line":2,"character":5}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            completion_request.len(), completion_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        // Write to stdin
        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        // Wait for the process to complete and get output
        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        // Verify the command succeeded
        assert!(output.status.success());

        // Verify the output contains expected LSP responses
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should contain initialize response
        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );

        // Should contain completion response with id 2
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain completion response"
        );

        // Should contain String methods like "len" which start with "len"
        // The completion should return "len()" since it matches the prefix
        assert!(
            stdout.contains(r#""label":"len()"#),
            "Should contain 'len()' completion for String method"
        );

        // Should contain shutdown response
        assert!(
            stdout.contains(r#""id":3"#),
            "Should contain shutdown response"
        );

        // Clean up the temporary file
        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_diagnostics() {
        use std::io::Write;
        use std::process::Stdio;

        // Create a temporary test file with an error
        let test_content = r#"fun main() {
  let x: Int = "not an int"
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_diagnostics.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        // Prepare LSP messages
        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Send didOpen notification
        let did_open = format!(
            r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{}","languageId":"garden","version":1,"text":"{}"}}}}}}"#,
            file_uri,
            test_content.replace('\n', "\\n").replace('"', "\\\"")
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":2,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            did_open.len(), did_open,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        // Write to stdin
        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        // Wait for the process to complete and get output
        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        // Verify the command succeeded
        assert!(output.status.success());

        // Verify the output contains expected LSP responses
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should contain initialize response
        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );

        // Should contain publishDiagnostics notification
        assert!(
            stdout.contains(r#""method":"textDocument/publishDiagnostics""#),
            "Should contain publishDiagnostics notification"
        );

        // Should contain the file URI in the diagnostics
        assert!(
            stdout.contains(&file_uri),
            "Should contain file URI in diagnostics"
        );

        // Should contain a diagnostic about the type error
        assert!(
            stdout.contains(r#""diagnostics""#),
            "Should contain diagnostics array"
        );

        // Should contain severity (error = 1)
        assert!(
            stdout.contains(r#""severity":1"#),
            "Should contain error severity"
        );

        // Should contain shutdown response
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain shutdown response"
        );

        // Clean up the temporary file
        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_code_action() {
        use std::io::Write;
        use std::process::Stdio;

        // Create a temporary test file with a typo that has a quickfix
        let test_content = r#"fun main() {
  Path{ p: "/foo" }.readd()
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_code_action.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        // Prepare LSP messages
        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Request code actions at line 1, characters 20-25 (where "readd" is)
        let code_action_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/codeAction","params":{{"textDocument":{{"uri":"{}"}},"range":{{"start":{{"line":1,"character":20}},"end":{{"line":1,"character":25}}}},"context":{{"diagnostics":[]}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            code_action_request.len(), code_action_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        // Write to stdin
        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        // Wait for the process to complete and get output
        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        // Verify the command succeeded
        assert!(output.status.success());

        // Verify the output contains expected LSP responses
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should contain initialize response
        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );

        // Should contain code action response with id 2
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain code action response"
        );

        // Should contain the quickfix title
        assert!(
            stdout.contains("read"),
            "Should contain 'read' in the quickfix suggestion"
        );

        // Should contain quickfix kind
        assert!(
            stdout.contains(r#""kind":"quickfix""#),
            "Should contain quickfix kind"
        );

        // Should contain shutdown response
        assert!(
            stdout.contains(r#""id":3"#),
            "Should contain shutdown response"
        );

        // Clean up the temporary file
        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_code_action_extract_function() {
        use std::io::Write;
        use std::process::Stdio;

        let test_content = "fun foo() {\n  let _ = 1 + 2\n}\n";
        let test_file =
            std::env::temp_dir().join("garden_lsp_test_code_action_extract_function.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Selection covers `1 + 2` on line 1.
        let code_action_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/codeAction","params":{{"textDocument":{{"uri":"{}"}},"range":{{"start":{{"line":1,"character":10}},"end":{{"line":1,"character":15}}}},"context":{{"diagnostics":[]}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            code_action_request.len(), code_action_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        assert!(output.status.success());

        let stdout = String::from_utf8_lossy(&output.stdout);

        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain code action response"
        );

        assert!(
            stdout.contains("Extract function"),
            "Should contain 'Extract function' code action title, got: {stdout}"
        );

        assert!(
            stdout.contains(r#""kind":"refactor.extract""#),
            "Should contain refactor.extract kind, got: {stdout}"
        );

        let _ = std::fs::remove_file(&test_file);
    }

    #[test]
    fn test_lsp_document_highlight() {
        use std::io::Write;
        use std::process::Stdio;

        let test_content = r#"fun file_name() {
  let foo = 1
  foo + foo
}
"#;
        let test_file = std::env::temp_dir().join("garden_lsp_test_highlight.gdn");
        std::fs::write(&test_file, test_content).expect("Failed to write test file");

        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut child = Command::new(path)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        let file_uri = format!("file://{}", test_file.display());

        let init_request =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
        let initialized = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

        // Place the cursor on `foo` in `let foo = 1` (line 1, character 6).
        let highlight_request = format!(
            r#"{{"jsonrpc":"2.0","id":2,"method":"textDocument/documentHighlight","params":{{"textDocument":{{"uri":"{}"}},"position":{{"line":1,"character":6}}}}}}"#,
            file_uri
        );

        let shutdown_request = r#"{"jsonrpc":"2.0","id":3,"method":"shutdown"}"#;
        let exit = r#"{"jsonrpc":"2.0","method":"exit"}"#;

        let input = format!(
            "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
            init_request.len(), init_request,
            initialized.len(), initialized,
            highlight_request.len(), highlight_request,
            shutdown_request.len(), shutdown_request,
            exit.len(), exit
        );

        {
            let stdin = child.stdin.as_mut().expect("Failed to get stdin");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to stdin");
        }

        let output = child
            .wait_with_output()
            .expect("Failed to wait for command");

        assert!(output.status.success());

        let stdout = String::from_utf8_lossy(&output.stdout);

        assert!(
            stdout.contains(r#""id":1"#),
            "Should contain initialize response"
        );
        assert!(
            stdout.contains(r#""documentHighlightProvider":true"#),
            "Should advertise documentHighlightProvider capability"
        );
        assert!(
            stdout.contains(r#""id":2"#),
            "Should contain documentHighlight response"
        );
        // Three occurrences (definition + two uses) means three ranges.
        assert_eq!(
            stdout.matches(r#""range""#).count(),
            3,
            "Should contain three highlight ranges, got: {stdout}"
        );
        assert!(
            stdout.contains(r#""id":3"#),
            "Should contain shutdown response"
        );

        let _ = std::fs::remove_file(&test_file);
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
