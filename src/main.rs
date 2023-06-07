mod colors;
mod commands;
mod eval;
mod interactive_session;
mod json_session;
mod parse;
mod prompt;

use std::cmp::max;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::{Parser, Subcommand};
use eval::{eval_def_or_exprs, Env, EvalError, Session};
use parse::parse_def_or_expr_from_str;

use crate::parse::line_of_position;

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
    /// Print an example JSON request that's valid in JSON sessions.
    JsonExample,
    /// Execute a Garden program at the path specified.
    Run { path: PathBuf },
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
        Commands::Run { path } => match std::fs::read(&path) {
            Ok(src_bytes) => run_file(src_bytes, &path, &interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
    }
}

fn run_file(src_bytes: Vec<u8>, path: &PathBuf, interrupted: &Arc<AtomicBool>) {
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
                        eprintln!("--> {}", position.path.display());

                        let display_line = line_of_position(&src, &position);
                        let formatted_line_num = format!("{} | ", display_line.line_num + 1);
                        eprintln!("{}{}", formatted_line_num, display_line.src);

                        let caret_space =
                            " ".repeat(formatted_line_num.len() + display_line.offset_on_line);
                        let caret_len = max(
                            1,
                            display_line.end_offset_on_line - display_line.offset_on_line,
                        );
                        eprintln!("{}{}", caret_space, "^".repeat(caret_len));

                        eprintln!("Error: {}", e);
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                    Err(EvalError::Stop(_)) => {
                        eprintln!("Error (stopped)");
                    }
                }

                let main_call_exprs =
                    parse_def_or_expr_from_str(&PathBuf::from("__main_fun__"), "main();").unwrap();
                match eval_def_or_exprs(&main_call_exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, e)) => {
                        eprintln!("--> {}", position.path.display());

                        // TODO: this is the wrong src if the position
                        // is in the main call itself, e.g. if the
                        // user has incorrectly defined main() with
                        // more parameters.
                        let display_line = line_of_position(&src, &position);
                        let formatted_line_num = format!("{} | ", display_line.line_num + 1);
                        eprintln!("{}{}", formatted_line_num, display_line.src);

                        let caret_space =
                            " ".repeat(formatted_line_num.len() + display_line.offset_on_line);
                        let caret_len = max(
                            1,
                            display_line.end_offset_on_line - display_line.offset_on_line,
                        );
                        eprintln!("{}{}", caret_space, "^".repeat(caret_len));

                        eprintln!("Error: {}", e);
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                    Err(EvalError::Stop(_)) => {
                        eprintln!("Error (stopped)");
                    }
                }
            }
            Err(parse::ParseError::OtherError(pos, e)) => {
                eprintln!("--> {}", pos.path.display());

                let display_line = line_of_position(&src, &pos);
                let formatted_line_num = format!("{} | ", display_line.line_num + 1);
                eprintln!("{}{}", formatted_line_num, display_line.src);

                let caret_space =
                    " ".repeat(formatted_line_num.len() + display_line.offset_on_line);
                let caret_len = max(
                    1,
                    display_line.end_offset_on_line - display_line.offset_on_line,
                );
                eprintln!("{}{}", caret_space, "^".repeat(caret_len));

                eprintln!("\nParse error: {}", e);
            }
            Err(parse::ParseError::Incomplete(e)) => {
                eprintln!("Parse error (incomplete input): {}", e);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}
