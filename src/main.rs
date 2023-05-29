mod colors;
mod commands;
mod eval;
mod interactive_session;
mod json_session;
mod parse;
mod prompt;

use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::{Parser, Subcommand};
use eval::{eval_def_or_exprs, Env, EvalError, Session};
use parse::parse_def_or_expr_from_str;

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
    }
}

fn line_of_offset(src: &str, byte_offset: usize) -> (&str, usize, usize) {
    let mut line_start_offset = 0;

    for (i, line) in src.lines().enumerate() {
        if line.len() + line_start_offset >= byte_offset {
            return (line, i, byte_offset - line_start_offset);
        }

        // TODO: This is wrong if src contains \r\n newlines.
        line_start_offset += line.len() + 1;
    }

    let last_line = match src.lines().last() {
        Some(line) => line,
        None => src, // empty string
    };

    (last_line, 0, last_line.len())
}

fn run_file(src_bytes: Vec<u8>, path: &PathBuf, interrupted: &Arc<AtomicBool>) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_def_or_expr_from_str(path, &src) {
            Ok(stmts) => {
                let mut env = Env::default();
                let mut session = Session {
                    history: src.clone(),
                    interrupted,
                    has_attached_stdout: true,
                };

                // TODO: files should only contain defs, not
                // expressions. Ignore the expressions.
                match eval_def_or_exprs(&stmts, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(e)) => {
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
                    Err(EvalError::ResumableError(e)) => {
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

                let (line_src, line_i, line_offset) = line_of_offset(&src, pos.offset);
                let formatted_line_num = format!("{} | ", line_i + 1);
                eprintln!("{}{}", formatted_line_num, line_src);

                let caret_space = " ".repeat(formatted_line_num.len() + line_offset);
                eprintln!("{}^", caret_space);

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
