use std::fs::OpenOptions;
use std::io::Write;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::commands::{print_available_commands, print_stack, run_command, Command, CommandError};
use crate::eval::EvalError;
use crate::eval::{self, eval_defs, eval_env, Session};
use crate::parse::{
    parse_def_or_expr_from_str, DefinitionsOrExpression, ParseError, Statement, Statement_,
};
use crate::{eval::Env, prompt::prompt_symbol};
use owo_colors::OwoColorize;
use rustyline::Editor;

enum ReadError {
    Aborted,
    Resumed,
    ReadlineError,
}

fn read_expr(
    env: &mut Env,
    session: &mut Session,
    rl: &mut Editor<()>,
    depth: usize,
) -> Result<DefinitionsOrExpression, ReadError> {
    loop {
        match rl.readline(&prompt_symbol(depth)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let _ = rl.save_history(".history");

                match Command::from_string(&input) {
                    Some(cmd) => match run_command(&mut std::io::stdout(), &cmd, env, &session) {
                        Ok(()) => {
                            println!();
                            println!();
                            continue;
                        }
                        Err(CommandError::Abort) => {
                            return Err(ReadError::Aborted);
                        }
                        Err(CommandError::Resume) => {
                            return Err(ReadError::Resumed);
                        }
                    },
                    None => {
                        if input.trim().starts_with(':') {
                            print_available_commands(&mut std::io::stdout());
                            println!();
                            continue;
                        }
                    }
                }

                match read_multiline_syntax(&input, rl) {
                    Ok((src, items)) => {
                        session.history.push_str(&src);
                        session.history.push('\n');
                        log_src(src).unwrap();

                        return Ok(items);
                    }
                    Err(ParseError::Incomplete(e)) => {
                        println!("Parsing failed (incomplete): {}", e);
                    }
                    Err(ParseError::OtherError(e)) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(_) => return Err(ReadError::ReadlineError),
        }

        println!();
    }
}

pub fn repl(interrupted: &Arc<AtomicBool>) {
    print_repl_header();

    let mut env = Env::default();
    let mut session = Session {
        history: String::new(),
        interrupted,
        has_attached_stdout: true,
    };

    let mut rl = new_editor();
    let mut depth = 0;
    loop {
        println!();

        match read_expr(&mut env, &mut session, &mut rl, depth) {
            Ok(items) => match items.clone() {
                DefinitionsOrExpression::Defs(defs) => {
                    eval_defs(&defs, &mut env);
                    continue;
                }
                DefinitionsOrExpression::Expr(expr) => {
                    let stack_frame = env.stack.last_mut().unwrap();
                    stack_frame
                        .stmts_to_eval
                        .push((false, Statement(expr.0, Statement_::Expr(expr))));
                }
            },
            Err(ReadError::Aborted) => {
                // TODO: doesn't this need to pop the stack to the toplevel?
                // It seems to be working already.
                depth = 0;
                continue;
            }
            Err(ReadError::Resumed) => {
                let stack_frame = env.stack.last_mut().unwrap();
                let (_, stmt) = stack_frame.stmts_to_eval.pop().unwrap();
                assert!(matches!(stmt.1, Statement_::FinishedLastInput));
            }
            Err(ReadError::ReadlineError) => {
                break;
            }
        }

        match eval_env(&mut env, &mut session) {
            Ok(result) => {
                match result {
                    eval::Value::Void => {}
                    v => {
                        println!("{}", v)
                    }
                }
                depth = 0;
            }
            Err(EvalError::ResumableError(msg)) => {
                println!("{}: {}", "Error".bright_red(), msg);
                println!("Resumable error.");
                depth += 1;
            }
            Err(EvalError::UserError(e)) => {
                // Unrecoverable.
                println!("{}: {}", "Error".bright_red(), e);
                print_stack(&mut std::io::stdout(), &env);
                depth = 0;
            }
            Err(EvalError::FinishedLastInput) => {
                let stack_frame = env.stack.last_mut().unwrap();
                let result = stack_frame.evalled_values.pop().unwrap();
                match result {
                    eval::Value::Void => {
                        println!("void")
                    }
                    v => {
                        println!("{}", v)
                    }
                }
                println!("Finished with input. Now what?");
            }
        }
    }
}

fn new_editor() -> Editor<()> {
    let mut rl: Editor<()> = Editor::new().unwrap();
    // TODO: put this in the home directory rather than the current directory.
    let _ = rl.load_history(".history");
    rl
}

fn print_repl_header() {
    println!(
        "{} {}{}",
        "Garden".bold().green(),
        env!("CARGO_PKG_VERSION").bold(),
        ": good programs take time to grow.".bold()
    );
    println!("Type {} if you're new here.", ":help".bold().green());
}

fn read_multiline_syntax(
    first_line: &str,
    rl: &mut Editor<()>,
) -> Result<(String, DefinitionsOrExpression), ParseError> {
    let mut src = first_line.to_string();

    loop {
        match parse_def_or_expr_from_str(&src) {
            Ok(items) => match items {
                DefinitionsOrExpression::Defs(ref defs)
                    if defs.is_empty() && !src.trim().is_empty() =>
                {
                    // If we didn't parse anything, but the text isn't
                    // just whitespace, it's probably a comment that
                    // will become a doc comment
                    match rl.readline(&prompt_symbol(1)) {
                        Ok(input) => {
                            src.push('\n');
                            src.push_str(&input);
                        }
                        Err(_) => return Ok((src, items)),
                    }
                }
                _ => {
                    return Ok((src, items));
                }
            },
            Err(e @ ParseError::Incomplete(_)) => match rl.readline(&prompt_symbol(1)) {
                Ok(input) => {
                    src.push('\n');
                    src.push_str(&input);
                }
                Err(_) => return Err(e),
            },
            Err(e @ ParseError::OtherError(_)) => {
                return Err(e);
            }
        }
    }
}

fn log_src(src: String) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("log.gdn")?;

    write!(file, "\n{}", src)
}
