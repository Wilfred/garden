use std::fs::OpenOptions;
use std::io::Write;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::commands::{print_available_commands, print_stack, run_command, Command, CommandError};
use crate::eval::{self, eval_env, Session};
use crate::eval::{eval_def_or_exprs, EvalError};
use crate::parse::{
    parse_def_or_expr_from_str, DefinitionsOrExpression, Expression, Expression_, ParseError,
    Statement, Statement_,
};
use crate::{eval::Env, prompt::prompt_symbol};
use owo_colors::OwoColorize;
use rustyline::Editor;

enum ReadError {
    Aborted,
    ReadlineError,
}

fn read_expr(
    env: &mut Env,
    session: &Session,
    rl: &mut Editor<()>,
    depth: usize,
) -> Result<(String, DefinitionsOrExpression), ReadError> {
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
                    },
                    None => {
                        if input.trim().starts_with(':') {
                            print_available_commands(&mut std::io::stdout());
                            continue;
                        }
                    }
                }

                match read_multiline_syntax(&input, rl) {
                    Ok((src, items)) => {
                        return Ok((src, items));
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

    let mut rl: Editor<()> = Editor::new().unwrap();
    // TODO: put this in the home directory rather than the current directory.
    let _ = rl.load_history(".history");

    loop {
        println!();

        match read_expr(&mut env, &session, &mut rl, 0) {
            Ok((src, items)) => {
                session.history.push_str(&src);
                session.history.push('\n');
                log_src(src).unwrap();

                match eval_def_or_exprs(&items, &mut env, &mut session) {
                    Ok(result) => match result {
                        eval::Value::Void => {}
                        v => {
                            println!("{}", v)
                        }
                    },
                    Err(EvalError::Aborted) => {}
                    Err(EvalError::ResumableError(msg)) => {
                        println!("{}", msg);

                        match read_expr(&mut env, &session, &mut rl, 1) {
                            Ok((_src, items)) => {
                                match items {
                                    DefinitionsOrExpression::Defs(_) => {
                                        // TODO: could probably just eval this def and try again.
                                    },
                                    DefinitionsOrExpression::Expr(expr) => {
                                        let stack_frame = env.stack.last_mut().unwrap();
                                        stack_frame.stmts_to_eval.push((
                                            false,
                                            Statement(expr.0, Statement_::Expr(expr)),
                                        ));
                                    }
                                }

                            }
                            Err(_) => {
                                break;
                            }
                        }

                        // TODO: loop.
                        if let Ok(result) = eval_env(&mut env, &mut session) {
                            match result {
                                eval::Value::Void => {}
                                v => {
                                    println!("{}", v)
                                }
                            }
                        } else {
                            println!("todo: handle error in evaluating after prompting user");
                        }
                    }
                    Err(EvalError::UserError(e)) => {
                        println!("{}: {}", "Error".bright_red(), e);
                        print_stack(&mut std::io::stdout(), &env);
                    }
                }
            }
            Err(ReadError::Aborted) => {
                // Nothing to do, we're in the top level.
                continue;
            }
            Err(ReadError::ReadlineError) => break,
        }
    }
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
