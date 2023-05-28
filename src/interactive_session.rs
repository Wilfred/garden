use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::commands::{
    print_available_commands, run_command, Command, CommandError, CommandParseError,
};
use crate::eval::{self, eval_defs, eval_env, Session};
use crate::eval::{ErrorKind, EvalError};
use crate::parse::{
    parse_def_or_expr_from_str, DefinitionsOrExpression, Expression, Expression_, ParseError,
};
use crate::{eval::Env, prompt::prompt_symbol};
use owo_colors::OwoColorize;
use rustyline::Editor;

enum ReadError {
    CommandError(CommandError),
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
                    Ok(cmd) => match run_command(&mut std::io::stdout(), &cmd, env, &session) {
                        Ok(()) => {
                            println!();
                            println!();
                            continue;
                        }
                        Err(e) => {
                            return Err(ReadError::CommandError(e));
                        }
                    },
                    Err(CommandParseError::NoSuchCommand) => {
                        print_available_commands(&mut std::io::stdout());
                        println!();
                        continue;
                    }
                    Err(CommandParseError::NotCommandSyntax) => {
                        // Continue with expression parsing.
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
                    Err(ParseError::OtherError(_pos, e)) => {
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
                    let stack_frame = env
                        .stack
                        .last_mut()
                        .expect("Should always have the toplevel stack frame");
                    stack_frame.exprs_to_eval.push((false, expr));
                }
            },
            Err(ReadError::CommandError(CommandError::Abort)) => {
                // TODO: doesn't this need to pop the stack to the toplevel?
                // It seems to be working already.
                depth = 0;
                continue;
            }
            Err(ReadError::CommandError(CommandError::Resume)) => {
                if depth > 0 {
                    depth -= 1;
                }

                let stack_frame = env.stack.last_mut().unwrap();
                if let Some((_, expr)) = stack_frame.exprs_to_eval.pop() {
                    assert!(matches!(expr.1, Expression_::Stop(_)));
                } else {
                    continue;
                }
            }
            Err(ReadError::CommandError(CommandError::Replace(expr))) => {
                let stack_frame = env.stack.last_mut().unwrap();

                let err_kind = if let Some((_, Expression(_, Expression_::Stop(e)))) =
                    stack_frame.exprs_to_eval.last()
                {
                    e.clone()
                } else {
                    println!(":replace failed: expected to be at an evaluation stopping point");
                    continue;
                };

                match err_kind {
                    Some(err_kind) => {
                        stack_frame.exprs_to_eval.pop();
                        match err_kind {
                            ErrorKind::BadValue => {
                                stack_frame.evalled_values.pop();
                            }
                            ErrorKind::MalformedExpression => {
                                stack_frame.exprs_to_eval.pop();
                            }
                        }
                        stack_frame.exprs_to_eval.push((false, expr));
                    }
                    None => {
                        println!(":replace failed: can't replace without an error.");
                        continue;
                    }
                }
            }
            Err(ReadError::CommandError(CommandError::Skip)) => {
                let stack_frame = env.stack.last_mut().unwrap();
                // Skip the Stop statement.
                stack_frame.exprs_to_eval.pop();

                stack_frame
                    .exprs_to_eval
                    .pop()
                    .expect("Tried to skip an expression, but none in this frame.");

                if depth > 0 {
                    depth -= 1;
                }
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
                depth += 1;
            }
            Err(EvalError::Interrupted) => {
                println!("Interrupted. You can take a look around, or use :resume to continue.");
                depth += 1;
            }
            Err(EvalError::Stop(_)) => {
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
        match parse_def_or_expr_from_str(&PathBuf::from("__interactive_session__"), &src) {
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
            Err(e @ ParseError::OtherError(_, _)) => {
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
