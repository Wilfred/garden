use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::ast::{self, ToplevelItem};
use crate::commands::{
    print_available_commands, run_command, Command, CommandError, CommandParseError,
};
use crate::eval::{self, eval_env, eval_toplevel_defs, Session};
use crate::eval::{ErrorKind, EvalError};
use crate::parse::{format_error, parse_toplevels, ParseError};
use crate::{eval::Env, prompt::prompt_symbol};

use owo_colors::OwoColorize;
use rustyline::Editor;

enum ReadError {
    CommandError(CommandError),
    ReadlineError,
}

/// Read toplevel items from stdin. If the user gives us a command,
/// execute it and prompt again.
fn read_expr(
    env: &mut Env,
    session: &mut Session,
    rl: &mut Editor<()>,
    is_stopped: bool,
) -> Result<(String, Vec<ToplevelItem>), ReadError> {
    loop {
        match rl.readline(&prompt_symbol(is_stopped)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let _ = rl.save_history(".history");

                match Command::from_string(&input) {
                    Ok(cmd) => match run_command(&mut std::io::stdout(), &cmd, env, session) {
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
                        log_src(&src).unwrap();

                        return Ok((src, items));
                    }
                    Err(ParseError::Incomplete(e)) => {
                        println!("Parsing failed (incomplete): {}", e.0);
                    }
                    Err(ParseError::Invalid { message: e, .. }) => {
                        println!("Parsing failed: {}", e.0);
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
    let mut is_stopped = false;
    let mut last_src = String::new();

    loop {
        println!();

        match read_expr(&mut env, &mut session, &mut rl, is_stopped) {
            Ok((src, items)) => {
                last_src = src;

                eval_toplevel_defs(&items, &mut env);

                let mut exprs = vec![];
                for item in items {
                    match item {
                        ToplevelItem::Expr(expr) => exprs.push(expr),
                        _ => {}
                    }
                }

                if exprs.is_empty() {
                    continue;
                }

                let stack_frame = env
                    .stack
                    .last_mut()
                    .expect("Should always have the toplevel stack frame");

                // Push expressions in reverse order, so the top of
                // exprs_to_eval is the first expression from the
                // user.
                for expr in exprs.iter().rev() {
                    stack_frame.exprs_to_eval.push((false, expr.1.clone()));
                }
            }
            Err(ReadError::CommandError(CommandError::Abort)) => {
                // TODO: doesn't this need to pop the stack to the toplevel?
                // It seems to be working already.
                is_stopped = false;
                continue;
            }
            Err(ReadError::CommandError(CommandError::Resume)) => {
                is_stopped = false;

                let stack_frame = env.stack.last_mut().unwrap();
                if let Some((_, expr)) = stack_frame.exprs_to_eval.pop() {
                    assert!(matches!(expr.1, ast::Expression_::Stop(_)));
                } else {
                    continue;
                }
            }
            Err(ReadError::CommandError(CommandError::Replace(expr))) => {
                let stack_frame = env.stack.last_mut().unwrap();

                let err_kind = if let Some((_, ast::Expression(_, ast::Expression_::Stop(e)))) =
                    stack_frame.exprs_to_eval.last()
                {
                    *e
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

                is_stopped = false;
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
                is_stopped = false;
            }
            Err(EvalError::ResumableError(position, msg)) => {
                // TODO: this assumes the bad position occurs in the most recent input,
                // not e.g. in an earlier function definition.
                println!("{}", &format_error(&msg, &position, &last_src));
                is_stopped = true;
            }
            Err(EvalError::Interrupted) => {
                println!("Interrupted. You can take a look around, or use :resume to continue.");
                is_stopped = true;
            }
            Err(EvalError::Stop(_)) => {
                let stack_frame = env.stack.last_mut().unwrap();
                let result = stack_frame.evalled_values.pop().unwrap();
                match result.1 {
                    eval::Value::Void => {
                        println!("void")
                    }
                    v => {
                        println!("{}", v)
                    }
                }
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

/// Read toplevel items from stdin.
///
/// If the user writes an incomplete item (e.g. the line ends with
/// `{`), then keep reading until we have a full definition or an
/// error.
fn read_multiline_syntax(
    first_line: &str,
    rl: &mut Editor<()>,
) -> Result<(String, Vec<ToplevelItem>), ParseError> {
    let mut src = first_line.to_string();

    loop {
        match parse_toplevels(&PathBuf::from("__interactive_session__"), &src) {
            Ok(items) => {
                if items.is_empty() && !src.trim().is_empty() {
                    // If we didn't parse anything, but the text isn't
                    // just whitespace, it's probably a comment that
                    // will become a doc comment.
                    match rl.readline(&prompt_symbol(false)) {
                        Ok(input) => {
                            src.push('\n');
                            src.push_str(&input);
                        }
                        Err(_) => return Ok((src, items)),
                    }
                } else {
                    return Ok((src, items));
                }
            }
            Err(e @ ParseError::Incomplete(_)) => match rl.readline(&prompt_symbol(false)) {
                Ok(input) => {
                    src.push('\n');
                    src.push_str(&input);
                }
                Err(_) => return Err(e),
            },
            Err(e @ ParseError::Invalid { .. }) => {
                return Err(e);
            }
        }
    }
}

fn log_src(src: &str) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("log.gdn")?;

    write!(file, "\n{}", src)
}
