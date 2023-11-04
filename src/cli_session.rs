use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use crate::ast::ToplevelItem;
use crate::commands::{
    print_available_commands, run_command, Command, CommandParseError, EvalAction,
};
use crate::diagnostics::format_error_with_stack;
use crate::env::Env;
use crate::eval::{eval_env, eval_toplevel_defs, Session};
use crate::eval::{push_test_stackframe, EvalError};
use crate::parse::{parse_toplevel_items, ParseError};
use crate::prompt::prompt_symbol;
use crate::values::Value;

use owo_colors::OwoColorize;
use rustyline::Editor;

enum ReadError {
    NeedsEval(EvalAction),
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
                            return Err(ReadError::NeedsEval(e));
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
                    Err(ParseError::Incomplete { message: e, .. }) => {
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
        start_time: Instant::now(),
        trace_exprs: false,
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
                    if let ToplevelItem::Expr(expr) = item {
                        exprs.push(expr);
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
            Err(ReadError::NeedsEval(EvalAction::Abort)) => {
                // TODO: doesn't this need to pop the stack to the toplevel?
                // It seems to be working already.
                is_stopped = false;
                continue;
            }
            Err(ReadError::NeedsEval(EvalAction::Resume)) => {
                // Continue to eval_env below.
            }
            Err(ReadError::NeedsEval(EvalAction::Replace(expr))) => {
                let stack_frame = env.stack.last_mut().unwrap();

                stack_frame.evalled_values.pop();
                stack_frame.exprs_to_eval.push((false, expr));

                // TODO: Prevent :replace when we've not just halted.
            }
            Err(ReadError::NeedsEval(EvalAction::Skip)) => {
                let stack_frame = env.stack.last_mut().unwrap();

                stack_frame
                    .exprs_to_eval
                    .pop()
                    .expect("Tried to skip an expression, but none in this frame.");
            }
            Err(ReadError::NeedsEval(EvalAction::RunTest(name))) => {
                // Push test then continue to eval_env().
                let test = match env.tests.get(&name) {
                    Some(test) => test.clone(),
                    None => {
                        println!("No such test: {}", name.0);
                        continue;
                    }
                };

                push_test_stackframe(&test, &mut env);
            }
            Err(ReadError::ReadlineError) => {
                break;
            }
        }

        match eval_env(&mut env, &mut session) {
            Ok(result) => {
                match result {
                    Value::Void => {}
                    v => {
                        println!("{}", v.display(&env))
                    }
                }
                is_stopped = false;
            }
            Err(EvalError::ResumableError(position, msg)) => {
                // TODO: this assumes the bad position occurs in the most recent input,
                // not e.g. in an earlier function definition.
                let _ = last_src; // should use this.
                println!("{}", &format_error_with_stack(&msg, &position, &env.stack));
                is_stopped = true;
            }
            Err(EvalError::Interrupted) => {
                println!("Interrupted. You can take a look around, or use :resume to continue.");
                is_stopped = true;
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
        match parse_toplevel_items(&PathBuf::from("__interactive_session__"), &src) {
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
            Err(e @ ParseError::Incomplete { .. }) => match rl.readline(&prompt_symbol(false)) {
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
