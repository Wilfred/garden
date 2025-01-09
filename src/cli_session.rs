use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use crate::commands::{
    print_available_commands, run_command, Command, CommandParseError, EvalAction,
};
use crate::diagnostics::format_error_with_stack;
use crate::env::Env;
use crate::eval::{eval, load_toplevel_items, ExpressionState, Session};
use crate::eval::{push_test_stackframe, EvalError};
use crate::prompt::prompt_symbol;
use garden_lang_parser::ast::{Definition_, IdGenerator, ToplevelItem};
use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_items, ParseError};

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
    id_gen: &mut IdGenerator,
) -> Result<(String, Vec<ToplevelItem>), ReadError> {
    loop {
        match rl.readline(&prompt_symbol(is_stopped)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let _ = rl.save_history(".history");

                match Command::from_string(&input) {
                    Ok(cmd) => {
                        match run_command(&mut std::io::stdout(), &cmd, env, session, id_gen) {
                            Ok(()) => {
                                println!();
                                println!();
                                continue;
                            }
                            Err(e) => {
                                return Err(ReadError::NeedsEval(e));
                            }
                        }
                    }
                    Err(CommandParseError::NoSuchCommand(s)) => {
                        print_available_commands(&s, &mut std::io::stdout()).unwrap();
                        println!();
                        continue;
                    }
                    Err(CommandParseError::NotCommandSyntax) => {
                        // Continue with expression parsing.
                    }
                }

                match read_multiline_syntax(&input, rl, id_gen) {
                    Ok((src, items)) => {
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

pub(crate) fn repl(interrupted: Arc<AtomicBool>) {
    print_repl_header();

    let mut id_gen = IdGenerator::default();
    let mut env = Env::new(&mut id_gen);
    let mut session = Session {
        interrupted,
        has_attached_stdout: true,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    let mut rl = new_editor();
    let mut is_stopped = false;
    let mut last_src = String::new();

    loop {
        println!();

        match read_expr(&mut env, &mut session, &mut rl, is_stopped, &mut id_gen) {
            Ok((src, items)) => {
                last_src = src;

                let summary = load_toplevel_items(&items, &mut env);
                for diagnostic in summary.diagnostics {
                    println!("Warning: {}", diagnostic.message);
                }

                let mut exprs = vec![];
                for item in items {
                    match item {
                        ToplevelItem(_, _, Definition_::Expr(e)) => {
                            exprs.push(e.clone());
                        }
                        _ => {}
                    }
                }

                if exprs.is_empty() {
                    continue;
                }

                let stack_frame = env
                    .stack
                    .0
                    .last_mut()
                    .expect("Should always have the toplevel stack frame");

                // Push expressions in reverse order, so the top of
                // exprs_to_eval is the first expression from the
                // user.
                for expr in exprs.iter().rev() {
                    stack_frame
                        .exprs_to_eval
                        .push((ExpressionState::NotEvaluated, expr.0.clone().into()));
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
                let stack_frame = env.stack.0.last_mut().unwrap();

                stack_frame.evalled_values.pop();
                stack_frame
                    .exprs_to_eval
                    .push((ExpressionState::NotEvaluated, expr.into()));

                // TODO: Prevent :replace when we've not just halted.
            }
            Err(ReadError::NeedsEval(EvalAction::Skip)) => {
                let stack_frame = env.stack.0.last_mut().unwrap();

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
                        println!("No such test: {}", name);
                        continue;
                    }
                };

                push_test_stackframe(&test, &mut env);
            }
            Err(ReadError::ReadlineError) => {
                break;
            }
        }

        match eval(&mut env, &session) {
            Ok(result) => {
                if let Some(display_str) = result.display_unless_unit(&env) {
                    println!("{}", display_str);
                }

                is_stopped = false;
            }
            Err(EvalError::ResumableError(position, msg)) => {
                // TODO: this assumes the bad position occurs in the most recent input,
                // not e.g. in an earlier function definition.
                let _ = last_src; // should use this.
                println!(
                    "{}",
                    &format_error_with_stack(&msg, &position, &env.stack.0)
                );
                is_stopped = true;
            }
            Err(EvalError::AssertionFailed(position)) => {
                let msg = ErrorMessage("Assertion failed".to_owned());
                println!(
                    "{}",
                    &format_error_with_stack(&msg, &position, &env.stack.0)
                );
                is_stopped = true;
            }
            Err(EvalError::Interrupted) => {
                println!("Interrupted. You can take a look around, or use :resume to continue.");
                is_stopped = true;
            }
            Err(EvalError::ReachedTickLimit) => {
                println!("Reached tick limit.");
                is_stopped = false;
            }
            Err(EvalError::ForbiddenInSandbox(pos)) => {
                println!(
                    "{}: This call is forbidden in a sandbox.",
                    pos.as_ide_string()
                );
                is_stopped = false;
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
    id_gen: &mut IdGenerator,
) -> Result<(String, Vec<ToplevelItem>), ParseError> {
    let mut src = first_line.to_owned();

    loop {
        let (items, errors) =
            parse_toplevel_items(&PathBuf::from("__interactive_session__"), &src, id_gen);

        // TODO: return all errors.
        match errors.into_iter().next() {
            None => {
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
            Some(e @ ParseError::Incomplete { .. }) => match rl.readline(&prompt_symbol(false)) {
                Ok(input) => {
                    src.push('\n');
                    src.push_str(&input);
                }
                Err(_) => return Err(e),
            },
            Some(e @ ParseError::Invalid { .. }) => return Err(e),
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
