use std::fs::OpenOptions;
use std::io::Write;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::commands::{print_stack, run_if_command, CommandError};
use crate::eval;
use crate::eval::EvalError;
use crate::parse::ParseError;
use crate::parse::Statement;
use crate::{
    eval::{eval_stmts, Env},
    parse::parse_toplevel_from_str,
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;
use rustyline::Editor;

pub fn repl(interrupted: &Arc<AtomicBool>) {
    println!(
        "{} {}{}",
        "Garden".bold().green(),
        env!("CARGO_PKG_VERSION").bold(),
        ": good programs take time to grow.".bold()
    );
    println!("Type {} if you're new here.", ":help".bold().green(),);

    let mut env = Env::default();
    let mut complete_src = String::new();

    let mut rl: Editor<()> = Editor::new().unwrap();
    // TODO: put this in the home directory rather than the current directory.
    let _ = rl.load_history(".history");

    loop {
        println!();

        match rl.readline(&prompt_symbol(0)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let _ = rl.save_history(".history");

                match run_if_command(&input, &env, &complete_src) {
                    Ok(()) => {
                        continue;
                    }
                    Err(CommandError::NotACommand) => {}
                    Err(CommandError::Abort) => {
                        // Nothing to do, we're in the top level.
                    }
                }

                match read_multiline_syntax(&input, &mut rl) {
                    Ok((src, stmts)) => {
                        complete_src.push_str(&src);
                        complete_src.push('\n');
                        log_src(input).unwrap();

                        match eval_stmts(&stmts, &mut env, &complete_src, &interrupted) {
                            Ok(result) => match result {
                                eval::Value::Void => {}
                                v => {
                                    println!("{}", v)
                                }
                            },
                            Err(EvalError::Aborted) => {}
                            Err(EvalError::UserError(e)) => {
                                println!("{}: {}", "Error".bright_red(), e);
                                print_stack(&env);
                            }
                        }
                    }
                    Err(ParseError::Incomplete(e)) => {
                        println!("Parsing failed: {}", e);
                    }
                    Err(ParseError::OtherError(e)) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(_) => break,
        }
    }
}

fn read_multiline_syntax(
    first_line: &str,
    rl: &mut Editor<()>,
) -> Result<(String, Vec<Statement>), ParseError> {
    let mut src = first_line.to_string();

    loop {
        match parse_toplevel_from_str(&src) {
            Ok(stmts) => {
                return Ok((src, stmts));
            }
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
