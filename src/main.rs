mod commands;
mod eval;
mod parse;
mod prompt;

use std::fs::OpenOptions;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::commands::{print_stack, run_if_command, CommandError};
use crate::eval::EvalError;
use crate::{
    eval::{eval_stmts, Env},
    parse::parse_toplevel_from_str,
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;
use rustyline::Editor;

fn main() {
    let interrupted = Arc::new(AtomicBool::new(false));

    let i = interrupted.clone();
    ctrlc::set_handler(move || {
        i.store(true, Ordering::SeqCst);
    })
    .expect("Error setting Ctrl-C handler");

    println!(
        "{} {}{}",
        "Welcome to the".bold(),
        "garden".bold().green(),
        "!".bold()
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

                let input = input.trim().to_string();

                match run_if_command(&input, &env, &complete_src) {
                    Ok(()) => {
                        continue;
                    }
                    Err(CommandError::NotACommand) => {}
                    Err(CommandError::Abort) => {
                        // Nothing to do, we're in the top level.
                    }
                }

                complete_src.push_str(&input);
                complete_src.push('\n');

                match parse_toplevel_from_str(&input) {
                    Ok(stmts) => {
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
                    Err(e) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(_) => break,
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
