mod commands;
mod eval;
mod parse;
mod prompt;

use std::fs::OpenOptions;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::commands::Commands;
use crate::{
    eval::{eval_stmts, Env},
    parse::parse_toplevel_from_str,
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;
use rustyline::Editor;
use strum::IntoEnumIterator;

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

                // TODO: factor out so this can be used in error interfaces too.
                match Commands::from_string(&input) {
                    Some(Commands::Help) => {
                        print!("The available commands are:");
                        for command in Commands::iter() {
                            print!(" {}", command.to_string().green());
                        }
                        println!();
                        continue;
                    }
                    None if input.starts_with(':') => {
                        println!("I don't know of any commands with that syntax.\n");
                        print!("The available commands are:");
                        for command in Commands::iter() {
                            print!(" {}", command.to_string().green());
                        }
                        println!();
                        continue;
                    }
                    Some(Commands::Source) => {
                        print!("{}", complete_src);
                        continue;
                    }
                    Some(Commands::Globals) => {
                        for (var_name, value) in &env.file_scope {
                            println!("{}\t{}", var_name.0.bright_green(), value);
                        }

                        continue;
                    }
                    Some(Commands::Locals) => {
                        if let Some((_, fun_scope)) = env.fun_scopes.last() {
                            for (var_name, value) in fun_scope {
                                println!("{}\t{}", var_name.0.bright_green(), value);
                            }
                        }

                        continue;
                    }
                    Some(Commands::Stack) => {
                        print_stack(&env);
                        continue;
                    }
                    Some(Commands::Parse(src)) => {
                        match parse_toplevel_from_str(&src) {
                            Ok(ast) => println!("{:?}", ast),
                            Err(e) => {
                                println!("{}: {}", "Error".bright_red(), e);
                                continue;
                            }
                        };
                        continue;
                    }
                    None => {}
                }

                complete_src.push_str(&input);
                complete_src.push('\n');

                match parse_toplevel_from_str(&input) {
                    Ok(stmts) => {
                        log_src(input).unwrap();
                        match eval_stmts(&stmts, &mut env, &interrupted) {
                            Ok(result) => match result {
                                eval::Value::Void => {}
                                v => {
                                    println!("{}", v)
                                }
                            },
                            Err(e) => {
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

fn print_stack(env: &Env) {
    for (description, _) in env.fun_scopes.iter().rev() {
        println!("In {}", description.0);
    }
}
