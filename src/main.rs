mod eval;
mod parse;
mod prompt;

use std::fs::OpenOptions;
use std::io::Write;

use crate::{
    eval::{eval_stmts, Env},
    parse::{lex, parse_toplevel},
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;
use rustyline::Editor;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, EnumIter)]
enum Commands {
    Help,
    Globals,
    Locals,
    Parse(String),
    Source,
}

impl Commands {
    fn from_string(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            ":help" => Some(Commands::Help),
            ":globals" => Some(Commands::Globals),
            ":locals" => Some(Commands::Locals),
            ":source" => Some(Commands::Source),
            _ => {
                if let Some(src) = s.strip_prefix(":parse ") {
                    Some(Commands::Parse(src.to_owned()))
                } else {
                    None
                }
            }
        }
    }

    fn to_string(&self) -> &str {
        match self {
            Commands::Help => ":help",
            Commands::Globals => ":globals",
            Commands::Locals => ":locals",
            Commands::Parse(_) => ":parse",
            Commands::Source => ":source",
        }
    }
}

fn main() {
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

                let input = input.trim().to_string();

                match Commands::from_string(&input) {
                    Some(Commands::Help) => {
                        print!("Available commands are:");
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
                            println!("{}\t{}", var_name.bright_green(), value);
                        }

                        continue;
                    }
                    Some(Commands::Locals) => {
                        if let Some((_, fun_scope)) = env.fun_scopes.last() {
                            for (var_name, value) in fun_scope {
                                println!("{}\t{}", var_name.bright_green(), value);
                            }
                        }

                        continue;
                    }
                    Some(Commands::Parse(src)) => {
                        let tokens = match lex(&src) {
                            Ok(tokens) => tokens,
                            Err(e) => {
                                println!("{}: {}", "Error".bright_red(), e);
                                continue;
                            }
                        };
                        let mut token_ptr = &tokens[..];

                        match parse_toplevel(&mut token_ptr) {
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

                let tokens = match lex(&input) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        println!("{}: {}", "Error".bright_red(), e);
                        continue;
                    }
                };
                let mut token_ptr = &tokens[..];

                match parse_toplevel(&mut token_ptr) {
                    Ok(stmts) => {
                        log_src(input).unwrap();
                        match eval_stmts(&stmts, &mut env) {
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

    let _ = rl.save_history(".history");
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
        println!("In {}", description);
    }
}
