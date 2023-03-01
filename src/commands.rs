use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{eval::Env, parse::parse_toplevel_from_str};

#[derive(Debug, EnumIter)]
pub enum Commands {
    Help,
    Globals,
    Locals,
    Parse(String),
    Source,
    Stack,
}

impl Commands {
    pub fn from_string(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            ":help" => Some(Commands::Help),
            ":globals" => Some(Commands::Globals),
            ":locals" => Some(Commands::Locals),
            ":source" => Some(Commands::Source),
            ":stack" => Some(Commands::Stack),
            _ => {
                if let Some(src) = s.strip_prefix(":parse ") {
                    Some(Commands::Parse(src.to_owned()))
                } else {
                    None
                }
            }
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Commands::Help => ":help",
            Commands::Globals => ":globals",
            Commands::Locals => ":locals",
            Commands::Parse(_) => ":parse",
            Commands::Source => ":source",
            Commands::Stack => ":stack",
        }
    }
}

pub fn run_if_command(input: &str, env: &Env, complete_src: &str) -> bool {
    match Commands::from_string(&input) {
        Some(Commands::Help) => {
            print!("The available commands are:");
            for command in Commands::iter() {
                print!(" {}", command.to_string().green());
            }
            println!();
            true
        }
        None if input.starts_with(':') => {
            println!("I don't know of any commands with that syntax.\n");
            print!("The available commands are:");
            for command in Commands::iter() {
                print!(" {}", command.to_string().green());
            }
            println!();
            true
        }
        Some(Commands::Source) => {
            print!("{}", complete_src);
            true
        }
        Some(Commands::Globals) => {
            for (var_name, value) in &env.file_scope {
                println!("{}\t{}", var_name.0.bright_green(), value);
            }

            true
        }
        Some(Commands::Locals) => {
            if let Some((_, fun_scope)) = env.fun_scopes.last() {
                for (var_name, value) in fun_scope {
                    println!("{}\t{}", var_name.0.bright_green(), value);
                }
            }

            true
        }
        Some(Commands::Stack) => {
            print_stack(env);
            true
        }
        Some(Commands::Parse(src)) => {
            match parse_toplevel_from_str(&src) {
                Ok(ast) => println!("{:?}", ast),
                Err(e) => {
                    println!("{}: {}", "Error".bright_red(), e);
                }
            };
            true
        }
        None => false,
    }
}

pub fn print_stack(env: &Env) {
    for (description, _) in env.fun_scopes.iter().rev() {
        println!("In {}", description.0);
    }
}
