use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    eval::Env,
    parse::{parse_toplevel_from_str, ParseError},
};

#[derive(Debug, EnumIter)]
pub enum Commands {
    Abort,
    Help,
    Globals,
    Locals,
    Parse(String),
    Source,
    Stack,
    Quit,
}

const HELP_TOPICS: &[(&str, &str)] = &[
    ("intro", "Garden is a programming language."),
    ("syntax", "Garden uses curly braces."),
];

impl Commands {
    pub fn from_string(s: &str) -> Option<Self> {
        match s.to_lowercase().trim() {
            ":abort" => Some(Commands::Abort),
            ":help" => Some(Commands::Help),
            ":globals" => Some(Commands::Globals),
            ":locals" => Some(Commands::Locals),
            ":source" => Some(Commands::Source),
            ":stack" => Some(Commands::Stack),
            ":quit" => Some(Commands::Quit),
            _ => {
                // TODO: allow :parse without any trailing whitespace.
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
            Commands::Abort => ":abort",
            Commands::Help => ":help",
            Commands::Globals => ":globals",
            Commands::Locals => ":locals",
            Commands::Parse(_) => ":parse",
            Commands::Source => ":source",
            Commands::Stack => ":stack",
            Commands::Quit => ":quit",
        }
    }
}

fn print_available_commands() {
    print!("The available commands are");

    let mut command_names: Vec<String> = Commands::iter().map(|c| c.to_string().into()).collect();
    command_names.sort();

    for (i, name) in command_names.iter().enumerate() {
        if i == command_names.len() - 1 {
            print!(" and {}.", name.green());
        } else if i == command_names.len() - 2 {
            print!(" {}", name.green());
        } else {
            print!(" {},", name.green());
        }
    }
    println!();
}

#[derive(Debug)]
pub enum CommandError {
    NotACommand,
    Abort,
}

pub fn run_if_command(input: &str, env: &Env, complete_src: &str) -> Result<(), CommandError> {
    match Commands::from_string(&input) {
        Some(Commands::Help) => {
            println!("{}\n", HELP_TOPICS[0].1);
            print_available_commands();
            Ok(())
        }
        Some(Commands::Source) => {
            print!("{}", complete_src);
            Ok(())
        }
        Some(Commands::Globals) => {
            for (var_name, value) in &env.file_scope {
                println!("{}\t{}", var_name.0.bright_green(), value);
            }

            Ok(())
        }
        Some(Commands::Locals) => {
            if let Some((_, fun_scope)) = env.fun_scopes.last() {
                for (var_name, value) in fun_scope {
                    println!("{}\t{}", var_name.0.bright_green(), value);
                }
            }

            Ok(())
        }
        Some(Commands::Stack) => {
            print_stack(env);
            Ok(())
        }
        Some(Commands::Parse(src)) => {
            match parse_toplevel_from_str(&src) {
                Ok(ast) => println!("{:?}", ast),
                Err(ParseError::Incomplete(e)) | Err(ParseError::OtherError(e)) => {
                    println!("{}: {}", "Error".bright_red(), e);
                }
            };
            Ok(())
        }
        Some(Commands::Abort) => Err(CommandError::Abort),
        Some(Commands::Quit) => {
            std::process::exit(0);
        }
        None if input.starts_with(':') => {
            print_available_commands();
            Ok(())
        }
        None => Err(CommandError::NotACommand),
    }
}

pub fn print_stack(env: &Env) {
    for (description, _) in env.fun_scopes.iter().rev() {
        println!("In {}", description.0);
    }
}
