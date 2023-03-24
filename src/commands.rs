use std::io::Write;

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    eval::Env,
    parse::{parse_toplevel_from_str, ParseError},
};

#[derive(Debug, EnumIter)]
pub enum Command {
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

impl Command {
    pub fn from_string(s: &str) -> Option<Self> {
        match s.to_lowercase().trim() {
            ":abort" => Some(Command::Abort),
            ":help" => Some(Command::Help),
            ":globals" => Some(Command::Globals),
            ":locals" => Some(Command::Locals),
            ":source" => Some(Command::Source),
            ":stack" => Some(Command::Stack),
            ":quit" => Some(Command::Quit),
            _ => {
                // TODO: allow :parse without any trailing whitespace.
                if let Some(src) = s.strip_prefix(":parse ") {
                    Some(Command::Parse(src.to_owned()))
                } else {
                    None
                }
            }
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Command::Abort => ":abort",
            Command::Help => ":help",
            Command::Globals => ":globals",
            Command::Locals => ":locals",
            Command::Parse(_) => ":parse",
            Command::Source => ":source",
            Command::Stack => ":stack",
            Command::Quit => ":quit",
        }
    }
}

pub fn print_available_commands<T: Write>(buf: &mut T) {
    write!(buf, "The available commands are").unwrap();

    let mut command_names: Vec<String> = Command::iter().map(|c| c.to_string().into()).collect();
    command_names.sort();

    for (i, name) in command_names.iter().enumerate() {
        if i == command_names.len() - 1 {
            write!(buf, " and {}.", name.green()).unwrap();
        } else if i == command_names.len() - 2 {
            write!(buf, " {}", name.green()).unwrap();
        } else {
            write!(buf, " {},", name.green()).unwrap();
        }
    }
    writeln!(buf).unwrap();
}

#[derive(Debug)]
pub enum CommandError {
    NotACommand,
    Abort,
}

pub fn run_if_command(input: &str, env: &Env, complete_src: &str) -> Result<(), CommandError> {
    match Command::from_string(&input) {
        Some(Command::Help) => {
            println!("{}\n", HELP_TOPICS[0].1);
            print_available_commands(&mut std::io::stdout());
            Ok(())
        }
        Some(Command::Source) => {
            print!("{}", complete_src);
            Ok(())
        }
        Some(Command::Globals) => {
            for (var_name, value) in &env.file_scope {
                println!("{}\t{}", var_name.0.bright_green(), value);
            }

            Ok(())
        }
        Some(Command::Locals) => {
            if let Some((_, fun_scope)) = env.fun_scopes.last() {
                for (var_name, value) in fun_scope {
                    println!("{}\t{}", var_name.0.bright_green(), value);
                }
            }

            Ok(())
        }
        Some(Command::Stack) => {
            print_stack(env);
            Ok(())
        }
        Some(Command::Parse(src)) => {
            match parse_toplevel_from_str(&src) {
                Ok(ast) => println!("{:?}", ast),
                Err(ParseError::Incomplete(e)) | Err(ParseError::OtherError(e)) => {
                    println!("{}: {}", "Error".bright_red(), e);
                }
            };
            Ok(())
        }
        Some(Command::Abort) => Err(CommandError::Abort),
        Some(Command::Quit) => {
            std::process::exit(0);
        }
        None => Err(CommandError::NotACommand),
    }
}

pub fn print_stack(env: &Env) {
    for (description, _) in env.fun_scopes.iter().rev() {
        println!("In {}", description.0);
    }
}
