use std::io::Write;

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    eval::{builtin_fun_doc, Env, Session, Value},
    parse::{parse_def_or_expr_from_str, ParseError, VariableName},
};

#[derive(Debug, EnumIter)]
pub enum Command {
    Abort,
    Doc(String),
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
                } else if let Some(src) = s.strip_prefix(":doc ") {
                    Some(Command::Doc(src.to_owned()))
                } else {
                    None
                }
            }
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Command::Abort => ":abort",
            Command::Doc(_) => ":doc",
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
}

#[derive(Debug)]
pub enum CommandError {
    Abort,
}

pub fn run_command<T: Write>(
    buf: &mut T,
    cmd: &Command,
    env: &Env,
    session: &Session,
) -> Result<(), CommandError> {
    match cmd {
        Command::Help => {
            write!(buf, "{}", HELP_TOPICS[0].1).unwrap();
            print_available_commands(buf);
        }
        Command::Doc(name) => {
            if let Some(value) = env.file_scope.get(&VariableName(name.to_string())) {
                match value {
                    Value::Fun(doc_comment, name, _, _) => match doc_comment {
                        Some(doc_comment) => write!(buf, "{}", doc_comment),
                        None => writeln!(buf, "`{}` has no documentation comment.", name.0),
                    }
                    .unwrap(),
                    Value::BuiltinFunction(kind) => {
                        write!(buf, "{}", builtin_fun_doc(kind)).unwrap()
                    }
                    _ => {
                        write!(buf, "`{}` is not a function.", name).unwrap();
                    }
                }
            } else {
                write!(buf, "No function defined named `{}`.", name).unwrap();
            }
        }
        Command::Source => {
            write!(buf, "{}", session.history).unwrap();
        }
        Command::Globals => {
            for (i, (var_name, value)) in env.file_scope.iter().enumerate() {
                write!(
                    buf,
                    "{}{}\t{}",
                    if i == 0 { "" } else { "\n" },
                    var_name.0.bright_green(),
                    value
                )
                .unwrap();
            }
        }
        Command::Locals => {
            if let Some((_, fun_scope)) = env.fun_scopes.last() {
                for (i, (var_name, value)) in fun_scope.iter().enumerate() {
                    write!(
                        buf,
                        "{}{}\t{}",
                        if i == 0 { "" } else { "\n" },
                        var_name.0.bright_green(),
                        value
                    )
                    .unwrap();
                }
            }
        }
        Command::Stack => {
            print_stack(buf, env);
        }
        Command::Parse(src) => {
            match parse_def_or_expr_from_str(&src) {
                Ok(ast) => write!(buf, "{:?}", ast).unwrap(),
                Err(ParseError::Incomplete(e)) | Err(ParseError::OtherError(e)) => {
                    write!(buf, "{}: {}", "Error".bright_red(), e).unwrap();
                }
            };
        }
        Command::Abort => {
            return Err(CommandError::Abort);
        }
        Command::Quit => {
            std::process::exit(0);
        }
    }
    Ok(())
}

pub fn print_stack<T: Write>(buf: &mut T, env: &Env) {
    for (i, (description, _)) in env.fun_scopes.iter().rev().enumerate() {
        write!(
            buf,
            "{}In {}",
            if i == 0 { "" } else { "\n" },
            description.0
        )
        .unwrap();
    }
}
