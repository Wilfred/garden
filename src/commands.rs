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
    Doc(Option<String>),
    Help,
    Globals,
    Locals,
    Parse(Option<String>),
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
                // TODO: require a word break after :parse and :doc
                if let Some(src) = s.strip_prefix(":parse") {
                    Some(Command::Parse(Some(src.trim_start().to_owned())))
                } else if let Some(src) = s.strip_prefix(":doc") {
                    Some(Command::Doc(Some(src.trim_start().to_owned())))
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

fn describe_fun(value: &Value) -> Option<String> {
    match value {
        Value::Fun(doc_comment, name, params, _) => {
            let mut res = String::new();
            match doc_comment {
                Some(doc_comment) => {
                    res.push_str(&doc_comment);
                }
                None => res.push_str(&format!("`{}` has no documentation comment.", name.0)),
            }
            res.push_str("\n\n");

            res.push_str(&format!(
                "fn {}({}) {{ ... }}",
                name.0,
                params
                    .iter()
                    .map(|p| p.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
            Some(res)
        }
        Value::BuiltinFunction(kind) => {
            // TODO: show signature of built-in functions.
            Some(builtin_fun_doc(kind).to_owned())
        }
        _ => None,
    }
}

pub fn run_command<T: Write>(
    buf: &mut T,
    cmd: &Command,
    env: &mut Env,
    session: &Session,
) -> Result<(), CommandError> {
    match cmd {
        Command::Help => {
            write!(buf, "{} ", HELP_TOPICS[0].1).unwrap();
            print_available_commands(buf);
        }
        Command::Doc(name) => {
            if let Some(name) = name {
                if let Some(value) = env.file_scope.get(&VariableName(name.to_string())) {
                    match describe_fun(value) {
                        Some(description) => write!(buf, "{}", description),
                        None => write!(buf, "`{}` is not a function.", name),
                    }
                    .unwrap();
                } else {
                    write!(buf, "No function defined named `{}`.", name).unwrap();
                }
            } else {
                write!(buf, ":doc requires a name, e.g. `:doc print`").unwrap();
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
            if let Some(stack_frame) = env.stack.last() {
                for (i, (var_name, value)) in stack_frame.bindings.iter().enumerate() {
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
            if let Some(src) = src {
                match parse_def_or_expr_from_str(&src) {
                    Ok(ast) => write!(buf, "{:?}", ast).unwrap(),
                    Err(ParseError::Incomplete(e)) | Err(ParseError::OtherError(e)) => {
                        write!(buf, "{}: {}", "Error".bright_red(), e).unwrap();
                    }
                };
            } else {
                write!(buf, ":parse requires a code snippet, e.g. `:parse 1 + 2`").unwrap();
            }
        }
        Command::Abort => {
            env.pop_to_toplevel();
            return Err(CommandError::Abort);
        }
        Command::Quit => {
            std::process::exit(0);
        }
    }
    Ok(())
}

pub fn print_stack<T: Write>(buf: &mut T, env: &Env) {
    for (i, stack_frame) in env.stack.iter().rev().enumerate() {
        write!(
            buf,
            "{}In {}",
            if i == 0 { "" } else { "\n" },
            stack_frame.fun_name.0
        )
        .unwrap();
    }
}
