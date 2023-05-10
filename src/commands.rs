use std::io::Write;

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    colors::green,
    eval::{builtin_fun_doc, Env, Session, Value},
    parse::{
        parse_def_or_expr_from_str, parse_expr_from_str, ParseError, Statement, Statement_,
        VariableName,
    },
};

#[derive(Debug, EnumIter)]
pub enum Command {
    Abort,
    Doc(Option<String>),
    Help,
    Globals,
    Locals,
    FrameValues,
    FrameStatements,
    Parse(Option<String>),
    Replace(Option<Statement>),
    Resume,
    Skip,
    Source,
    Trace,
    Stack,
    Quit,
}

const HELP_TOPICS: &[(&str, &str)] = &[
    ("intro", "Garden is a programming language."),
    ("syntax", "Garden uses curly braces."),
];

impl Command {
    pub fn from_string(s: &str) -> Result<Self, ()> {
        match s.to_lowercase().trim() {
            ":abort" => Ok(Command::Abort),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":globals" => Ok(Command::Globals),
            ":help" => Ok(Command::Help),
            ":locals" => Ok(Command::Locals),
            ":resume" => Ok(Command::Resume),
            ":skip" => Ok(Command::Skip),
            ":source" => Ok(Command::Source),
            ":stack" => Ok(Command::Stack),
            ":trace" => Ok(Command::Trace),
            ":quit" => Ok(Command::Quit),
            _ => {
                // TODO: require a word break after :parse and :doc
                if let Some(src) = s.strip_prefix(":parse") {
                    Ok(Command::Parse(Some(src.trim_start().to_owned())))
                } else if let Some(src) = s.strip_prefix(":replace") {
                    let src = src.trim_start().to_owned();
                    match parse_expr_from_str(&src) {
                        Ok(expr) => {
                            let stmt = Statement(expr.0, Statement_::Expr(expr));
                            Ok(Command::Replace(Some(stmt)))
                        }
                        Err(e) => {
                            // TODO: this breaks JSON sessions.
                            println!("Error during parse of replacement: {:?}", e);
                            Ok(Command::Replace(None))
                        }
                    }
                } else if let Some(src) = s.strip_prefix(":doc") {
                    Ok(Command::Doc(Some(src.trim_start().to_owned())))
                } else {
                    Err(())
                }
            }
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Command::Abort => ":abort",
            Command::Doc(_) => ":doc",
            Command::FrameStatements => ":fstmts",
            Command::FrameValues => ":fvalues",
            Command::Globals => ":globals",
            Command::Help => ":help",
            Command::Locals => ":locals",
            Command::Parse(_) => ":parse",
            Command::Replace(_) => ":replace",
            Command::Resume => ":resume",
            Command::Skip => ":skip",
            Command::Source => ":source",
            Command::Stack => ":stack",
            Command::Trace => ":trace",
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
            write!(buf, " and {}.", green(name)).unwrap();
        } else if i == command_names.len() - 2 {
            write!(buf, " {}", green(name)).unwrap();
        } else {
            write!(buf, " {},", green(name)).unwrap();
        }
    }
    write!(buf, "\n").unwrap();
}

#[derive(Debug)]
pub enum CommandError {
    Replace(Statement),
    Resume,
    Abort,
    Skip,
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
        Command::Replace(stmt) => {
            if let Some(stmt) = stmt {
                return Err(CommandError::Replace(stmt.clone()));
            } else {
                return Ok(());
            }
        }
        Command::Resume => {
            return Err(CommandError::Resume);
        }
        Command::Skip => {
            return Err(CommandError::Skip);
        }
        Command::Source => {
            write!(
                buf,
                "{}",
                if session.history == "" {
                    "(empty)"
                } else {
                    &session.history
                }
            )
            .unwrap();
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
        Command::FrameStatements => {
            if let Some(stack_frame) = env.stack.last() {
                for (_, stmt) in stack_frame.stmts_to_eval.iter().rev() {
                    writeln!(buf, "{:?}", stmt.1).unwrap();
                }
            }
        }
        Command::FrameValues => {
            if let Some(stack_frame) = env.stack.last() {
                for value in stack_frame.evalled_values.iter().rev() {
                    match value {
                        Value::Void => {
                            writeln!(buf, "void")
                        }
                        v => {
                            writeln!(buf, "{}", v)
                        }
                    }
                    .unwrap();
                }
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
        Command::Trace => {
            env.trace_exprs = !env.trace_exprs;
            write!(
                buf,
                "Expression tracing {}.",
                if env.trace_exprs {
                    "enabled"
                } else {
                    "disabled"
                }
            )
            .unwrap();
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
