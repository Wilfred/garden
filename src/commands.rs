use std::{io::Write, path::PathBuf};

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    colors::green,
    eval::{builtin_fun_doc, Env, Session, Value},
    parse::{
        parse_def_or_expr_from_str, parse_inline_expr_from_str, Expression, ParseError,
        VariableName,
    },
};

#[derive(Debug, EnumIter)]
pub enum Command {
    Abort,
    Doc(Option<String>),
    Help(Option<String>),
    Globals,
    Locals,
    FrameValues,
    FrameStatements,
    Parse(Option<String>),
    Replace(Option<Expression>),
    Resume,
    Skip,
    Source,
    Trace,
    Stack,
    Quit,
}

/// Returns Some if `s` starts with `word` and is followed by a word
/// boundary.
fn split_first_word<'a>(s: &'a str, word: &str) -> Option<&'a str> {
    if let Some(suffix) = s.strip_prefix(word) {
        if suffix == "" {
            return Some("");
        }

        if let Some(rest) = suffix.strip_prefix(" ") {
            Some(rest)
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(Debug)]
pub enum CommandParseError {
    NoSuchCommand,
    NotCommandSyntax,
}

impl Command {
    pub fn from_string(s: &str) -> Result<Self, CommandParseError> {
        match s.to_lowercase().trim() {
            ":abort" => Ok(Command::Abort),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":globals" => Ok(Command::Globals),
            ":locals" => Ok(Command::Locals),
            ":resume" => Ok(Command::Resume),
            ":skip" => Ok(Command::Skip),
            ":source" => Ok(Command::Source),
            ":stack" => Ok(Command::Stack),
            ":trace" => Ok(Command::Trace),
            ":quit" => Ok(Command::Quit),
            _ => {
                if let Some(src) = split_first_word(s, ":doc") {
                    return Ok(Command::Doc(Some(src.to_owned())));
                }
                if let Some(src) = split_first_word(s, ":help") {
                    return Ok(Command::Help(Some(src.to_owned())));
                }
                if let Some(src) = split_first_word(s, ":parse") {
                    return Ok(Command::Parse(Some(src.to_owned())));
                }
                if let Some(src) = split_first_word(s, ":replace") {
                    // TODO: find a better name for this.
                    return match parse_inline_expr_from_str(
                        &PathBuf::from("__interactive_inline__"),
                        &src,
                    ) {
                        Ok(expr) => Ok(Command::Replace(Some(expr))),
                        Err(_) => Ok(Command::Replace(None)),
                    };
                }

                if s.starts_with(":") {
                    Err(CommandParseError::NoSuchCommand)
                } else {
                    Err(CommandParseError::NotCommandSyntax)
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
            Command::Help(_) => ":help",
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
}

#[derive(Debug)]
pub enum CommandError {
    Replace(Expression),
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
        Command::Help(text) => {
            let text = text.clone().unwrap_or_default();

            match Command::from_string(&text) {
                Ok(command) => {
                    write!(buf, "{}", command_help(command)).unwrap();
                }
                Err(CommandParseError::NoSuchCommand) => {
                    print_available_commands(buf);
                }
                Err(CommandParseError::NotCommandSyntax) => {
                    write!(
                        buf,
                        "{}\n\n",
                        "This is the help command for interacting with Garden programs. Welcome."
                    )
                    .unwrap();
                    print_available_commands(buf);
                }
            }
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
                write!(
                    buf,
                    ":replace requires a valid expression, e.g. `:replace 42`"
                )
                .unwrap();
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
                for (_, expr) in stack_frame.exprs_to_eval.iter().rev() {
                    writeln!(buf, "{:?}", expr.1).unwrap();
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
                match parse_def_or_expr_from_str(&PathBuf::from("__interactive__"), &src) {
                    Ok(ast) => write!(buf, "{:?}", ast).unwrap(),
                    Err(ParseError::Incomplete(e)) | Err(ParseError::OtherError(_, e)) => {
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

fn command_help(command: Command) -> &'static str {
    match command {
        Command::Abort => "The :abort command stops evaluation of the current expression, brining you back to the toplevel.\n\nExample usage:\n\n:abort",
        Command::Doc(_) => "The :doc command displays information about Garden values.\n\nExample:\n\n:doc print",
        Command::Help(_) => "The :help command displays information about interacting with Garden. It can also describe commands.\n\nExample:\n\n:help :doc",
        Command::Globals => "The :globals command displays information about toplevel definitions.\n\nExample:\n\n:globals",
        Command::Locals => "The :locals command displays information about local variables in the current stack frame.\n\nExample:\n\n:locals",
        Command::FrameValues => "The :fvalues command displays the intermediate value stack when evaluating the expressions in the current stack frame.\n\nExample:\n\n:fvalues",
        Command::FrameStatements => "The :fstmts command displays the statement stack in the current stack frame.\n\nExample:\n\n:fstmts",
        Command::Parse(_) => "The :parse command displays the parse tree generated for the expression given.\n\nExample:\n\n:parse 1 + 2",
        Command::Replace(_) => "The :replace command discards the top value in the value stack and replaces it with the expression provided.\n\nExample:\n\n:replace 123",
        Command::Resume => "The :resume command restarts evaluation if it's previously stopped.\n\nExample:\n\n:resume",
        Command::Skip => "The :skip command discards the current expression, and execution continues from the next expression.\n\nExample:\n\n:skip",
        Command::Source => "The :source command displays the history of all code evaluated in the current session.\n\nExample:\n\n:source",
        Command::Trace => "The :trace command toggles whether execution prints each expression before evaluation.\n\nExample:\n\n:trace",
        Command::Stack => "The :stack command prints the current call stack.\n\nExample:\n\n:stack",
        Command::Quit => "The :quit command terminates this Garden session and exits.\n\nExample:\n\n:quit",
    }
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
