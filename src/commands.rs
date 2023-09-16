use std::{fmt::Display, io::Write, path::PathBuf};

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::ast;
use crate::eval::{eval_exprs, type_representation};
use crate::{
    colors::green,
    eval::{builtin_fun_doc, Env, Session, Value},
    parse::{parse_def_or_expr_from_str, parse_inline_expr_from_str, ParseError},
};

#[derive(Debug, EnumIter)]
pub enum Command {
    Abort,
    Doc(Option<String>),
    Help(Option<String>),
    Functions,
    Locals,
    FrameValues,
    FrameStatements,
    Methods,
    Parse(Option<String>),
    Replace(Option<ast::Expression>),
    Resume,
    Skip,
    Search(Option<String>),
    Source,
    Trace,
    Type(Option<ast::Expression>),
    Stack,
    Quit,
    // TODO: Version,
}

/// Returns Some if `s` starts with `word` and is followed by a word
/// boundary.
fn split_first_word<'a>(s: &'a str, word: &str) -> Option<&'a str> {
    if let Some(suffix) = s.strip_prefix(word) {
        if suffix.is_empty() {
            return Some("");
        }

        if let Some(rest) = suffix.strip_prefix(' ') {
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

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Command::Abort => ":abort",
            Command::Doc(_) => ":doc",
            Command::FrameStatements => ":fstmts",
            Command::FrameValues => ":fvalues",
            Command::Functions => ":funs",
            Command::Help(_) => ":help",
            Command::Locals => ":locals",
            Command::Methods => ":methods",
            Command::Parse(_) => ":parse",
            Command::Replace(_) => ":replace",
            Command::Resume => ":resume",
            Command::Search(_) => ":search",
            Command::Skip => ":skip",
            Command::Source => ":source",
            Command::Stack => ":stack",
            Command::Trace => ":trace",
            Command::Type(_) => ":type",
            Command::Quit => ":quit",
        };
        write!(f, "{}", name)
    }
}

impl Command {
    pub fn from_string(s: &str) -> Result<Self, CommandParseError> {
        match s.to_lowercase().trim() {
            ":abort" => Ok(Command::Abort),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":funs" => Ok(Command::Functions),
            ":locals" => Ok(Command::Locals),
            ":methods" => Ok(Command::Methods),
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
                if let Some(src) = split_first_word(s, ":search") {
                    return Ok(Command::Search(Some(src.to_owned())));
                }
                if let Some(src) = split_first_word(s, ":replace") {
                    // TODO: find a better name for this.
                    return match parse_inline_expr_from_str(
                        &PathBuf::from("__interactive_inline__"),
                        src,
                    ) {
                        Ok(expr) => Ok(Command::Replace(Some(expr))),
                        Err(_) => Ok(Command::Replace(None)),
                    };
                }

                if let Some(src) = split_first_word(s, ":type") {
                    return match parse_inline_expr_from_str(
                        &PathBuf::from("__interactive_inline__"),
                        src,
                    ) {
                        Ok(expr) => Ok(Command::Type(Some(expr))),
                        Err(_) => Ok(Command::Type(None)),
                    };
                }

                if s.starts_with(':') {
                    Err(CommandParseError::NoSuchCommand)
                } else {
                    Err(CommandParseError::NotCommandSyntax)
                }
            }
        }
    }
}

pub fn print_available_commands<T: Write>(buf: &mut T) {
    write!(buf, "The available commands are").unwrap();

    let mut command_names: Vec<String> = Command::iter().map(|c| c.to_string()).collect();
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
    Replace(ast::Expression),
    Resume,
    Abort,
    Skip,
}

fn describe_fun(value: &Value) -> Option<String> {
    match value {
        Value::Fun(
            name,
            ast::FunInfo {
                doc_comment,
                params,
                return_type,
                ..
            },
        ) => {
            let mut res = String::new();
            match doc_comment {
                Some(doc_comment) => {
                    res.push_str(doc_comment);
                }
                None => res.push_str(&format!("`{}` has no documentation comment.", name.1 .0)),
            }
            res.push_str("\n\n");

            // show type hints
            res.push_str(&format!("fn {}", name.1 .0));
            res.push_str("(");
            for (i, param) in params.iter().enumerate() {
                if i != 0 {
                    res.push_str(", ");
                }

                let name = &param.0;
                res.push_str(&name.1 .0);

                if let Some(param_ty) = &param.1 {
                    res.push_str(&format!(": {}", param_ty.0));
                }
            }
            res.push_str(")");

            if let Some(return_type) = return_type {
                res.push_str(&format!(": {}", return_type.0));
            }

            res.push_str(" { ... }");

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
    session: &mut Session,
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
                // TODO: suggest :doc if user writes `:help foo`
                Err(CommandParseError::NotCommandSyntax) => {
                    write!(
                        buf,
                        "This is the help command for interacting with Garden programs. Welcome.\n\n"
                    )
                    .unwrap();
                    print_available_commands(buf);
                }
            }
        }
        Command::Methods => {
            let mut type_names: Vec<_> = env.methods.keys().collect();
            type_names.sort_by_key(|typename| &typename.0);

            let mut is_first = true;
            for type_name in type_names {
                let mut method_names: Vec<_> =
                    env.methods.get(type_name).unwrap().values().collect();
                method_names.sort_by_key(|meth| &meth.name.1 .0);

                for method_name in method_names {
                    if !is_first {
                        write!(buf, "\n").unwrap();
                    }
                    write!(buf, "{}::{}", type_name.0, &method_name.name.1 .0).unwrap();

                    is_first = false;
                }
            }
        }
        Command::Doc(name) => {
            if let Some(name) = name {
                if let Some(value) = env.file_scope.get(&ast::SymbolName(name.to_string())) {
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
        Command::Search(text) => {
            let text = text.clone().unwrap_or_default();

            // TODO: search doc comments too.
            let mut matching_defs = vec![];
            for (global_def, _) in env.file_scope.iter() {
                if global_def.0.contains(&text) {
                    matching_defs.push(global_def);
                }
            }

            for name in &matching_defs {
                writeln!(buf, "function: {}", name.0).unwrap();
            }
            write!(buf, "{} definitions found.", matching_defs.len()).unwrap();

            return Ok(());
        }
        Command::Source => {
            write!(
                buf,
                "{}",
                if session.history.is_empty() {
                    "(empty)"
                } else {
                    &session.history
                }
            )
            .unwrap();
        }
        Command::Functions => {
            let mut names_and_vals: Vec<(ast::SymbolName, Value)> = env
                .file_scope
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            names_and_vals.sort_by(|(n1, _), (n2, _)| n1.0.cmp(&n2.0));

            for (i, (var_name, value)) in names_and_vals.iter().enumerate() {
                write!(
                    buf,
                    "{}{:<20} {}",
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
                    match &value.1 {
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
                for (i, (var_name, value)) in stack_frame.bindings.all().iter().enumerate() {
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
                match parse_def_or_expr_from_str(&PathBuf::from("__interactive__"), src) {
                    Ok(ast) => write!(buf, "{:#?}", ast).unwrap(),
                    Err(ParseError::Incomplete(e))
                    | Err(ParseError::Invalid { message: e, .. }) => {
                        write!(buf, "{}: {}", "Error".bright_red(), e.0).unwrap();
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
        Command::Type(expr) => {
            if let Some(expr) = expr {
                match eval_exprs(&[expr.clone()], env, session) {
                    Ok(value) => {
                        write!(buf, "{}", type_representation(&value).0).unwrap();
                    }
                    Err(e) => {
                        // TODO: Print a proper stack trace.
                        write!(buf, "Evaluation failed: {:?}", e).unwrap();
                    }
                }
            } else {
                write!(buf, ":type requires a code snippet, e.g. `:type 1 + 2`").unwrap();
            }
        }
    }
    Ok(())
}

fn command_help(command: Command) -> &'static str {
    match command {
        Command::Abort => "The :abort command stops evaluation of the current expression, brining you back to the toplevel.\n\nExample usage:\n\n:abort",
        Command::Doc(_) => "The :doc command displays information about Garden values.\n\nExample:\n\n:doc print",
        Command::Help(_) => "The :help command displays information about interacting with Garden. It can also describe commands.\n\nExample:\n\n:help :doc",
        Command::Functions => "The :funs command displays information about toplevel functions.\n\nExample:\n\n:funs",
        Command::FrameValues => "The :fvalues command displays the intermediate value stack when evaluating the expressions in the current stack frame.\n\nExample:\n\n:fvalues",
        Command::FrameStatements => "The :fstmts command displays the statement stack in the current stack frame.\n\nExample:\n\n:fstmts",
        Command::Locals => "The :locals command displays information about local variables in the current stack frame.\n\nExample:\n\n:locals",
        Command::Methods => "The :methods command displays all the methods currently defined.\n\nExample:\n\n:methods",
        Command::Parse(_) => "The :parse command displays the parse tree generated for the expression given.\n\nExample:\n\n:parse 1 + 2",
        Command::Replace(_) => "The :replace command discards the top value in the value stack and replaces it with the expression provided.\n\nExample:\n\n:replace 123",
        Command::Resume => "The :resume command restarts evaluation if it's previously stopped.\n\nExample:\n\n:resume",
        Command::Search(_) => "The :search command shows all the definitions whose name contains the search term.\n\nExample:\n\n:search string",
        Command::Skip => "The :skip command discards the current expression, and execution continues from the next expression.\n\nExample:\n\n:skip",
        Command::Source => "The :source command displays the history of all code evaluated in the current session.\n\nExample:\n\n:source",
        Command::Trace => "The :trace command toggles whether execution prints each expression before evaluation.\n\nExample:\n\n:trace",
        Command::Type(_) => "The :type command shows the type of a given expression.\n\nExample:\n\n:type 1 + 2",
        Command::Stack => "The :stack command prints the current call stack.\n\nExample:\n\n:stack",
        Command::Quit => "The :quit command terminates this Garden session and exits.\n\nExample:\n\n:quit",
    }
}

pub fn print_stack<T: Write>(buf: &mut T, env: &Env) {
    for (i, stack_frame) in env.stack.iter().rev().enumerate() {
        let name = match &stack_frame.call_site {
            Some((v, _)) => v.1 .0.clone(),
            None => "toplevel".to_owned(),
        };

        write!(buf, "{}In {}", if i == 0 { "" } else { "\n" }, name).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_help_takes_argument() {
        let cmd = Command::from_string(":help print").unwrap();
        assert!(matches!(cmd, Command::Help(_)));
    }
}
