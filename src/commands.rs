use std::{fmt::Display, io::Write, path::PathBuf};

use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::ast::{self, SymbolName, TypeName};
use crate::eval::eval_exprs;
use crate::parse::parse_toplevel_item;
use crate::values::{builtin_fun_doc, type_representation, Value};
use crate::version::VERSION;
use crate::{
    colors::green,
    eval::{Env, Session},
    parse::{parse_inline_expr_from_str, ParseError},
};

#[derive(Debug, EnumIter)]
pub enum Command {
    Abort,
    Doc(Option<String>),
    Help(Option<String>),
    Functions,
    Locals,
    ForgetLocal(Option<String>),
    FrameValues,
    FrameStatements,
    Methods,
    Parse(Option<String>),
    Replace(Option<ast::Expression>),
    Resume,
    Skip,
    Search(Option<String>),
    Source,
    Test(Option<String>),
    Trace,
    Type(Option<ast::Expression>),
    Stack,
    Quit,
    Version,
}

/// Split out the command name and the arguments (if any).
fn parse_command(s: &str) -> (&str, Option<String>) {
    let s = s.trim();
    if let Some((name, args)) = s.split_once(" ") {
        // Deliberately return an owned String because it simplifies
        // the callers.
        (name, Some(args.to_owned()))
    } else {
        (s, None)
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
            Command::ForgetLocal(_) => ":forget_local",
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
            Command::Test(_) => ":test",
            Command::Trace => ":trace",
            Command::Type(_) => ":type",
            Command::Quit => ":quit",
            Command::Version => ":version",
        };
        write!(f, "{}", name)
    }
}

impl Command {
    // TODO: from_string(":search") should produce an error.
    pub fn from_string(s: &str) -> Result<Self, CommandParseError> {
        let (command_name, args) = parse_command(s);

        match command_name.to_lowercase().as_str() {
            ":abort" => Ok(Command::Abort),
            ":doc" => Ok(Command::Doc(args)),
            ":forget_local" => Ok(Command::ForgetLocal(args)),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":funs" => Ok(Command::Functions),
            ":help" => Ok(Command::Help(args)),
            ":locals" => Ok(Command::Locals),
            ":methods" => Ok(Command::Methods),
            ":parse" => Ok(Command::Parse(args)),
            ":replace" => {
                // TODO: find a better name for this.
                match parse_inline_expr_from_str(
                    &PathBuf::from("__interactive_inline__"),
                    &args.unwrap_or_default(),
                ) {
                    Ok(expr) => Ok(Command::Replace(Some(expr))),
                    Err(_) => Ok(Command::Replace(None)),
                }
            }
            ":resume" => Ok(Command::Resume),
            ":search" => Ok(Command::Search(args)),
            ":skip" => Ok(Command::Skip),
            ":source" => Ok(Command::Source),
            ":test" => Ok(Command::Test(args)),
            ":stack" => Ok(Command::Stack),
            ":trace" => Ok(Command::Trace),
            ":type" => {
                match parse_inline_expr_from_str(
                    &PathBuf::from("__interactive_inline__"),
                    &args.unwrap_or_default(),
                ) {
                    Ok(expr) => Ok(Command::Type(Some(expr))),
                    Err(_) => Ok(Command::Type(None)),
                }
            }
            ":quit" => Ok(Command::Quit),
            ":version" => Ok(Command::Version),
            _ => {
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

/// Actions that require an evaluation loop, and can't be run during command handling.
#[derive(Debug)]
pub enum EvalAction {
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
                None => res.push_str(&format!("`{}` has no documentation comment.", name.name.0)),
            }
            res.push_str("\n\n");

            // show type hints
            res.push_str(&format!("fn {}", name.name.0));
            res.push('(');
            for (i, param) in params.iter().enumerate() {
                if i != 0 {
                    res.push_str(", ");
                }

                let name = &param.symbol;
                res.push_str(&name.name.0);

                if let Some(param_ty) = &param.type_ {
                    res.push_str(&format!(": {}", param_ty.0));
                }
            }
            res.push(')');

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
) -> Result<(), EvalAction> {
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
                method_names.sort_by_key(|meth| &meth.name.name.0);

                for method_name in method_names {
                    if !is_first {
                        writeln!(buf).unwrap();
                    }
                    write!(buf, "{}::{}", type_name.0, &method_name.name.name.0).unwrap();

                    is_first = false;
                }
            }
        }
        Command::Doc(name) => {
            if let Some(name) = name {
                if let Some((type_name, method_name)) = name.split_once("::") {
                    if let Some(type_methods) = env.methods.get(&TypeName(type_name.to_owned())) {
                        if let Some(method_info) =
                            type_methods.get(&SymbolName(method_name.to_owned()))
                        {
                            if let Some(doc_comment) = method_info.doc_comment() {
                                write!(buf, "{}", doc_comment).unwrap();
                            } else {
                                // TODO: show a signature too, similar to :doc on functions.
                                write!(
                                    buf,
                                    "Method `{}` does not have a doc comment.",
                                    method_name,
                                )
                                .unwrap();
                            }
                        } else {
                            write!(buf, "No method named `{}` on `{}`.", method_name, type_name)
                                .unwrap();
                        }
                    } else {
                        // TODO: distinguish between no type with this name, and the type having no methods.
                        write!(buf, "No type named `{}`.", type_name).unwrap();
                    }

                    //
                } else if let Some(value) = env.file_scope.get(&SymbolName(name.to_owned())) {
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
                return Err(EvalAction::Replace(stmt.clone()));
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
            return Err(EvalAction::Resume);
        }
        Command::Skip => {
            return Err(EvalAction::Skip);
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
        Command::ForgetLocal(name) => {
            if let Some(name) = name {
                let stack_frame = env
                    .stack
                    .last_mut()
                    .expect("Should always have at least one frame");
                if stack_frame.bindings.has(&SymbolName(name.clone())) {
                    stack_frame.bindings.remove(&SymbolName(name.clone()));
                } else {
                    write!(
                        buf,
                        "No local variable named `{}` is defined in this stack frame.",
                        name
                    )
                    .unwrap();
                }
            } else {
                write!(
                    buf,
                    ":forget_local requires a name, e.g. `:forget_local variable_name_here`"
                )
                .unwrap();
            }
        }
        Command::FrameStatements => {
            if let Some(stack_frame) = env.stack.last() {
                for (_, expr) in stack_frame.exprs_to_eval.iter().rev() {
                    writeln!(buf, "{:#?}", expr.1).unwrap();
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
        Command::Test(_) => {
            todo!();
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
                match parse_toplevel_item(&PathBuf::from("__interactive__"), src) {
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
            return Err(EvalAction::Abort);
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
        Command::Version => {
            write!(buf, "Garden {}", VERSION.as_str()).unwrap();
        }
    }
    Ok(())
}

fn command_help(command: Command) -> &'static str {
    match command {
        Command::Abort => "The :abort command stops evaluation of the current expression, brining you back to the toplevel.\n\nExample usage:\n\n:abort",
        Command::Doc(_) => "The :doc command displays information about Garden values.\n\nExample:\n\n:doc print",
        Command::Help(_) => "The :help command displays information about interacting with Garden. It can also describe commands.\n\nExample:\n\n:help :doc",
        // TODO: add a more comprehensive example of :forget_local usage.
        Command::ForgetLocal(_) => "The :forget_local command undefines the local variable in the current stack frame.\n\nExample:\n\n:forget_local foo",
        Command::FrameValues => "The :fvalues command displays the intermediate value stack when evaluating the expressions in the current stack frame.\n\nExample:\n\n:fvalues",
        Command::FrameStatements => "The :fstmts command displays the statement stack in the current stack frame.\n\nExample:\n\n:fstmts",
        Command::Functions => "The :funs command displays information about toplevel functions.\n\nExample:\n\n:funs",
        Command::Locals => "The :locals command displays information about local variables in the current stack frame.\n\nExample:\n\n:locals",
        Command::Methods => "The :methods command displays all the methods currently defined.\n\nExample:\n\n:methods",
        Command::Parse(_) => "The :parse command displays the parse tree generated for the expression given.\n\nExample:\n\n:parse 1 + 2",
        Command::Replace(_) => "The :replace command discards the top value in the value stack and replaces it with the expression provided.\n\nExample:\n\n:replace 123",
        Command::Resume => "The :resume command restarts evaluation if it's previously stopped.\n\nExample:\n\n:resume",
        Command::Search(_) => "The :search command shows all the definitions whose name contains the search term.\n\nExample:\n\n:search string",
        Command::Skip => "The :skip command discards the current expression, and execution continues from the next expression.\n\nExample:\n\n:skip",
        Command::Source => "The :source command displays the history of all code evaluated in the current session.\n\nExample:\n\n:source",
        Command::Test(_) => "The :test command runs the test with the name specified.\n\nExample:\n\n:test some_test_name",
        Command::Trace => "The :trace command toggles whether execution prints each expression before evaluation.\n\nExample:\n\n:trace",
        Command::Type(_) => "The :type command shows the type of a given expression.\n\nExample:\n\n:type 1 + 2",
        Command::Stack => "The :stack command prints the current call stack.\n\nExample:\n\n:stack",
        Command::Quit => "The :quit command terminates this Garden session and exits.\n\nExample:\n\n:quit",
        Command::Version => "The :version command shows the current version and commit of this Garden session.\n\nExample:\n\n:version",
    }
}

pub fn print_stack<T: Write>(buf: &mut T, env: &Env) {
    for (i, stack_frame) in env.stack.iter().rev().enumerate() {
        let name = &stack_frame.enclosing_name.0;
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

    #[test]
    fn test_parse_command() {
        assert_eq!(parse_command(":foo"), (":foo", None));
    }

    #[test]
    fn test_parse_command_with_args() {
        assert_eq!(
            parse_command(":foo bar baz"),
            (":foo", Some("bar baz".to_owned()))
        );
    }

    #[test]
    fn test_parse_command_trim() {
        assert_eq!(parse_command(" :foo "), (":foo", None));
    }
}
