use std::fmt::Display;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::Duration;

use humantime::format_duration;
use itertools::Itertools as _;
use owo_colors::OwoColorize;
use rustc_hash::FxHashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::colors::green;
use crate::env::Env;
use crate::eval::{eval_exprs, Session};
use crate::garden_type::Type;
use crate::parser::ast::{self, IdGenerator, MethodKind, SymbolName, TypeHint, TypeName};
use crate::parser::vfs::{to_project_relative, Vfs};
use crate::parser::{parse_inline_expr_from_str, parse_toplevel_items, ParseError};
use crate::types::{BuiltinType, TypeDef};
use crate::values::{Value, Value_};
use crate::version::VERSION;

#[derive(Debug, EnumIter)]
pub(crate) enum Command {
    Abort,
    Doc(Option<String>),
    Help(Option<String>),
    Functions,
    Locals,
    File(Option<String>),
    Forget(Option<String>),
    ForgetCalls,
    ForgetLocal(Option<String>),
    FrameValues,
    FrameStatements,
    Globals,
    Methods(Option<String>),
    Namespaces(Option<String>),
    Parse(Option<String>),
    Quit,
    Replace(Option<String>),
    Resume,
    Skip,
    Stack,
    Search(Option<String>),
    Source(Option<String>),
    Test(Option<String>),
    Trace,
    Type(Option<String>),
    Types,
    Uptime,
    Version,
}

/// Split out the command name and the arguments (if any).
fn parse_command(s: &str) -> (&str, Option<&str>) {
    let s = s.trim();
    if let Some((name, args)) = s.split_once(' ') {
        (name, Some(args))
    } else {
        (s, None)
    }
}

#[derive(Debug)]
pub(crate) enum CommandParseError {
    NoSuchCommand(String),
    NotCommandSyntax,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Command::Abort => ":abort",
            Command::Doc(_) => ":doc",
            Command::File(_) => ":file",
            Command::Forget(_) => ":forget",
            Command::ForgetCalls => ":forget_calls",
            Command::ForgetLocal(_) => ":forget_local",
            Command::FrameStatements => ":fstmts",
            Command::FrameValues => ":fvalues",
            Command::Functions => ":funs",
            Command::Globals => ":globals",
            Command::Help(_) => ":help",
            Command::Locals => ":locals",
            Command::Methods(_) => ":methods",
            Command::Namespaces(_) => ":namespaces",
            Command::Parse(_) => ":parse",
            Command::Quit => ":quit",
            Command::Replace(_) => ":replace",
            Command::Resume => ":resume",
            Command::Search(_) => ":search",
            Command::Skip => ":skip",
            Command::Source(_) => ":source",
            Command::Stack => ":stack",
            Command::Test(_) => ":test",
            Command::Trace => ":trace",
            Command::Type(_) => ":type",
            Command::Types => ":types",
            Command::Uptime => ":uptime",
            Command::Version => ":version",
        };
        write!(f, "{name}")
    }
}

impl Command {
    // TODO: from_string(":search") should produce an error.
    pub(crate) fn from_string(s: &str) -> Result<Self, CommandParseError> {
        let (command_name, args) = parse_command(s);
        let args = args.map(|s| s.to_owned());

        match command_name.to_lowercase().as_str() {
            ":abort" => Ok(Command::Abort),
            ":doc" => Ok(Command::Doc(args)),
            ":file" => Ok(Command::File(args)),
            ":forget" => Ok(Command::Forget(args)),
            ":forget_calls" => Ok(Command::ForgetCalls),
            ":forget_local" => Ok(Command::ForgetLocal(args)),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":funs" => Ok(Command::Functions),
            ":globals" => Ok(Command::Globals),
            ":help" => Ok(Command::Help(args)),
            ":locals" => Ok(Command::Locals),
            ":methods" => Ok(Command::Methods(args)),
            ":namespaces" => Ok(Command::Namespaces(args)),
            ":parse" => Ok(Command::Parse(args)),
            ":quit" => Ok(Command::Quit),
            ":replace" => Ok(Command::Replace(Some(args.unwrap_or_default()))),
            ":resume" => Ok(Command::Resume),
            ":search" => Ok(Command::Search(args)),
            ":skip" => Ok(Command::Skip),
            ":source" => Ok(Command::Source(args)),
            ":test" => Ok(Command::Test(args)),
            ":stack" => Ok(Command::Stack),
            ":trace" => Ok(Command::Trace),
            ":type" => Ok(Command::Type(Some(args.unwrap_or_default()))),
            ":types" => Ok(Command::Types),
            ":uptime" => Ok(Command::Uptime),
            ":version" => Ok(Command::Version),
            _ => {
                if s.starts_with(':') {
                    Err(CommandParseError::NoSuchCommand(s.to_owned()))
                } else {
                    Err(CommandParseError::NotCommandSyntax)
                }
            }
        }
    }
}

pub(crate) fn print_available_commands<T: Write>(
    attempted: &str,
    buf: &mut T,
) -> Result<(), std::io::Error> {
    if !attempted.is_empty() {
        writeln!(buf, "No such command `{attempted}`.")?;
    }
    write!(buf, "The available commands are")?;

    let mut command_names: Vec<String> = Command::iter().map(|c| c.to_string()).collect();
    command_names.sort();

    for (i, name) in command_names.iter().enumerate() {
        if i == command_names.len() - 1 {
            write!(buf, " and {}.", green(name))?;
        } else if i == command_names.len() - 2 {
            write!(buf, " {}", green(name))?;
        } else {
            write!(buf, " {},", green(name))?;
        }
    }

    Ok(())
}

#[derive(Debug)]
pub(crate) enum CommandError {
    Io(std::io::Error),
    Action(EvalAction),
}

impl From<std::io::Error> for CommandError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

/// Actions that require an evaluation loop, and can't be run during command handling.
#[derive(Debug)]
pub(crate) enum EvalAction {
    Replace(ast::Expression),
    RunTest(SymbolName),
    Resume,
    Abort,
    Skip,
}

fn describe_type(type_: &TypeDef) -> String {
    match type_ {
        TypeDef::Builtin(builtin_type, _) => {
            let name = match builtin_type {
                BuiltinType::Int => "Int",
                BuiltinType::String => "String",
                BuiltinType::Fun => "Fun",
                BuiltinType::List => "List",
                BuiltinType::Tuple => "Tuple",
                BuiltinType::Namespace => "Namespace",
            };
            // TODO: Offer more comprehensive docs on built-in types.
            format!("{name} is a built-in type.")
        }
        TypeDef::Enum(enum_info) => {
            let mut description = String::new();

            if let Some(doc_comment) = &enum_info.doc_comment {
                description.push_str(doc_comment);
                description.push_str("\n\n");
            }

            let enum_name = &enum_info.name_sym;
            description.push_str(&format!("enum {}", &enum_name));

            if !enum_info.type_params.is_empty() {
                description.push_str(&format!(
                    "<{}>",
                    enum_info.type_params.iter().map(|p| &p.name).join(", ")
                ));
            }

            description.push_str("{\n");
            for variant_info in &enum_info.variants {
                let variant_desc = match &variant_info.payload_hint {
                    Some(hint) => {
                        format!("   {}({}),\n", variant_info.name_sym.name, hint.as_src())
                    }
                    None => format!("   {},\n", variant_info.name_sym.name),
                };
                description.push_str(&variant_desc)
            }
            description.push('}');

            description
        }
        TypeDef::Struct(struct_info) => {
            let mut description = String::new();

            if let Some(doc_comment) = &struct_info.doc_comment {
                description.push_str(doc_comment);
                description.push_str("\n\n");
            }

            let struct_name = &struct_info.name_sym;
            description.push_str(&format!("struct {} {{\n", &struct_name));

            for field in &struct_info.fields {
                description.push_str(&format!(
                    "   {}: {},\n",
                    field.sym.name,
                    field.hint.as_src()
                ));
            }

            description.push('}');

            description
        }
    }
}

fn describe_fun(value: &Value, project_root: &Path) -> Option<String> {
    match value.as_ref() {
        Value_::Fun {
            name_sym, fun_info, ..
        } => Some(format_fun_info(fun_info, name_sym, None, project_root)),
        Value_::BuiltinFunction(_kind, fun_info, _) => {
            if let Some(fun_info) = fun_info {
                if let Some(fun_name) = &fun_info.name_sym {
                    return Some(format_fun_info(fun_info, fun_name, None, project_root));
                }
            }

            Some("Undocumented built-in function.".to_owned())
        }
        _ => None,
    }
}

/// Format `fun_info` as a signature with doc comment.
fn format_fun_info(
    fun_info: &ast::FunInfo,
    name_symbol: &ast::Symbol,
    recv_hint: Option<&TypeHint>,
    project_root: &Path,
) -> String {
    let mut res = String::new();
    if let Some(doc_comment) = &fun_info.doc_comment {
        res.push_str(doc_comment);
        res.push_str("\n\n");
    }

    if let Some(name_symbol) = &fun_info.name_sym {
        res.push_str(&format!(
            "// Defined in {}\n",
            name_symbol.position.as_ide_string(project_root)
        ));
    }
    res.push_str(&format_signature(fun_info, name_symbol, recv_hint));

    res
}

fn format_signature(
    fun_info: &ast::FunInfo,
    name_symbol: &ast::Symbol,
    recv_hint: Option<&TypeHint>,
) -> String {
    let mut res = String::new();
    res.push_str("fn");

    if !fun_info.type_params.is_empty() {
        res.push('<');
        for (i, param) in fun_info.type_params.iter().enumerate() {
            if i != 0 {
                res.push_str(", ");
            }
            res.push_str(&format!("{}", param.name));
        }

        res.push('>');
    }

    if let Some(recv_hint) = recv_hint {
        res.push_str(&format!(" (this: {})", recv_hint.as_src()));
    }

    res.push_str(&format!(" {}", name_symbol.name));
    res.push('(');
    for (i, param) in fun_info.params.params.iter().enumerate() {
        if i != 0 {
            res.push_str(", ");
        }

        res.push_str(&format!("{}", &param.symbol.name));

        if let Some(param_hint) = &param.hint {
            res.push_str(&format!(": {}", param_hint.as_src()));
        }
    }
    res.push(')');

    if let Some(return_hint) = &fun_info.return_hint {
        res.push_str(&format!(": {}", return_hint.as_src()));
    }

    res.push_str(" { ... }");
    res
}

pub(crate) fn run_command<T: Write>(
    buf: &mut T,
    cmd: Command,
    env: &mut Env,
    session: &mut Session,
) -> Result<(), CommandError> {
    match cmd {
        Command::Help(text) => {
            let text = text.clone().unwrap_or_default();

            match Command::from_string(&text) {
                Ok(command) => {
                    write!(buf, "{}", command_help(command))?;
                }
                Err(CommandParseError::NoSuchCommand(s)) => {
                    print_available_commands(&s, buf)?;
                }
                // TODO: suggest :doc if user writes `:help foo`
                Err(CommandParseError::NotCommandSyntax) => {
                    write!(
                        buf,
                        "The following commands are available. For general help on using Garden, see https://www.garden-lang.org/.\n\n"
                    )?;
                    print_available_commands("", buf)?;
                }
            }
        }
        Command::Methods(text) => {
            let text = text.clone().unwrap_or_default();

            let mut type_names: Vec<_> = env.types.keys().collect();
            type_names.sort_by_key(|typename| format!("{typename}"));

            let mut is_first = true;
            for type_name in type_names {
                let mut method_names: Vec<_> =
                    env.types.get(type_name).unwrap().methods.values().collect();
                method_names.sort_by_key(|meth| &meth.name_sym.name.text);

                for meth_info in method_names {
                    let name = format!("{}::{}", type_name, &meth_info.name_sym.name);

                    let signature = match meth_info.fun_info() {
                        Some(fun_info) => {
                            let params = fun_info
                                .params
                                .params
                                .iter()
                                .map(|p| match &p.hint {
                                    Some(hint) => format!("_: {}", hint.as_src()),
                                    None => "_".to_owned(),
                                })
                                .join(", ");

                            let ret_hint = match &fun_info.return_hint {
                                Some(hint) => format!(": {}", hint.as_src()),
                                None => "".to_owned(),
                            };
                            format!("({params}){ret_hint}")
                        }
                        None => "()".to_owned(),
                    };

                    if name.contains(&text) {
                        if !is_first {
                            writeln!(buf)?;
                        }
                        write!(buf, "{name}{signature}")?;
                        is_first = false;
                    }
                }
            }
        }
        Command::Namespaces(_name) => {
            writeln!(buf, "Namespaces by file:")?;

            for (i, (path, ns)) in env.namespaces.iter().enumerate() {
                if i != 0 {
                    writeln!(buf)?;
                }

                let ns = ns.borrow();
                write!(buf, "\n{}", path.display())?;

                let mut syms = ns.external_syms.iter().collect::<Vec<_>>();
                syms.sort_by_key(|s| s.text.to_ascii_lowercase());

                for sym in syms {
                    write!(buf, "\n  {}", sym.text)?;
                }
            }

            writeln!(buf, "\n")?;
            let ns = env.prelude_namespace.borrow();
            write!(buf, "{}", ns.abs_path.display())?;

            let mut syms = ns.external_syms.iter().collect::<Vec<_>>();
            syms.sort_by_key(|s| s.text.to_ascii_lowercase());

            for sym in syms {
                write!(buf, "\n  {}", sym.text)?;
            }
        }
        Command::Doc(name) => {
            if let Some(name) = name {
                document_item(&name, env, buf)?;
            } else {
                write!(buf, ":doc requires a name, e.g. `:doc print`")?;
            }
        }
        Command::Replace(src) => {
            if let Some(src) = src {
                let path = Rc::new(PathBuf::from("__interactive_inline__"));
                let vfs_path = env.vfs.insert(path.clone(), src.clone());

                let (expr, errors) = parse_inline_expr_from_str(&vfs_path, &src, &mut env.id_gen);

                if let Some(first_err) = errors.first() {
                    write!(
                        buf,
                        ":replace requires a valid expression, e.g. `:replace 42`.\n{}",
                        first_err.message().as_string()
                    )?;
                    return Ok(());
                }

                return Err(CommandError::Action(EvalAction::Replace(expr)));
            } else {
                write!(
                    buf,
                    ":replace requires a valid expression, e.g. `:replace 42`"
                )?;
                return Ok(());
            }
        }
        Command::Resume => {
            return Err(CommandError::Action(EvalAction::Resume));
        }
        Command::Skip => {
            return Err(CommandError::Action(EvalAction::Skip));
        }
        Command::Search(text) => {
            let text = text.clone().unwrap_or_default();

            let ns = env.current_namespace();
            let ns = ns.borrow();

            // TODO: search doc comments too.
            let mut matching_defs = vec![];
            for (global_def, _) in ns.values.iter() {
                if global_def.text.contains(&text) {
                    matching_defs.push(global_def);
                }
            }

            for name in &matching_defs {
                writeln!(buf, "function: {name}")?;
            }
            write!(buf, "{} definitions found.", matching_defs.len())?;

            return Ok(());
        }
        Command::Source(name) => {
            if let Some(name) = name {
                match find_item_source(&name, env) {
                    Ok(Some(src_string)) => write!(buf, "{src_string}"),
                    Ok(None) => {
                        write!(buf, "Source not available for {name}.")
                    }
                    Err(msg) => write!(buf, "{msg}"),
                }
            } else {
                write!(
                    buf,
                    ":source requires a name, e.g. `:source String::contains`"
                )
            }?
        }
        Command::Uptime => {
            // Round to the nearest second.
            let uptime = Duration::from_secs(session.start_time.elapsed().as_secs());
            write!(buf, "{}", format_duration(uptime))?;
        }
        Command::Functions => {
            let ns = env.current_namespace();
            let ns = ns.borrow();

            let mut names = vec![];
            for (name, value) in ns.values.iter() {
                let is_fun = match value.as_ref() {
                    Value_::Fun { .. }
                    | Value_::Closure(_, _, _)
                    | Value_::BuiltinFunction(_, _, _) => true,
                    Value_::Integer(_) => false,
                    Value_::String(_) => false,
                    Value_::List { .. } => false,
                    Value_::Tuple { .. } => false,
                    Value_::EnumVariant { .. } => false,
                    Value_::EnumConstructor { .. } => false,
                    Value_::Struct { .. } => false,
                    Value_::Namespace(_) => false,
                };

                if is_fun {
                    names.push(&name.text);
                }
            }

            names.sort();

            for (i, var_name) in names.iter().enumerate() {
                write!(buf, "{}{}", if i == 0 { "" } else { "\n" }, var_name)?;
            }
        }
        Command::File(name) => {
            let working_dir = std::env::current_dir().unwrap_or(PathBuf::from("/"));

            match name {
                Some(file_name) => {
                    let abs_path = working_dir.join(PathBuf::from(file_name));
                    let ns = env.get_or_create_namespace(&abs_path);

                    let stack_frame = env
                        .stack
                        .0
                        .first_mut()
                        .expect("Should always have at least one frame");

                    let old_abs_path = stack_frame.namespace.borrow().abs_path.clone();
                    stack_frame.namespace = ns;

                    write!(
                        buf,
                        "Switched from {} to {}.",
                        old_abs_path.display(),
                        abs_path.display(),
                    )?;
                }
                None => {
                    let stack_frame = env
                        .stack
                        .0
                        .last()
                        .expect("Should always have at least one frame");

                    let ns = stack_frame.namespace.borrow();
                    write!(
                        buf,
                        "The current namespace is {}, the working directory is {}.",
                        to_project_relative(&ns.abs_path, &env.project_root).display(),
                        working_dir.display(),
                    )?;
                }
            }
        }
        Command::ForgetLocal(name) => {
            if let Some(name) = name {
                let stack_frame = env
                    .stack
                    .0
                    .last_mut()
                    .expect("Should always have at least one frame");

                let sym_name = SymbolName { text: name.clone() };
                let sym_intern_id = env.id_gen.intern_symbol(&sym_name);

                if stack_frame.bindings.has(sym_intern_id) {
                    stack_frame.bindings.remove(sym_intern_id);
                } else {
                    write!(
                        buf,
                        "No local variable named `{name}` is defined in this stack frame."
                    )?;
                }
            } else {
                write!(
                    buf,
                    ":forget_local requires a name, e.g. `:forget_local variable_name_here`"
                )?;
            }
        }
        Command::Forget(name) => {
            if let Some(name) = name {
                let sym = SymbolName {
                    text: name.to_owned(),
                };

                let ns = env.current_namespace();
                let mut ns = ns.borrow_mut();

                if ns.values.remove(&sym).is_none() {
                    write!(buf, "No function or enum value named `{name}` is defined.")?;
                }
            } else {
                write!(buf, ":forget requires a name, e.g. `:forget function_name`")?;
            }
        }
        Command::FrameStatements => {
            if let Some(stack_frame) = env.stack.0.last() {
                for (_, expr) in stack_frame.exprs_to_eval.iter().rev() {
                    writeln!(buf, "{:#?}", expr.expr_)?;
                }
            }
        }
        Command::FrameValues => {
            if let Some(stack_frame) = env.stack.0.last() {
                for value in stack_frame.evalled_values.iter().rev() {
                    writeln!(buf, "{}", value.display(env))?;
                }
            }
        }
        Command::Locals => {
            if let Some(stack_frame) = env.stack.0.last() {
                let mut locals = vec![];
                for (var_id, value) in stack_frame.bindings.all().iter() {
                    let name = match env.id_gen.intern_id_to_name.get(var_id) {
                        Some(SymbolName { text: name }) => name.clone(),
                        None => "INTERNAL ERROR: Could not find var with this ID".to_owned(),
                    };

                    locals.push((name, value.clone()));
                }

                locals.sort_by_key(|(s, _)| s.to_lowercase());

                let mut max_length = 0;
                for (name, _) in &locals {
                    max_length = std::cmp::max(max_length, name.len());
                }

                for (i, (name, value)) in locals.iter().enumerate() {
                    write!(
                        buf,
                        "{}{:width$} {}",
                        if i == 0 { "" } else { "\n" },
                        name.bright_green(),
                        value.display(env),
                        width = max_length,
                    )?;
                }
            }
        }
        Command::Globals => {
            let namespace = env.current_namespace();
            let namespace = namespace.borrow();

            let mut values = namespace.values.iter().collect::<Vec<_>>();
            values.sort_by_key(|s| s.0.text.to_lowercase());

            let mut max_length = 0;
            for (name, _) in &values {
                max_length = std::cmp::max(max_length, name.text.len());
            }

            for (i, (sym_name, value)) in values.into_iter().enumerate() {
                // here
                write!(
                    buf,
                    "{}{:width$} {}",
                    if i == 0 { "" } else { "\n" },
                    sym_name.text.bright_green(),
                    value.display(env),
                    width = max_length,
                )?;
            }
        }
        Command::Stack => {
            print_stack(buf, env);
        }
        Command::Test(name) => match name {
            Some(name) => {
                return Err(CommandError::Action(EvalAction::RunTest(SymbolName {
                    text: name.clone(),
                })));
            }
            None => write!(buf, ":test requires a name, e.g. `:test name_of_test`.")?,
        },
        Command::Trace => {
            session.trace_exprs = !session.trace_exprs;
            write!(
                buf,
                "Expression tracing {}.",
                if session.trace_exprs {
                    "enabled"
                } else {
                    "disabled"
                }
            )?;
        }
        Command::Parse(src) => {
            if let Some(src) = src {
                let mut vfs = Vfs::default();
                let mut id_gen = IdGenerator::default();

                let path = Rc::new(PathBuf::from("__interactive__"));
                let vfs_path = vfs.insert(path.clone(), src.to_owned());

                let (items, errors) = parse_toplevel_items(&vfs_path, &src, &mut id_gen);
                for error in errors {
                    let msg = match error {
                        ParseError::Invalid { message, .. } => message,
                        ParseError::Incomplete { message, .. } => message,
                    };

                    writeln!(buf, "{}: {}", "Error".bright_red(), msg.as_string())?;
                }

                for (i, item) in items.iter().enumerate() {
                    write!(buf, "{}{:#?}", if i == 0 { "" } else { "\n" }, item)?
                }
            } else {
                write!(buf, ":parse requires a code snippet, e.g. `:parse 1 + 2`")?;
            }
        }
        Command::Abort => {
            env.stack.pop_to_toplevel();
            return Err(CommandError::Action(EvalAction::Abort));
        }
        Command::Quit => {
            std::process::exit(0);
        }
        Command::Type(src) => {
            if let Some(src) = src {
                let path = Rc::new(PathBuf::from("__interactive_inline__"));
                let vfs_path = env.vfs.insert(path.clone(), src.clone());

                let (expr, errors) = parse_inline_expr_from_str(&vfs_path, &src, &mut env.id_gen);

                if let Some(first_err) = errors.first() {
                    write!(
                        buf,
                        ":type requires a valid expression, e.g. `:type 42`.\n{}",
                        first_err.message().as_string()
                    )?;
                } else {
                    match eval_exprs(&[expr], env, session) {
                        Ok(value) => {
                            write!(buf, "{}", Type::from_value(&value))?;
                        }
                        Err(e) => {
                            // TODO: Print a proper stack trace.
                            write!(buf, "Evaluation failed: {e:?}")?;
                        }
                    }
                }
            } else {
                write!(buf, ":type requires a code snippet, e.g. `:type 1 + 2`")?;
            }
        }
        Command::Types => {
            let mut names: Vec<_> = env.all_types().into_iter().map(|s| s.text).collect();
            names.sort();

            for (i, var_name) in names.iter().enumerate() {
                write!(buf, "{}{}", if i == 0 { "" } else { "\n" }, var_name)?;
            }
        }
        Command::Version => {
            write!(buf, "Garden {}", VERSION.as_str())?;
        }
        Command::ForgetCalls => {
            env.prev_call_args = FxHashMap::default();
            env.prev_method_call_args = FxHashMap::default();

            write!(buf, "Discarded all saved values for method and function calls. The next call will be saved.")?;
        }
    }
    Ok(())
}

fn find_item_source(name: &str, env: &Env) -> Result<Option<String>, String> {
    let ns = env.current_namespace();

    if let Some((type_name, method_name)) = name.split_once("::") {
        if let Some(type_def_and_methods) = env.types.get(&TypeName {
            text: type_name.to_owned(),
        }) {
            if let Some(method_info) = type_def_and_methods.methods.get(&SymbolName {
                text: method_name.to_owned(),
            }) {
                Ok(method_info
                    .fun_info()
                    .and_then(|fun_info| env.vfs.pos_src(&fun_info.pos).map(|s| s.to_owned())))
            } else {
                Err(format!("No method named `{method_name}` on `{type_name}`."))
            }
        } else {
            // TODO: distinguish between no type with this name, and the type having no methods.
            Err(format!("No type named `{type_name}`."))
        }
    } else if let Some(type_) = env.get_type_def(&TypeName {
        text: name.to_owned(),
    }) {
        match type_ {
            TypeDef::Builtin(_, Some(struct_info)) => {
                Ok(env.vfs.pos_src(&struct_info.pos).map(|s| s.to_owned()))
            }
            TypeDef::Builtin(_, None) => Ok(None),
            TypeDef::Enum(enum_info) => Ok(env.vfs.pos_src(&enum_info.pos).map(|s| s.to_owned())),
            TypeDef::Struct(struct_info) => {
                Ok(env.vfs.pos_src(&struct_info.pos).map(|s| s.to_owned()))
            }
        }
    } else if let Some(value) = ns.borrow().values.get(&SymbolName {
        text: name.to_owned(),
    }) {
        match value.as_ref() {
            Value_::Fun { fun_info, .. } => {
                Ok(env.vfs.pos_src(&fun_info.pos).map(|s| s.to_owned()))
            }
            // TODO: Offer source of stub for built-in functions.
            _ => Ok(None),
        }
    } else {
        Err(format!("No function defined named `{name}`."))
    }
}

/// Get the name and doc comment of this item, if any is defined in
/// `Env`. This may be a function name, method name, or type name.
fn find_item(name: &str, env: &Env) -> Result<(String, Option<String>), String> {
    let ns = env.current_namespace();

    if let Some((before_colon, after_colon)) = name.split_once("::") {
        if let Some(type_def_and_methods) = env.types.get(&TypeName {
            text: before_colon.to_owned(),
        }) {
            if let Some(method_info) = type_def_and_methods.methods.get(&SymbolName {
                text: after_colon.to_owned(),
            }) {
                Ok((
                    format!("Method `{after_colon}`"),
                    format_method_info(method_info, &env.project_root),
                ))
            } else {
                Err(format!(
                    "No method named `{after_colon}` on `{before_colon}`."
                ))
            }
        } else if let Some(v) = ns.borrow().values.get(&SymbolName {
            text: before_colon.to_owned(),
        }) {
            let Value_::Namespace(named_ns) = v.as_ref() else {
                return Err(format!("`{before_colon}` is not a namespace."));
            };
            let named_ns = named_ns.borrow();

            let Some(value) = named_ns.values.get(&SymbolName {
                text: after_colon.to_owned(),
            }) else {
                return Err(format!(
                    "`{before_colon}` does not contain a value named `{after_colon}`."
                ));
            };

            match describe_fun(value, &env.project_root) {
                Some(description) => Ok((format!("Function `{name}`"), Some(description))),
                None => Err(format!("`{name}` is not a function.")),
            }
        } else {
            Err(format!("No type named `{before_colon}`."))
        }
    } else if let Some(type_) = env.get_type_def(&TypeName {
        text: name.to_owned(),
    }) {
        Ok((format!("Type `{name}`"), Some(describe_type(type_))))
    } else if let Some(value) = ns.borrow().values.get(&SymbolName {
        text: name.to_owned(),
    }) {
        // TODO: Ideally we'd print both values and type if both are defined.
        match describe_fun(value, &env.project_root) {
            Some(description) => Ok((format!("Function `{name}`"), Some(description))),
            None => Err(format!("`{name}` is not a function.")),
        }
    } else {
        Err(format!("No function defined named `{name}`."))
    }
}

fn format_method_info(method_info: &ast::MethodInfo, project_root: &Path) -> Option<String> {
    let fun_info = match &method_info.kind {
        MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
        MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
    };

    fun_info.map(|fi| {
        format_fun_info(
            fi,
            &method_info.name_sym,
            Some(&method_info.receiver_hint),
            project_root,
        )
    })
}

fn document_item<T: Write>(name: &str, env: &Env, buf: &mut T) -> std::io::Result<()> {
    match find_item(name, env) {
        Ok((_, Some(doc_comment))) => write!(buf, "{doc_comment}"),
        Ok((item_kind_desc, None)) => {
            write!(buf, "{item_kind_desc} does not have a doc comment.")
        }
        Err(msg) => write!(buf, "{msg}"),
    }
}

fn command_help(command: Command) -> &'static str {
    match command {
        Command::Abort => "The :abort command stops evaluation of the current expression, bringing you back to the toplevel.\n\nExample usage:\n\n:abort",
        Command::Doc(_) => "The :doc command displays information about Garden values.\n\nExamples:\n\n:doc print\n:doc String::starts_with\n:doc some_namespace::some_fun",
        Command::Help(_) => "The :help command displays information about interacting with Garden. It can also describe commands.\n\nExample:\n\n:help :doc",
        Command::File(_) => "The :file command shows the current file where evaluation is occurring. It can also change the file of the toplevel.\n\nExample:\n\n:file\n:file myproject.gdn",
        // TODO: add a more comprehensive example of :forget_local usage.
        Command::Forget(_) => "The :forget command undefines a function or enum value.\n\nExample:\n\n:forget function_name",
        Command::ForgetCalls => "The :forget_calls command discards previously saved values from function and method calls. This ensures that the next call is saved instead.\n\nExample:\n\n:forget_calls",
        Command::ForgetLocal(_) => "The :forget_local command undefines the local variable in the current stack frame.\n\nExample:\n\n:forget_local foo",
        Command::FrameValues => "The :fvalues command displays the intermediate value stack when evaluating the expressions in the current stack frame.\n\nExample:\n\n:fvalues",
        Command::FrameStatements => "The :fstmts command displays the statement stack in the current stack frame.\n\nExample:\n\n:fstmts",
        Command::Functions => "The :funs command displays information about toplevel functions.\n\nExample:\n\n:funs",
        Command::Globals => "The :globals command displays information about global values in the current file.\n\nExample:\n\n:globals",
        Command::Locals => "The :locals command displays information about local variables in the current stack frame.\n\nExample:\n\n:locals",
        Command::Methods(_) => "The :methods command displays all the methods currently defined. If given an argument, limits to names containing that substring.\n\nExample:\n\n:methods\n:method Str",
        Command::Namespaces(_) => "The :namespaces command displays all the namespaces of all the files loaded.\n\nExample:\n\n:namespaces",
        Command::Parse(_) => "The :parse command displays the parse tree generated for the expression given.\n\nExample:\n\n:parse 1 + 2",
        Command::Quit => "The :quit command terminates this Garden session and exits.\n\nExample:\n\n:quit",
        Command::Replace(_) => "The :replace command discards the top value in the value stack and replaces it with the expression provided.\n\nExample:\n\n:replace 123",
        Command::Resume => "The :resume command restarts evaluation if it's previously stopped.\n\nExample:\n\n:resume",
        Command::Search(_) => "The :search command shows all the definitions whose name contains the search term.\n\nExample:\n\n:search string",
        Command::Skip => "The :skip command discards the current expression, and execution continues from the next expression.\n\nExample:\n\n:skip",
        Command::Source(_) => "The :source command displays the source code of a definition.\n\nExample:\n\n:source String::contains",
        Command::Test(_) => "The :test command runs the test with the name specified.\n\nExample:\n\n:test some_test_name",
        Command::Trace => "The :trace command toggles whether execution prints each expression before evaluation.\n\nExample:\n\n:trace",
        Command::Type(_) => "The :type command shows the type of a given expression.\n\nExample:\n\n:type 1 + 2",
        Command::Types => "The :types command shows all the types currently defined.\n\nExample:\n\n:types",
        Command::Stack => "The :stack command prints the current call stack.\n\nExample:\n\n:stack",
        Command::Uptime => "The :uptime command displays how long this Garden session has been running.\n\nExample:\n\n:uptime",
        Command::Version => "The :version command shows the current version and commit of this Garden session.\n\nExample:\n\n:version",
    }
}

pub(crate) fn print_stack<T: Write>(buf: &mut T, env: &Env) {
    for (i, stack_frame) in env.stack.0.iter().rev().enumerate() {
        let name = &stack_frame.enclosing_name;
        let formatted_pos = match &stack_frame.caller_pos {
            Some(pos) => format!("{} ", pos.as_ide_string(&env.project_root)),
            None => "".to_owned(),
        };

        write!(
            buf,
            "{}{}{}",
            if i == 0 { "" } else { "\n" },
            formatted_pos,
            name
        )
        .unwrap();
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
        assert_eq!(parse_command(":foo bar baz"), (":foo", Some("bar baz")));
    }

    #[test]
    fn test_parse_command_trim() {
        assert_eq!(parse_command(" :foo "), (":foo", None));
    }
}
