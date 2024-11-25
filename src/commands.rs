use std::time::Duration;
use std::{fmt::Display, io::Write, path::PathBuf};

use humantime::format_duration;
use itertools::Itertools as _;
use owo_colors::OwoColorize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::env::Env;
use crate::eval::eval_exprs;
use crate::garden_type::Type;
use crate::types::{BuiltinType, TypeDef};
use crate::values::Value;
use crate::version::VERSION;
use crate::{colors::green, eval::Session};
use garden_lang_parser::ast::{
    self, MethodKind, SourceString, SymbolName, SyntaxIdGenerator, TypeHint, TypeName,
};
use garden_lang_parser::{parse_inline_expr_from_str, parse_toplevel_items, ParseError};

#[derive(Debug, EnumIter)]
pub(crate) enum Command {
    Abort,
    Doc(Option<String>),
    Help(Option<String>),
    Functions,
    Locals,
    ForgetLocal(Option<String>),
    Forget(Option<String>),
    FrameValues,
    FrameStatements,
    Methods(Option<String>),
    Parse(Option<String>),
    Quit,
    Replace(Option<ast::Expression>),
    Resume,
    Skip,
    Stack,
    Search(Option<String>),
    Source(Option<String>),
    Test(Option<String>),
    Trace,
    Type(Option<ast::Expression>),
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
    NoSuchCommand,
    NotCommandSyntax,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Command::Abort => ":abort",
            Command::Doc(_) => ":doc",
            Command::ForgetLocal(_) => ":forget_local",
            Command::Forget(_) => ":forget",
            Command::FrameStatements => ":fstmts",
            Command::FrameValues => ":fvalues",
            Command::Functions => ":funs",
            Command::Help(_) => ":help",
            Command::Locals => ":locals",
            Command::Methods(_) => ":methods",
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
        write!(f, "{}", name)
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
            ":forget" => Ok(Command::Forget(args)),
            ":forget_local" => Ok(Command::ForgetLocal(args)),
            ":fstmts" => Ok(Command::FrameStatements),
            ":fvalues" => Ok(Command::FrameValues),
            ":funs" => Ok(Command::Functions),
            ":help" => Ok(Command::Help(args)),
            ":locals" => Ok(Command::Locals),
            ":methods" => Ok(Command::Methods(args)),
            ":parse" => Ok(Command::Parse(args)),
            ":quit" => Ok(Command::Quit),
            ":replace" => {
                // TODO: this should continue from the last SyntaxId.
                let mut id_gen = SyntaxIdGenerator::default();

                // TODO: find a better name for this.
                let (expr, errors) = parse_inline_expr_from_str(
                    &PathBuf::from("__interactive_inline__"),
                    &args.unwrap_or_default(),
                    &mut id_gen,
                );
                if errors.is_empty() {
                    Ok(Command::Replace(Some(expr)))
                } else {
                    Ok(Command::Replace(None))
                }
            }
            ":resume" => Ok(Command::Resume),
            ":search" => Ok(Command::Search(args)),
            ":skip" => Ok(Command::Skip),
            ":source" => Ok(Command::Source(args)),
            ":test" => Ok(Command::Test(args)),
            ":stack" => Ok(Command::Stack),
            ":trace" => Ok(Command::Trace),
            ":type" => {
                let mut id_gen = SyntaxIdGenerator::default();
                let (expr, errors) = parse_inline_expr_from_str(
                    &PathBuf::from("__interactive_inline__"),
                    &args.unwrap_or_default(),
                    &mut id_gen,
                );
                if errors.is_empty() {
                    Ok(Command::Type(Some(expr)))
                } else {
                    Ok(Command::Type(None))
                }
            }
            ":types" => Ok(Command::Types),
            ":uptime" => Ok(Command::Uptime),
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

pub(crate) fn print_available_commands<T: Write>(buf: &mut T) {
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

fn describe_fun(value: &Value) -> Option<String> {
    match value {
        Value::Fun {
            name_sym, fun_info, ..
        } => Some(format_fun_info(fun_info, name_sym, None)),
        Value::BuiltinFunction(_kind, fun_info) => {
            if let Some(fun_info) = fun_info {
                if let Some(fun_name) = &fun_info.name_sym {
                    return Some(format_fun_info(fun_info, fun_name, None));
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
) -> String {
    let mut res = String::new();
    if let Some(doc_comment) = &fun_info.doc_comment {
        res.push_str(doc_comment);
        res.push_str("\n\n");
    }

    if let Some(name_symbol) = &fun_info.name_sym {
        res.push_str(&format!(
            "// Defined in {}\n",
            name_symbol.position.as_ide_string()
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
    for (i, param) in fun_info.params.iter().enumerate() {
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
        Command::Methods(text) => {
            let text = text.clone().unwrap_or_default();

            let mut type_names: Vec<_> = env.methods.keys().collect();
            type_names.sort_by_key(|typename| format!("{typename}"));

            let mut is_first = true;
            for type_name in type_names {
                let mut method_names: Vec<_> =
                    env.methods.get(type_name).unwrap().values().collect();
                method_names.sort_by_key(|meth| &meth.name_sym.name.0);

                for meth_info in method_names {
                    let name = format!("{}::{}", type_name, &meth_info.name_sym.name);

                    let signature = match meth_info.fun_info() {
                        Some(fun_info) => {
                            let params = fun_info
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
                            format!("({}){}", params, ret_hint)
                        }
                        None => "()".to_owned(),
                    };

                    if name.contains(&text) {
                        if !is_first {
                            writeln!(buf).unwrap();
                        }
                        write!(buf, "{}{}", name, signature).unwrap();
                        is_first = false;
                    }
                }
            }
        }
        Command::Doc(name) => {
            if let Some(name) = name {
                document_item(name, env, buf).unwrap();
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
                writeln!(buf, "function: {}", name).unwrap();
            }
            write!(buf, "{} definitions found.", matching_defs.len()).unwrap();

            return Ok(());
        }
        Command::Source(name) => if let Some(name) = name {
            match find_item_source(name, env) {
                Ok(Some(src_string)) => write!(buf, "{}", src_string.src),
                Ok(None) => {
                    write!(buf, "Source not available for {name}.")
                }
                Err(msg) => write!(buf, "{}", msg),
            }
        } else {
            write!(
                buf,
                ":source requires a name, e.g. `:source String::contains`"
            )
        }
        .unwrap(),
        Command::Uptime => {
            // Round to the nearest second.
            let uptime = Duration::from_secs(session.start_time.elapsed().as_secs());
            write!(buf, "{}", format_duration(uptime)).unwrap();
        }
        Command::Functions => {
            let mut names = vec![];
            for (name, value) in env.file_scope.iter() {
                let is_fun = match value {
                    Value::Fun { .. } | Value::Closure(_, _) | Value::BuiltinFunction(_, _) => true,
                    Value::Integer(_) => false,
                    Value::String(_) => false,
                    Value::List { .. } => false,
                    Value::Tuple { .. } => false,
                    Value::EnumVariant { .. } => false,
                    Value::EnumConstructor { .. } => false,
                    Value::Struct { .. } => false,
                };

                if is_fun {
                    names.push(&name.0);
                }
            }

            names.sort();

            for (i, var_name) in names.iter().enumerate() {
                write!(buf, "{}{}", if i == 0 { "" } else { "\n" }, var_name).unwrap();
            }
        }
        Command::ForgetLocal(name) => {
            if let Some(name) = name {
                let stack_frame = env
                    .stack
                    .0
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
        Command::Forget(name) => {
            if let Some(name) = name {
                let sym = SymbolName(name.to_owned());

                match env.file_scope.get(&sym) {
                    Some(_) => {
                        env.file_scope.remove(&sym);
                    }
                    None => {
                        write!(
                            buf,
                            "No function or enum value named `{}` is defined.",
                            name
                        )
                        .unwrap();
                    }
                }
            } else {
                write!(buf, ":forget requires a name, e.g. `:forget function_name`").unwrap();
            }
        }
        Command::FrameStatements => {
            if let Some(stack_frame) = env.stack.0.last() {
                for (_, expr) in stack_frame.exprs_to_eval.iter().rev() {
                    writeln!(buf, "{:#?}", expr.expr_).unwrap();
                }
            }
        }
        Command::FrameValues => {
            if let Some(stack_frame) = env.stack.0.last() {
                for value in stack_frame.evalled_values.iter().rev() {
                    writeln!(buf, "{}", value.display(env)).unwrap();
                }
            }
        }
        Command::Locals => {
            if let Some(stack_frame) = env.stack.0.last() {
                for (i, (var_name, value)) in stack_frame.bindings.all().iter().enumerate() {
                    write!(
                        buf,
                        "{}{}\t{}",
                        if i == 0 { "" } else { "\n" },
                        var_name.0.bright_green(),
                        value.display(env)
                    )
                    .unwrap();
                }
            }
        }
        Command::Stack => {
            print_stack(buf, env);
        }
        Command::Test(name) => match name {
            Some(name) => {
                return Err(EvalAction::RunTest(SymbolName(name.clone())));
            }
            None => write!(buf, ":test requires a name, e.g. `:test name_of_test`.").unwrap(),
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
            )
            .unwrap();
        }
        Command::Parse(src) => {
            if let Some(src) = src {
                let mut id_gen = SyntaxIdGenerator::default();

                let (items, errors) =
                    parse_toplevel_items(&PathBuf::from("__interactive__"), src, &mut id_gen);
                for error in errors {
                    let msg = match error {
                        ParseError::Invalid { message, .. } => message,
                        ParseError::Incomplete { message, .. } => message,
                    };
                    writeln!(buf, "{}: {}", "Error".bright_red(), msg.0).unwrap();
                }

                for (i, item) in items.iter().enumerate() {
                    write!(buf, "{}{:#?}", if i == 0 { "" } else { "\n" }, item).unwrap()
                }
            } else {
                write!(buf, ":parse requires a code snippet, e.g. `:parse 1 + 2`").unwrap();
            }
        }
        Command::Abort => {
            env.stack.pop_to_toplevel();
            return Err(EvalAction::Abort);
        }
        Command::Quit => {
            std::process::exit(0);
        }
        Command::Type(expr) => {
            if let Some(expr) = expr {
                match eval_exprs(&[expr.clone()], env, session) {
                    Ok(value) => {
                        write!(
                            buf,
                            "{}",
                            Type::from_value(&value, env, &env.stack.type_bindings())
                        )
                        .unwrap();
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
        Command::Types => {
            let mut names: Vec<_> = env.all_types().into_iter().map(|s| s.name).collect();
            names.sort();

            for (i, var_name) in names.iter().enumerate() {
                write!(buf, "{}{}", if i == 0 { "" } else { "\n" }, var_name).unwrap();
            }
        }
        Command::Version => {
            write!(buf, "Garden {}", VERSION.as_str()).unwrap();
        }
    }
    Ok(())
}

fn find_item_source(name: &str, env: &Env) -> Result<Option<SourceString>, String> {
    if let Some((type_name, method_name)) = name.split_once("::") {
        if let Some(type_methods) = env.methods.get(&TypeName {
            name: type_name.to_owned(),
        }) {
            if let Some(method_info) = type_methods.get(&SymbolName(method_name.to_owned())) {
                Ok(method_info
                    .fun_info()
                    .map(|fun_info| fun_info.src_string.clone()))
            } else {
                Err(format!("No method named `{method_name}` on `{type_name}`."))
            }
        } else {
            // TODO: distinguish between no type with this name, and the type having no methods.
            Err(format!("No type named `{type_name}`."))
        }
    } else if let Some(type_) = env.get_type_def(&TypeName {
        name: name.to_owned(),
    }) {
        match type_ {
            TypeDef::Builtin(_, Some(struct_info)) => Ok(Some(struct_info.src_string.clone())),
            TypeDef::Builtin(_, None) => Ok(None),
            TypeDef::Enum(enum_info) => Ok(Some(enum_info.src_string.clone())),
            TypeDef::Struct(struct_info) => Ok(Some(struct_info.src_string.clone())),
        }
    } else if let Some(value) = env.file_scope.get(&SymbolName(name.to_owned())) {
        match value {
            Value::Fun { fun_info, .. } => Ok(Some(fun_info.src_string.clone())),
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
    if let Some((type_name, method_name)) = name.split_once("::") {
        if let Some(type_methods) = env.methods.get(&TypeName {
            name: type_name.to_owned(),
        }) {
            if let Some(method_info) = type_methods.get(&SymbolName(method_name.to_owned())) {
                Ok((
                    format!("Method `{method_name}`"),
                    format_method_info(method_info),
                ))
            } else {
                Err(format!("No method named `{method_name}` on `{type_name}`."))
            }
        } else {
            // TODO: distinguish between no type with this name, and the type having no methods.
            Err(format!("No type named `{type_name}`."))
        }
    } else if let Some(type_) = env.get_type_def(&TypeName {
        name: name.to_owned(),
    }) {
        Ok((format!("Type `{name}`"), Some(describe_type(type_))))
    } else if let Some(value) = env.file_scope.get(&SymbolName(name.to_owned())) {
        // TODO: Ideally we'd print both values and type if both are defined.
        match describe_fun(value) {
            Some(description) => Ok((format!("Function `{name}`"), Some(description))),
            None => Err(format!("`{name}` is not a function.")),
        }
    } else {
        Err(format!("No function defined named `{name}`."))
    }
}

fn format_method_info(method_info: &ast::MethodInfo) -> Option<String> {
    let fun_info = match &method_info.kind {
        MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
        MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
    };

    fun_info.map(|fi| format_fun_info(fi, &method_info.name_sym, Some(&method_info.receiver_hint)))
}

fn document_item<T: Write>(name: &str, env: &Env, buf: &mut T) -> std::io::Result<()> {
    match find_item(name, env) {
        Ok((_, Some(doc_comment))) => write!(buf, "{}", doc_comment),
        Ok((item_kind_desc, None)) => {
            write!(buf, "{item_kind_desc} does not have a doc comment.")
        }
        Err(msg) => write!(buf, "{}", msg),
    }
}

fn command_help(command: Command) -> &'static str {
    match command {
        Command::Abort => "The :abort command stops evaluation of the current expression, brining you back to the toplevel.\n\nExample usage:\n\n:abort",
        Command::Doc(_) => "The :doc command displays information about Garden values.\n\nExample:\n\n:doc print",
        Command::Help(_) => "The :help command displays information about interacting with Garden. It can also describe commands.\n\nExample:\n\n:help :doc",
        // TODO: add a more comprehensive example of :forget_local usage.
        Command::ForgetLocal(_) => "The :forget_local command undefines the local variable in the current stack frame.\n\nExample:\n\n:forget_local foo",
        Command::Forget(_) => "The :forget command undefines a function or enum value.\n\nExample:\n\n:forget function_name",
        Command::FrameValues => "The :fvalues command displays the intermediate value stack when evaluating the expressions in the current stack frame.\n\nExample:\n\n:fvalues",
        Command::FrameStatements => "The :fstmts command displays the statement stack in the current stack frame.\n\nExample:\n\n:fstmts",
        Command::Functions => "The :funs command displays information about toplevel functions.\n\nExample:\n\n:funs",
        Command::Locals => "The :locals command displays information about local variables in the current stack frame.\n\nExample:\n\n:locals",
        Command::Methods(_) => "The :methods command displays all the methods currently defined. If given an argument, limits to names containing that substring.\n\nExample:\n\n:methods\n:method Str",
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
            Some(pos) => format!("{} ", pos.as_ide_string()),
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
