//! Example-driven function discovery.
//!
//! Given some example inputs and a desired output, search the
//! available functions and methods for a call that produces that
//! output. This is inspired by suggest.el
//! (<https://github.com/Wilfred/suggest.el>) and the idea of
//! example-driven development: rather than searching documentation by
//! name, you describe the data you have and the data you want, and the
//! tool enumerates calls that bridge the two.
//!
//! The request and result are both JSON, so this can drive a web UI.
//! A request looks like:
//!
//! ```json
//! {"inputs": ["\"hello world\"", "\" \""], "output": "[\"hello\", \"world\"]"}
//! ```
//!
//! where each input and the output is a Garden expression.

use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use serde::{Deserialize, Serialize};

use crate::env::Env;
use crate::eval::{
    eval_toplevel_call, eval_toplevel_exprs, eval_toplevel_method_call, EvalError, Session,
    StdoutStderrMode,
};
use crate::parser::ast::{
    Expression, FunInfo, IdGenerator, MethodInfo, SymbolName, ToplevelItem, TypeName,
};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::{Vfs, VfsPathBuf};
use crate::values::{type_representation, Value};

/// A request to find functions and methods that turn `inputs` into
/// `output`. Each entry is a Garden expression, e.g. `"hello"` or
/// `[1, 2, 3]`.
#[derive(Deserialize)]
struct SuggestRequest {
    inputs: Vec<String>,
    output: String,
}

#[derive(Serialize)]
struct SuggestResponse {
    /// An error encountered while evaluating the example inputs or
    /// output. `null` on success.
    error: Option<String>,
    /// The example inputs, as the user wrote them.
    inputs: Vec<String>,
    /// The desired output, as the user wrote it. `null` if evaluating
    /// the inputs or output failed.
    output: Option<String>,
    /// Calls that produce the desired output from the inputs.
    suggestions: Vec<Suggestion>,
}

#[derive(Serialize)]
struct Suggestion {
    /// `"function"` or `"method"`.
    kind: String,
    /// The name of the function or method, e.g. `split`.
    name: String,
    /// A call that produces the desired output, e.g.
    /// `"a b".split(" ")`.
    call: String,
    /// The signature of the function or method, e.g.
    /// `(this: String, needle: String): List<String>`.
    signature: String,
    /// Constant arguments that were synthesized to make the call,
    /// rather than taken from the example inputs, e.g. `["0"]` for
    /// `[1, 2, 3].get(0)`. Empty when the call uses only the inputs.
    constants: Vec<String>,
}

/// Constant expressions tried as additional arguments when the
/// example inputs alone don't fill a function's parameters. Mirrors
/// the constant synthesis in suggest.el.
const SYNTHESIZED_CONSTANTS: &[&str] = &["0", "1", "2", "-1", "\"\"", "\" \"", "[]"];

/// Find functions and methods that turn the example inputs into the
/// desired output, and print the result as JSON.
pub(crate) fn run_suggest(src: &str, path: &Path, interrupted: Arc<AtomicBool>, trace_exprs: bool) {
    let (input_srcs, output_src) = match parse_request(src) {
        Ok(request) => (request.inputs, request.output),
        Err(message) => {
            print_error(&message);
            return;
        }
    };

    let (vfs, _vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let mut env = Env::new(IdGenerator::default(), vfs);

    // Evaluate example calls in the file's namespace, so the prelude
    // functions and methods are in scope.
    let ns = env.get_or_create_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    // Run candidate calls in a sandbox with resource limits, so that we
    // can safely try arbitrary functions.
    env.tick_limit = Some(100_000);
    env.stack_limit = Some(1_000);
    env.enforce_sandbox = true;

    let session = Session {
        interrupted,
        // Suppress any output produced by candidate calls, so it
        // doesn't pollute our JSON.
        stdout_stderr_mode: StdoutStderrMode::DoNotWrite,
        start_time: Instant::now(),
        trace_exprs,
        pretty_print_json: false,
    };

    // A synthetic path used to position the synthetic call expressions.
    let call_vfs_path = env
        .vfs
        .insert(Rc::new(PathBuf::from("__suggest.gdn")), String::new());

    // Evaluate the example inputs to values.
    let mut inputs: Vec<(String, Value)> = vec![];
    for input_src in &input_srcs {
        match eval_snippet(&mut env, &session, input_src) {
            Ok(value) => inputs.push((input_src.clone(), value)),
            Err(message) => {
                print_error(&format!(
                    "Could not evaluate input `{input_src}`: {message}"
                ));
                return;
            }
        }
    }

    // Evaluate the desired output to a value.
    let output_value = match eval_snippet(&mut env, &session, &output_src) {
        Ok(value) => value,
        Err(message) => {
            print_error(&format!(
                "Could not evaluate output `{output_src}`: {message}"
            ));
            return;
        }
    };

    // Evaluate the constants we may synthesize as extra arguments.
    let mut constants: Vec<(String, Value)> = vec![];
    for constant_src in SYNTHESIZED_CONSTANTS {
        if let Ok(value) = eval_snippet(&mut env, &session, constant_src) {
            constants.push(((*constant_src).to_owned(), value));
        }
    }

    let suggestions = find_suggestions(
        &mut env,
        &session,
        &call_vfs_path,
        &inputs,
        &constants,
        &output_value,
    );

    let response = SuggestResponse {
        error: None,
        inputs: input_srcs,
        output: Some(output_src),
        suggestions,
    };
    println!(
        "{}",
        serde_json::to_string_pretty(&response).expect("Failed to serialize response")
    );
}

/// Parse a JSON request describing the example inputs and output.
///
/// Lines starting with `//` are stripped before parsing, so the same
/// file can carry the `// args:` footer that reftests use.
fn parse_request(src: &str) -> Result<SuggestRequest, String> {
    let json = src
        .lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n");

    serde_json::from_str(&json).map_err(|err| format!("Invalid JSON request: {err}"))
}

/// Evaluate a single Garden expression to a value.
fn eval_snippet(env: &mut Env, session: &Session, src: &str) -> Result<Value, String> {
    let vfs_path = env.vfs.insert(
        Rc::new(PathBuf::from("__suggest_input.gdn")),
        src.to_owned(),
    );
    let (items, errors) = parse_toplevel_items(&vfs_path, src, &mut env.id_gen);

    if let Some(error) = errors.first() {
        return Err(error.message().as_string());
    }

    let exprs = exprs_in_items(&items);
    if exprs.is_empty() {
        return Err("Expected an expression.".to_owned());
    }

    reset_eval_state(env);
    match eval_toplevel_exprs(&exprs, env, session) {
        Ok(mut values) => Ok(values.pop().unwrap_or_else(Value::unit)),
        Err(err) => Err(describe_eval_error(&err)),
    }
}

/// What a function or method parameter will accept as an argument.
enum ParamType {
    /// Any value, e.g. an unannotated parameter or a generic type
    /// variable like `T`.
    Any,
    /// Only values whose runtime type has this name, e.g. `Int`.
    Concrete(String),
}

/// Classify each parameter of `fun_info` by the kind of argument it
/// accepts. A parameter whose type is one of the function's own
/// generic type variables accepts anything.
fn param_types(fun_info: &FunInfo) -> Vec<ParamType> {
    let generics: Vec<&str> = fun_info
        .type_params
        .iter()
        .map(|type_param| type_param.name.text.as_str())
        .collect();

    fun_info
        .params
        .params
        .iter()
        .map(|param| match &param.hint {
            None => ParamType::Any,
            Some(hint) => {
                let name = &hint.sym.name.text;
                if generics.contains(&name.as_str()) {
                    ParamType::Any
                } else {
                    ParamType::Concrete(name.clone())
                }
            }
        })
        .collect()
}

/// Whether `args` are acceptable arguments for parameters of these
/// types.
fn args_match_params(params: &[ParamType], args: &[Value]) -> bool {
    if params.len() != args.len() {
        return false;
    }

    params.iter().zip(args).all(|(param, arg)| match param {
        ParamType::Any => true,
        ParamType::Concrete(type_name) => type_representation(arg).text == *type_name,
    })
}

/// One way of arranging the example inputs (and possibly some
/// synthesized constants) into a list of arguments.
struct Arrangement {
    /// The argument values, in order.
    values: Vec<Value>,
    /// The source text of each argument, in order.
    srcs: Vec<String>,
    /// The source text of any synthesized constants used.
    constants: Vec<String>,
}

/// Try every function and method against the example inputs, and
/// collect the calls that produce the desired output.
fn find_suggestions(
    env: &mut Env,
    session: &Session,
    call_vfs_path: &VfsPathBuf,
    inputs: &[(String, Value)],
    constants: &[(String, Value)],
    output: &Value,
) -> Vec<Suggestion> {
    let mut suggestions = vec![];
    let n = inputs.len();

    // Functions in scope. A function is a candidate when its arity
    // matches the number of inputs, or is one greater (so we can
    // synthesize a single constant argument).
    let mut fn_candidates: Vec<(SymbolName, Vec<ParamType>, String)> = vec![];
    {
        let ns = env.current_namespace();
        let ns = ns.borrow();
        for (name, value) in ns.values.iter() {
            // We can only call functions whose arity we know, which
            // rules out built-in functions without a signature.
            let Some(fun_info) = value.fun_info() else {
                continue;
            };
            let arity = fun_info.params.params.len();
            if arity != n && arity != n + 1 {
                continue;
            }

            fn_candidates.push((name.clone(), param_types(fun_info), fun_signature(fun_info)));
        }
    }
    fn_candidates.sort_by(|(a, _, _), (b, _, _)| a.text.cmp(&b.text));

    for (name, params, signature) in fn_candidates {
        if let Some(suggestion) = try_function(
            env,
            session,
            call_vfs_path,
            &name,
            &params,
            &signature,
            inputs,
            constants,
            output,
        ) {
            suggestions.push(suggestion);
        }
    }

    // Methods in scope. As with functions, the method is a candidate
    // when the receiver plus its parameters match the number of
    // inputs, or is one greater.
    let mut method_candidates: Vec<(TypeName, SymbolName, Vec<ParamType>, String)> = vec![];
    if n >= 1 {
        for (type_name, type_and_methods) in env.types.iter() {
            for (method_name, method_info) in type_and_methods.methods.iter() {
                let Some(fun_info) = method_info.fun_info() else {
                    continue;
                };
                let arity = fun_info.params.params.len() + 1;
                if arity != n && arity != n + 1 {
                    continue;
                }

                method_candidates.push((
                    type_name.clone(),
                    method_name.clone(),
                    param_types(fun_info),
                    method_signature(method_info, fun_info),
                ));
            }
        }
    }
    method_candidates.sort_by(|(a_ty, a_name, _, _), (b_ty, b_name, _, _)| {
        a_ty.text
            .cmp(&b_ty.text)
            .then_with(|| a_name.text.cmp(&b_name.text))
    });

    for (type_name, method_name, params, signature) in method_candidates {
        if let Some(suggestion) = try_method(
            env,
            session,
            call_vfs_path,
            &type_name,
            &method_name,
            &params,
            &signature,
            inputs,
            constants,
            output,
        ) {
            suggestions.push(suggestion);
        }
    }

    suggestions
}

/// Try calling `name` with the inputs (and possibly a synthesized
/// constant) until we find an arrangement that produces `output`.
fn try_function(
    env: &mut Env,
    session: &Session,
    call_vfs_path: &VfsPathBuf,
    name: &SymbolName,
    params: &[ParamType],
    signature: &str,
    inputs: &[(String, Value)],
    constants: &[(String, Value)],
    output: &Value,
) -> Option<Suggestion> {
    let extra = params.len() - inputs.len();

    for arrangement in arrangements(inputs, constants, extra) {
        // Skip arrangements whose argument types don't fit the
        // parameters, both to avoid pointless calls and because some
        // built-in functions assume their arguments are well-typed.
        if !args_match_params(params, &arrangement.values) {
            continue;
        }

        reset_eval_state(env);
        let Ok(value) = eval_toplevel_call(name, &arrangement.values, env, session, call_vfs_path)
        else {
            continue;
        };
        if &value != output {
            continue;
        }

        return Some(Suggestion {
            kind: "function".to_owned(),
            name: name.text.clone(),
            call: format!("{}({})", name.text, arrangement.srcs.join(", ")),
            signature: signature.to_owned(),
            constants: arrangement.constants,
        });
    }

    None
}

/// Try calling the method `method_name` on each input of the right
/// type, with the remaining inputs (and possibly a synthesized
/// constant) as arguments, until we find a call that produces
/// `output`.
fn try_method(
    env: &mut Env,
    session: &Session,
    call_vfs_path: &VfsPathBuf,
    type_name: &TypeName,
    method_name: &SymbolName,
    params: &[ParamType],
    signature: &str,
    inputs: &[(String, Value)],
    constants: &[(String, Value)],
    output: &Value,
) -> Option<Suggestion> {
    // The receiver is always one of the inputs, never a synthesized
    // constant: the user has some data and wants to know what to call
    // on it.
    for (recv_idx, (recv_src, recv_value)) in inputs.iter().enumerate() {
        // Methods are dispatched on the runtime type of the receiver,
        // so only try receivers of the right type.
        if type_representation(recv_value) != *type_name {
            continue;
        }

        let other_inputs: Vec<(String, Value)> = inputs
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != recv_idx)
            .map(|(_, input)| input.clone())
            .collect();

        // The receiver fills one slot, so the remaining slots are the
        // method's parameters.
        let extra = params.len() - other_inputs.len();

        for arrangement in arrangements(&other_inputs, constants, extra) {
            // Skip arrangements whose argument types don't fit the
            // parameters (see `try_function`).
            if !args_match_params(params, &arrangement.values) {
                continue;
            }

            reset_eval_state(env);
            let Ok(value) = eval_toplevel_method_call(
                recv_value,
                method_name,
                &arrangement.values,
                env,
                session,
                call_vfs_path,
            ) else {
                continue;
            };
            if &value != output {
                continue;
            }

            return Some(Suggestion {
                kind: "method".to_owned(),
                name: method_name.text.clone(),
                call: format!(
                    "{}.{}({})",
                    recv_src,
                    method_name.text,
                    arrangement.srcs.join(", ")
                ),
                signature: signature.to_owned(),
                constants: arrangement.constants,
            });
        }
    }

    None
}

/// All the ways to arrange `items` into an argument list, optionally
/// inserting `extra` synthesized constants.
///
/// We only synthesize a single constant at a time (`extra` is 0 or
/// 1), mirroring suggest.el. With one constant, we try each constant
/// at each position around every permutation of the inputs.
fn arrangements(
    items: &[(String, Value)],
    constants: &[(String, Value)],
    extra: usize,
) -> Vec<Arrangement> {
    let mut result = vec![];

    for perm in permutations(items.len()) {
        let base_values: Vec<Value> = perm.iter().map(|i| items[*i].1.clone()).collect();
        let base_srcs: Vec<String> = perm.iter().map(|i| items[*i].0.clone()).collect();

        if extra == 0 {
            result.push(Arrangement {
                values: base_values,
                srcs: base_srcs,
                constants: vec![],
            });
            continue;
        }

        for (constant_src, constant_value) in constants {
            for pos in 0..=base_values.len() {
                let mut values = base_values.clone();
                let mut srcs = base_srcs.clone();
                values.insert(pos, constant_value.clone());
                srcs.insert(pos, constant_src.clone());

                result.push(Arrangement {
                    values,
                    srcs,
                    constants: vec![constant_src.clone()],
                });
            }
        }
    }

    result
}

/// Build the displayed signature of a function, e.g.
/// `(x: Int, y: Int): Int`.
fn fun_signature(fun_info: &FunInfo) -> String {
    let params = fun_info
        .params
        .params
        .iter()
        .map(|param| {
            let type_src = match &param.hint {
                Some(hint) => hint.as_src(),
                None => "_".to_owned(),
            };
            format!("{}: {}", param.symbol.name.text, type_src)
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!("({}){}", params, return_signature(fun_info))
}

/// Build the displayed signature of a method, including the receiver,
/// e.g. `(this: String, needle: String): List<String>`.
fn method_signature(method_info: &MethodInfo, fun_info: &FunInfo) -> String {
    let mut params = vec![format!(
        "{}: {}",
        method_info.receiver_sym.name.text,
        method_info.receiver_hint.as_src()
    )];
    for param in &fun_info.params.params {
        let type_src = match &param.hint {
            Some(hint) => hint.as_src(),
            None => "_".to_owned(),
        };
        params.push(format!("{}: {}", param.symbol.name.text, type_src));
    }

    format!("({}){}", params.join(", "), return_signature(fun_info))
}

fn return_signature(fun_info: &FunInfo) -> String {
    match &fun_info.return_hint {
        Some(hint) => format!(": {}", hint.as_src()),
        None => String::new(),
    }
}

/// All permutations of the indices `0..n`.
fn permutations(n: usize) -> Vec<Vec<usize>> {
    let mut result = vec![];
    let mut current = vec![];
    let mut used = vec![false; n];
    permute(n, &mut used, &mut current, &mut result);
    result
}

fn permute(n: usize, used: &mut [bool], current: &mut Vec<usize>, result: &mut Vec<Vec<usize>>) {
    if current.len() == n {
        result.push(current.clone());
        return;
    }

    for i in 0..n {
        if used[i] {
            continue;
        }
        used[i] = true;
        current.push(i);
        permute(n, used, current, result);
        current.pop();
        used[i] = false;
    }
}

/// Reset the evaluation stack so we can try another candidate call
/// from a clean state, even if the previous call raised an error.
fn reset_eval_state(env: &mut Env) {
    env.stack.pop_to_toplevel();
    env.ticks = 0;

    let frame = env.current_frame_mut();
    frame.exprs_to_eval.clear();
    frame.evalled_values.clear();
    frame.evalled_values.push(Value::unit());
    frame.bindings_next_block.clear();
}

fn exprs_in_items(items: &[ToplevelItem]) -> Vec<Expression> {
    let mut exprs = vec![];
    for item in items {
        match item {
            ToplevelItem::Expr(expr) => exprs.push(expr.0.clone()),
            ToplevelItem::Block(block) => {
                for expr in &block.exprs {
                    exprs.push(expr.as_ref().clone());
                }
            }
            _ => {}
        }
    }
    exprs
}

fn describe_eval_error(err: &EvalError) -> String {
    match err {
        EvalError::Exception(exception_info) => exception_info.message.as_string(),
        EvalError::AssertionFailed(_, message) => message.as_string(),
        EvalError::Interrupted => "Interrupted".to_owned(),
        EvalError::ReachedTickLimit(_) => "Reached the tick limit".to_owned(),
        EvalError::ReachedStackLimit(_) => "Reached the stack limit".to_owned(),
        EvalError::ForbiddenInSandbox(_) => {
            "Tried to execute unsafe code in sandboxed mode".to_owned()
        }
    }
}

fn print_error(message: &str) {
    let response = SuggestResponse {
        error: Some(message.to_owned()),
        inputs: vec![],
        output: None,
        suggestions: vec![],
    };
    println!(
        "{}",
        serde_json::to_string_pretty(&response).expect("Failed to serialize response")
    );
}
