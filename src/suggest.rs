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
//! The result is emitted as JSON so it can drive a web UI.

use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use serde::Serialize;

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
}

/// Find functions and methods that turn the example inputs into the
/// desired output, and print the result as JSON.
pub(crate) fn run_suggest(src: &str, path: &Path, interrupted: Arc<AtomicBool>, trace_exprs: bool) {
    let (input_srcs, output_src) = match parse_examples(src) {
        Ok((inputs, Some(output))) => (inputs, output),
        Ok((_, None)) => {
            print_error("No `output:` line found in the examples.");
            return;
        }
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

    let suggestions = find_suggestions(&mut env, &session, &call_vfs_path, &inputs, &output_value);

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

/// Parse the example file into input expressions and an output
/// expression.
///
/// The format is line-based. Blank lines and lines starting with `//`
/// are ignored. Every other line must be either `input: <expr>` or
/// `output: <expr>`.
fn parse_examples(src: &str) -> Result<(Vec<String>, Option<String>), String> {
    let mut inputs = vec![];
    let mut output = None;

    for line in src.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with("//") {
            continue;
        }

        if let Some(rest) = line.strip_prefix("input:") {
            inputs.push(rest.trim().to_owned());
        } else if let Some(rest) = line.strip_prefix("output:") {
            if output.is_some() {
                return Err("Found more than one `output:` line.".to_owned());
            }
            output = Some(rest.trim().to_owned());
        } else {
            return Err(format!(
                "Expected a line starting with `input:` or `output:`, but got: {line}"
            ));
        }
    }

    Ok((inputs, output))
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

/// Try every function and method against the example inputs, and
/// collect the calls that produce the desired output.
fn find_suggestions(
    env: &mut Env,
    session: &Session,
    call_vfs_path: &VfsPathBuf,
    inputs: &[(String, Value)],
    output: &Value,
) -> Vec<Suggestion> {
    let mut suggestions = vec![];
    let n = inputs.len();
    let perms = permutations(n);

    // Functions in scope. A function with `n` parameters is a
    // candidate when we have `n` inputs.
    let mut fn_candidates: Vec<(SymbolName, String)> = vec![];
    {
        let ns = env.current_namespace();
        let ns = ns.borrow();
        for (name, value) in ns.values.iter() {
            // We can only call functions whose arity we know, which
            // rules out built-in functions without a signature.
            let Some(fun_info) = value.fun_info() else {
                continue;
            };
            if fun_info.params.params.len() != n {
                continue;
            }

            fn_candidates.push((name.clone(), fun_signature(fun_info)));
        }
    }
    fn_candidates.sort_by(|(a, _), (b, _)| a.text.cmp(&b.text));

    for (name, signature) in fn_candidates {
        for perm in &perms {
            let args: Vec<Value> = perm.iter().map(|i| inputs[*i].1.clone()).collect();

            reset_eval_state(env);
            let Ok(value) = eval_toplevel_call(&name, &args, env, session, call_vfs_path) else {
                continue;
            };
            if &value != output {
                continue;
            }

            let arg_srcs: Vec<String> = perm.iter().map(|i| inputs[*i].0.clone()).collect();
            suggestions.push(Suggestion {
                kind: "function".to_owned(),
                name: name.text.clone(),
                call: format!("{}({})", name.text, arg_srcs.join(", ")),
                signature: signature.clone(),
            });
            // One example call per function is enough.
            break;
        }
    }

    // Methods in scope. A method with `n - 1` parameters (in addition
    // to the receiver) is a candidate when we have `n` inputs.
    let mut method_candidates: Vec<(TypeName, SymbolName, String)> = vec![];
    if n >= 1 {
        for (type_name, type_and_methods) in env.types.iter() {
            for (method_name, method_info) in type_and_methods.methods.iter() {
                let Some(fun_info) = method_info.fun_info() else {
                    continue;
                };
                if fun_info.params.params.len() + 1 != n {
                    continue;
                }

                method_candidates.push((
                    type_name.clone(),
                    method_name.clone(),
                    method_signature(method_info, fun_info),
                ));
            }
        }
    }
    method_candidates.sort_by(|(a_ty, a_name, _), (b_ty, b_name, _)| {
        a_ty.text
            .cmp(&b_ty.text)
            .then_with(|| a_name.text.cmp(&b_name.text))
    });

    for (type_name, method_name, signature) in method_candidates {
        for perm in &perms {
            let (recv_src, recv_value) = &inputs[perm[0]];

            // Methods are dispatched on the runtime type of the
            // receiver, so only try receivers of the right type.
            if type_representation(recv_value) != type_name {
                continue;
            }

            let args: Vec<Value> = perm[1..].iter().map(|i| inputs[*i].1.clone()).collect();

            reset_eval_state(env);
            let Ok(value) = eval_toplevel_method_call(
                recv_value,
                &method_name,
                &args,
                env,
                session,
                call_vfs_path,
            ) else {
                continue;
            };
            if &value != output {
                continue;
            }

            let arg_srcs: Vec<String> = perm[1..].iter().map(|i| inputs[*i].0.clone()).collect();
            suggestions.push(Suggestion {
                kind: "method".to_owned(),
                name: method_name.text.clone(),
                call: format!("{}.{}({})", recv_src, method_name.text, arg_srcs.join(", ")),
                signature: signature.clone(),
            });
            // One example call per method is enough.
            break;
        }
    }

    suggestions
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
