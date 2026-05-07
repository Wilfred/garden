//! Sandboxed playground execution for running Garden code snippets.

use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use serde::Serialize;

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::Severity;
use crate::env::Env;
use crate::eval::{
    eval_exprs, eval_tests, load_toplevel_items, EvalError, ExceptionInfo, Session,
    StdoutJsonFormat, StdoutMode, ToplevelEvalSummary,
};
use crate::parser::ast::{Expression, IdGenerator, ToplevelItem};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::test_runner::describe_tests;
use crate::values::Value;

#[derive(Serialize)]
struct PlaygroundResponse {
    error: Option<String>,
    value: Option<String>,
}

/// Run a Garden program in sandboxed mode and print the result as JSON.
///
/// Evaluates each toplevel expression in order, stopping on the first
/// error. Each evaluated value (excluding `Unit`) is emitted as its
/// own JSON response.
pub(crate) fn run_sandboxed_playground(src: &str, path: &Path, interrupted: Arc<AtomicBool>) {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_or_create_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    // Enable sandbox restrictions and resource limits.
    env.tick_limit = Some(100_000);
    env.stack_limit = Some(1_000);
    env.enforce_sandbox = true;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteJson(StdoutJsonFormat::Playground),
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    let responses = match run(&items, &mut env, &session, &vfs_path) {
        Ok((summary, values)) => {
            let mut items = vec![];
            if !summary.tests.is_empty() {
                items.push(PlaygroundResponse {
                    error: None,
                    value: Some(describe_tests(&env, &summary)),
                });
            }

            let displays: Vec<String> = values
                .iter()
                .filter_map(|v| v.display_unless_unit(&env))
                .collect();

            if displays.is_empty() {
                items.push(PlaygroundResponse {
                    error: None,
                    value: Some("Unit".to_owned()),
                });
            } else {
                for display in displays {
                    items.push(PlaygroundResponse {
                        error: None,
                        value: Some(display),
                    });
                }
            }

            items
        }
        Err(EvalError::Exception(ExceptionInfo {
            position: _,
            message: msg,
        })) => vec![PlaygroundResponse {
            error: Some(msg.as_string()),
            value: None,
        }],
        Err(EvalError::AssertionFailed(_, msg)) => vec![PlaygroundResponse {
            error: Some(msg.as_string()),
            value: None,
        }],
        Err(EvalError::Interrupted) => vec![PlaygroundResponse {
            error: Some("Interrupted".to_owned()),
            value: None,
        }],
        Err(EvalError::ReachedTickLimit(_)) => vec![PlaygroundResponse {
            error: Some("Reached the tick limit".to_owned()),
            value: None,
        }],
        Err(EvalError::ReachedStackLimit(_)) => vec![PlaygroundResponse {
            error: Some("Reached the stack limit".to_owned()),
            value: None,
        }],
        Err(EvalError::ForbiddenInSandbox(_)) => vec![PlaygroundResponse {
            error: Some("Tried to execute unsafe code in sandboxed mode".to_owned()),
            value: None,
        }],
    };

    for response in responses {
        let json = serde_json::to_string(&response).expect("Failed to serialize response");
        println!("{}", json);
    }
}

/// Evaluate the toplevel items, capturing a value for each toplevel
/// expression or block individually.
fn run(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
    vfs_path: &crate::parser::vfs::VfsPathBuf,
) -> Result<(ToplevelEvalSummary, Vec<Value>), EvalError> {
    let mut defs = vec![];
    let mut expr_groups: Vec<Vec<Expression>> = vec![];
    for item in items {
        match item {
            ToplevelItem::Expr(toplevel_expression) => {
                expr_groups.push(vec![toplevel_expression.0.clone()]);
            }
            ToplevelItem::Block(block) => {
                expr_groups.push(block.exprs.iter().map(|e| e.as_ref().clone()).collect());
            }
            _ => {
                defs.push(item.clone());
            }
        }
    }

    let ns = env.current_namespace();
    let (mut diagnostics, _new_syms) = load_toplevel_items(&defs, env, ns.clone());
    for diagnostic in diagnostics.iter() {
        if matches!(diagnostic.severity, Severity::Error) {
            return Err(EvalError::Exception(ExceptionInfo {
                position: diagnostic.position.clone(),
                message: diagnostic.message.clone(),
            }));
        }
    }

    diagnostics.extend(check_toplevel_items_in_env(vfs_path, items, env, ns));

    let summary = eval_tests(items, env, session);

    let mut values = vec![];
    for group in &expr_groups {
        let group_values = eval_exprs(group, env, session)?;
        if let Some(last) = group_values.into_iter().last() {
            values.push(last);
        }
    }

    Ok((summary, values))
}
