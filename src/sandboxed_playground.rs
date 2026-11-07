//! Sandboxed playground execution for running Garden code snippets.

use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use serde::Serialize;

use crate::env::Env;
use crate::eval::{
    eval_toplevel_items, EvalError, ExceptionInfo, Session, StdoutJsonFormat, StdoutStderrMode,
};
use crate::garden_type::Type;
use crate::parser::ast::IdGenerator;
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::test_runner::describe_tests;
use crate::values::Value;

#[derive(Serialize)]
struct PlaygroundResponse {
    error: Option<String>,
    value: Option<String>,
    #[serde(rename = "type")]
    type_: Option<String>,
}

/// Run a sandboxed Garden program and print its value, type, or error as JSON.
pub(crate) fn run_sandboxed_playground(
    src: &str,
    path: &Path,
    interrupted: Arc<AtomicBool>,
    trace_exprs: bool,
) {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);

    let mut prelude_types = env
        .prelude_namespace
        .borrow()
        .types
        .keys()
        .map(|name| name.text.clone())
        .collect::<Vec<_>>();
    prelude_types.sort();
    println!("{}", serde_json::json!({ "prelude_types": prelude_types }));

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
        stdout_stderr_mode: StdoutStderrMode::WriteJson(StdoutJsonFormat::Playground),
        start_time: Instant::now(),
        trace_exprs,
        pretty_print_json: false,
    };

    let responses = match eval_toplevel_items(&vfs_path, &items, &mut env, &session) {
        Ok(summary) => {
            let mut items = vec![];
            if !summary.tests.is_empty() {
                items.push(PlaygroundResponse {
                    error: None,
                    value: Some(describe_tests(&env, &summary)),
                    type_: None,
                });
            }

            let last_value = summary.values.last().cloned().unwrap_or_else(Value::unit);
            let value_display = last_value.display(&env);
            items.push(PlaygroundResponse {
                error: None,
                value: Some(value_display),
                type_: Some(Type::from_value(&last_value).to_string()),
            });

            items
        }
        Err(EvalError::Exception(ExceptionInfo {
            position: _,
            message: msg,
        })) => vec![PlaygroundResponse {
            error: Some(msg.as_string()),
            value: None,
            type_: None,
        }],
        Err(EvalError::AssertionFailed(_, msg)) => vec![PlaygroundResponse {
            error: Some(msg.as_string()),
            value: None,
            type_: None,
        }],
        Err(EvalError::Interrupted) => vec![PlaygroundResponse {
            error: Some("Interrupted".to_owned()),
            value: None,
            type_: None,
        }],
        Err(EvalError::ReachedTickLimit(_)) => vec![PlaygroundResponse {
            error: Some("Reached the tick limit".to_owned()),
            value: None,
            type_: None,
        }],
        Err(EvalError::ReachedStackLimit(_)) => vec![PlaygroundResponse {
            error: Some("Reached the stack limit".to_owned()),
            value: None,
            type_: None,
        }],
        Err(EvalError::ForbiddenInSandbox(_)) => vec![PlaygroundResponse {
            error: Some("Tried to execute unsafe code in sandboxed mode".to_owned()),
            value: None,
            type_: None,
        }],
    };

    for response in responses {
        let json = serde_json::to_string(&response).expect("Failed to serialize response");
        println!("{}", json);
    }
}
