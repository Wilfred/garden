//! Sandboxed playground execution for running Garden code snippets.

use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use serde::Serialize;

use crate::env::Env;
use crate::eval::{eval_toplevel_items, EvalError, Session, StdoutMode};
use crate::parser::ast::IdGenerator;
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;

#[derive(Serialize)]
struct PlaygroundResponse {
    error: Option<String>,
}

/// Run a Garden program in sandboxed mode and print the result as JSON.
///
/// Evaluates all toplevel items in order, stopping on the first error.
/// Prints JSON with an `error` field that is null on success or contains
/// an error message on failure.
pub(crate) fn run_sandboxed_playground(src: &str, path: &Path, interrupted: Arc<AtomicBool>) {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);

    // Set the toplevel stack frame as also in the file namespace.
    let ns = env.get_or_create_namespace(path);
    let frame = env.current_frame_mut();
    frame.namespace = ns;

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteJson,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: true,
    };

    let response = match eval_toplevel_items(&vfs_path, &items, &mut env, &session) {
        Ok(_) => PlaygroundResponse { error: None },
        Err(EvalError::Exception(_, msg)) => PlaygroundResponse {
            error: Some(msg.as_string()),
        },
        Err(EvalError::AssertionFailed(_, msg)) => PlaygroundResponse {
            error: Some(msg.as_string()),
        },
        Err(EvalError::Interrupted) => PlaygroundResponse {
            error: Some("Interrupted".to_owned()),
        },
        Err(EvalError::ReachedTickLimit(_)) => PlaygroundResponse {
            error: Some("Reached the tick limit".to_owned()),
        },
        Err(EvalError::ReachedStackLimit(_)) => PlaygroundResponse {
            error: Some("Reached the stack limit".to_owned()),
        },
        Err(EvalError::ForbiddenInSandbox(_)) => PlaygroundResponse {
            error: Some("Tried to execute unsafe code in sandboxed mode".to_owned()),
        },
    };

    let json = serde_json::to_string(&response).expect("Failed to serialize response");
    println!("{}", json);
}
