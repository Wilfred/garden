use std::{
    path::Path,
    sync::{atomic::AtomicBool, Arc},
    time::Instant,
};

use garden_lang_parser::{
    ast::{IdGenerator, ToplevelItem_},
    parse_toplevel_items,
};
use rustc_hash::FxHashMap;
use serde::Serialize;

use crate::{eval::eval_tests, parse_toplevel_items_or_die, EvalError};
use crate::{load_toplevel_items, Env, Session, StdoutMode};

#[derive(Serialize, Debug)]
struct SandboxedTestsSummary {
    description: String,
    tests: FxHashMap<String, String>,
}

fn sandboxed_tests_summary(
    src: &str,
    path: &Path,
    offset: usize,
    interrupted: Arc<AtomicBool>,
) -> SandboxedTestsSummary {
    let id_gen = IdGenerator::default();
    let mut env = Env::new(id_gen);

    let (items, errors) = parse_toplevel_items(path, src, &mut env.id_gen);
    if !errors.is_empty() {
        return SandboxedTestsSummary {
            description: "Parse error".to_owned(),
            tests: FxHashMap::default(),
        };
    }

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::DoNotWrite,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    // TODO: for real IDE usage we'll want to use the environment of
    // the current session.
    load_toplevel_items(&items, &mut env);

    // TODO: allow users to choose this value.
    //
    // Currently it's chosen by bumping it if writing a sample file
    // that hits the limit.
    env.tick_limit = Some(100_000);
    env.enforce_sandbox = true;

    let mut test_at_cursor = None;
    for item in items.iter() {
        if item.1.contains_offset(offset) && matches!(item.2, ToplevelItem_::Test(_)) {
            test_at_cursor = Some(item.clone());
            break;
        }
    }

    let relevant_items = match &test_at_cursor {
        Some(test_item) => vec![test_item.clone()],
        None => items,
    };

    let summary = eval_tests(&relevant_items, &mut env, &session);

    let mut num_passed = 0;
    let mut num_failed = 0;
    let mut num_errored = 0;
    let mut num_sandboxed = 0;
    let mut num_timed_out = 0;

    let mut tests = FxHashMap::default();
    for (test_sym, err) in &summary.tests {
        let msg = match err {
            Some(EvalError::Interrupted | EvalError::ResumableError(_, _)) => {
                num_errored += 1;
                "errored"
            }
            Some(EvalError::AssertionFailed(_, _)) => {
                num_failed += 1;
                "failed"
            }
            Some(EvalError::ReachedTickLimit(_)) => {
                num_timed_out += 1;
                "timed_out"
            }
            Some(EvalError::ForbiddenInSandbox(_)) => {
                num_sandboxed += 1;
                "sandboxed"
            }
            None => {
                num_passed += 1;
                "passed"
            }
        };
        tests.insert(test_sym.name.name.clone(), msg.to_owned());
    }

    if test_at_cursor.is_some() {
        let description = if num_passed == 1 {
            "passing"
        } else if num_failed == 1 {
            "failing"
        } else if num_errored == 1 {
            "erroring"
        } else if num_timed_out == 1 {
            "timing out"
        } else if num_sandboxed == 1 {
            "sandboxed"
        } else {
            "unknown state"
        }
        .to_owned();

        return SandboxedTestsSummary { description, tests };
    }

    let mut parts = vec![];
    if num_passed > 0 {
        parts.push(format!("{} passed", num_passed));
    }
    if num_failed > 0 {
        parts.push(format!("{} failed", num_failed));
    }
    if num_errored > 0 {
        parts.push(format!("{} errored", num_errored));
    }
    if num_timed_out > 0 {
        parts.push(format!("{} timed out", num_timed_out));
    }
    if num_sandboxed > 0 {
        parts.push(format!("{} sandboxed", num_sandboxed));
    }

    if parts.is_empty() {
        parts.push("No tests".to_owned());
    }

    SandboxedTestsSummary {
        description: parts.join(", "),
        tests,
    }
}

pub(crate) fn run_sandboxed_tests_in_file(
    src: &str,
    path: &Path,
    offset: usize,
    interrupted: Arc<AtomicBool>,
) {
    let summary = sandboxed_tests_summary(src, path, offset, interrupted);
    println!("{}", serde_json::to_string(&summary).unwrap());
}

pub(crate) fn run_tests_in_file(src: &str, path: &Path, interrupted: Arc<AtomicBool>) {
    let id_gen = IdGenerator::default();
    let mut env = Env::new(id_gen);
    let items = parse_toplevel_items_or_die(path, src, &mut env.id_gen);

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    load_toplevel_items(&items, &mut env);

    let summary = eval_tests(&items, &mut env, &session);

    let total_tests = summary.tests.len();
    let tests_failed = summary
        .tests
        .iter()
        .filter(|(_, err)| err.is_some())
        .count();
    let tests_passed = total_tests - tests_failed;

    if tests_passed == 0 && tests_failed == 0 {
        println!("No tests found.");
    } else {
        println!("{} passed, {} failed.", tests_passed, tests_failed,);

        for (test_sym, err) in &summary.tests {
            let Some(err) = err else {
                continue;
            };

            print!("Failed: {}", test_sym.name);

            let pos = match err {
                EvalError::Interrupted => None,
                EvalError::ResumableError(position, _) => Some(position),
                EvalError::AssertionFailed(position, _) => Some(position),
                EvalError::ReachedTickLimit(position) => Some(position),
                EvalError::ForbiddenInSandbox(position) => Some(position),
            };
            match pos {
                Some(pos) => println!(" {}", pos.as_ide_string()),
                None => println!(),
            }
        }
    }

    // TODO: support printing back traces from every test failure.
    // TODO: print incremental progress as tests run.

    if tests_failed > 0 {
        std::process::exit(1);
    }
}
