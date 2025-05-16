use std::io::IsTerminal as _;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use owo_colors::OwoColorize as _;
use rustc_hash::FxHashMap;
use serde::Serialize;

use crate::eval::eval_tests;
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::{
    load_toplevel_items, parse_toplevel_items_or_die, Env, EvalError, Session, StdoutMode,
};

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
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);
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
    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns);

    // TODO: allow users to choose this value.
    //
    // Currently it's chosen by bumping it if writing a sample file
    // that hits the limit.
    env.tick_limit = Some(100_000);
    env.enforce_sandbox = true;

    let mut test_at_cursor = None;
    for item in items.iter() {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) && matches!(item, ToplevelItem::Test(_)) {
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
            Some(EvalError::Interrupted) => {
                num_errored += 1;
                "errored: interrupted".to_owned()
            }
            Some(EvalError::ResumableError(_, msg)) => {
                num_errored += 1;
                format!("errored: {}", msg.as_string())
            }
            Some(EvalError::AssertionFailed(_, msg)) => {
                num_failed += 1;
                format!("failed: {}", msg.as_string())
            }
            Some(EvalError::ReachedTickLimit(_)) => {
                num_timed_out += 1;
                "timed_out".to_owned()
            }
            Some(EvalError::ForbiddenInSandbox(_)) => {
                num_sandboxed += 1;
                "sandboxed".to_owned()
            }
            None => {
                num_passed += 1;
                "passed".to_owned()
            }
        };
        tests.insert(test_sym.name.text.clone(), msg);
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

pub(crate) fn run_tests_in_files(
    srcs_and_paths: &[(String, PathBuf)],
    interrupted: Arc<AtomicBool>,
) {
    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    let id_gen = IdGenerator::default();
    let mut env = Env::new(id_gen, Vfs::default());

    let mut all_items = vec![];

    for (src, path) in srcs_and_paths {
        let vfs_path = env.vfs.insert(Rc::new(path.to_owned()), src.to_owned());

        let items = parse_toplevel_items_or_die(&vfs_path, src, &mut env.vfs, &mut env.id_gen);

        let ns = env.get_or_create_namespace(path);
        load_toplevel_items(&items, &mut env, ns);

        all_items.extend_from_slice(&items);
    }

    let summary = eval_tests(&all_items, &mut env, &session);

    let total_tests = summary.tests.len();
    let tests_failed = summary
        .tests
        .iter()
        .filter(|(_, err)| err.is_some())
        .count();
    let tests_passed = total_tests - tests_failed;

    let use_color = std::io::stdout().is_terminal();
    if tests_passed == 0 && tests_failed == 0 {
        println!("No tests found.");
    } else {
        for (test_sym, err) in &summary.tests {
            let Some(err) = err else {
                continue;
            };

            print!(
                "Failed: {}",
                if use_color {
                    test_sym.name.text.bold().to_string()
                } else {
                    test_sym.name.text.clone()
                }
            );

            let (pos, msg) = match err {
                EvalError::Interrupted => (None, None),
                EvalError::ResumableError(position, msg) => (Some(position), Some(msg)),
                EvalError::AssertionFailed(position, msg) => (Some(position), Some(msg)),
                EvalError::ReachedTickLimit(position) => (Some(position), None),
                EvalError::ForbiddenInSandbox(position) => (Some(position), None),
            };

            match (pos, msg) {
                (Some(pos), Some(msg)) => {
                    println!(" {}\n  {}", pos.as_ide_string(), msg.as_string())
                }
                (Some(pos), None) => println!(" {}", pos.as_ide_string()),
                _ => println!(),
            }
        }

        if tests_failed > 0 {
            println!();
        }

        let total_tests = tests_passed + tests_failed;

        if tests_failed == 0 && total_tests == 1 {
            println!("Ran 1 test: it passed.");
        } else if tests_failed == 0 {
            println!(
                "Ran {} test{}: all tests passed.",
                total_tests,
                if total_tests == 1 { "" } else { "s" },
            );
        } else {
            println!(
                "Ran {} test{}: {} passed and {} failed.",
                total_tests,
                if total_tests == 1 { "" } else { "s" },
                tests_passed,
                tests_failed
            );
        }
    }

    // TODO: support printing back traces from every test failure.
    // TODO: print incremental progress as tests run.

    if tests_failed > 0 {
        std::process::exit(1);
    }
}
