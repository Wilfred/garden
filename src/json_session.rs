use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread::JoinHandle;
use std::time::Instant;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::{format_diagnostic, format_error_with_stack, Diagnostic, Level};
use crate::env::Env;
use crate::eval::{
    eval, eval_tests_until_error, eval_toplevel_exprs_then_stop, eval_up_to, load_toplevel_items,
    push_test_stackframe, EvalUpToErr, ExpressionState, StdoutMode,
};
use crate::{
    commands::{print_available_commands, run_command, Command, CommandParseError, EvalAction},
    eval::{EvalError, Session},
};
use garden_lang_parser::ast::{IdGenerator, SourceString, Vfs};
use garden_lang_parser::position::Position;
use garden_lang_parser::{parse_toplevel_items, parse_toplevel_items_from_span, ParseError};

type RequestId = usize;

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "method", rename_all = "snake_case")]
enum Request {
    Run {
        input: String,
        // TODO: distinguish methods 'running in REPL' from 'running a snippet
        // in a file', so we don't need these optional fields.
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<PathBuf>,
        #[serde(skip_serializing_if = "Option::is_none")]
        offset: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        end_offset: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<RequestId>,
    },
    Load {
        input: String,
        path: PathBuf,
        offset: usize,
        end_offset: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<RequestId>,
    },
    /// Stop the current evaluation, as if we'd pressed Ctrl-c.
    Interrupt,
    EvalUpTo {
        path: Option<PathBuf>,
        src: String,
        offset: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<RequestId>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct DiagnosticForJson {
    // Not an ErrorMessage.
    message: String,
    position: Position,
    level: Level,
}

impl From<Diagnostic> for DiagnosticForJson {
    fn from(value: Diagnostic) -> Self {
        Self {
            message: value.message.as_string(),
            position: value.position,
            level: value.level,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ResponseKind {
    Evaluate {
        warnings: Vec<DiagnosticForJson>,
        value: Result<Option<String>, Vec<ResponseError>>,
        stack_frame_name: Option<String>,
    },
    RunCommand {
        message: String,
        stack_frame_name: Option<String>,
    },
    Ready {
        message: String,
    },
    MalformedRequest {
        message: String,
    },
    Printed {
        s: String,
    },
    FoundDefinition,
    /// We received an interrupt request. The current (or next)
    /// evaluate request will return an error of "Interrupted".
    Interrupted {
        stack_frame_name: Option<String>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct ReadyResponse {
    message: String,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub(crate) struct ResponseError {
    position: Option<Position>,
    message: String,
    stack: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct Response {
    pub(crate) kind: ResponseKind,
    pub(crate) position: Option<Position>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) id: Option<RequestId>,
}

pub(crate) fn sample_request_as_json() -> String {
    serde_json::to_string(&Request::Run {
        input: "1 + 2".into(),
        path: Some(PathBuf::from("/foo/bar.gdn")),
        offset: Some(100),
        end_offset: Some(110),
        id: Some(123),
    })
    .unwrap()
}

fn handle_load_request(
    id: Option<usize>,
    path: &Path,
    input: &str,
    offset: usize,
    end_offset: usize,
    env: &mut Env,
) -> Response {
    let (items, errors) =
        parse_toplevel_items_from_span(path, input, &mut env.id_gen, offset, end_offset);

    if !errors.is_empty() {
        return as_error_response(errors, input);
    }

    let (diagnostics, new_syms) = load_toplevel_items(&items, env);

    let summary = if new_syms.is_empty() {
        "".to_owned()
    } else if new_syms.len() == 1 {
        format!("Loaded {}.", new_syms[0].name)
    } else {
        format!("Loaded {} definitions.", new_syms.len())
    };

    Response {
        kind: ResponseKind::Evaluate {
            warnings: diagnostics.into_iter().map(Into::into).collect::<Vec<_>>(),
            value: Ok(Some(summary)),
            stack_frame_name: Some(env.top_frame_name()),
        },
        position: None,
        id,
    }
}

pub(crate) fn start_eval_thread(session: Session, receiver: Receiver<String>) -> JoinHandle<()> {
    std::thread::Builder::new()
        .name("eval".to_owned())
        .spawn(move || eval_worker(receiver, session))
        .unwrap()
}

fn eval_worker(receiver: Receiver<String>, session: Session) {
    let mut session = session;

    let id_gen = IdGenerator::default();
    let vfs = Vfs::default();
    let mut env = Env::new(id_gen, vfs);

    while let Ok(input) = receiver.recv() {
        handle_request_in_worker(&input, &mut env, &mut session);
    }
}

fn as_error_response(errors: Vec<ParseError>, input: &str) -> Response {
    let response_errors: Vec<_> = errors
        .into_iter()
        .map(|e| match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_diagnostic(
                    &message,
                    &position,
                    Level::Error,
                    &SourceString {
                        src: input.to_owned(),
                        offset: 0,
                    },
                ));

                ResponseError {
                    position: Some(position),
                    message: message.as_string(),
                    stack,
                }
            }
            ParseError::Incomplete { message, .. } => ResponseError {
                position: None,
                message: message.as_string(),
                stack: None,
            },
        })
        .collect();

    Response {
        kind: ResponseKind::Evaluate {
            warnings: vec![],
            value: Err(response_errors),
            stack_frame_name: None,
        },
        position: None,
        id: None,
    }
}

fn handle_eval_up_to_request(
    path: Option<&PathBuf>,
    src: &str,
    offset: usize,
    env: &mut Env,
    session: &Session,
    id: Option<RequestId>,
) -> Response {
    let (items, mut errors) = parse_toplevel_items(
        &path
            .cloned()
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        src,
        &mut env.vfs,
        &mut env.id_gen,
    );

    if let Some(e) = errors.pop() {
        match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_diagnostic(
                    &message,
                    &position,
                    Level::Error,
                    &SourceString {
                        src: src.to_owned(),
                        offset: 0,
                    },
                ));
                return Response {
                    kind: ResponseKind::Evaluate {
                        warnings: vec![],
                        value: Err(vec![ResponseError {
                            position: Some(position),
                            message: message.as_string(),
                            stack,
                        }]),
                        stack_frame_name: None,
                    },
                    position: None,
                    id,
                };
            }
            ParseError::Incomplete { message, .. } => {
                return Response {
                    kind: ResponseKind::Evaluate {
                        warnings: vec![],
                        value: Err(vec![ResponseError {
                            position: None,
                            message: message.as_string(),
                            stack: None,
                        }]),
                        stack_frame_name: None,
                    },
                    position: None,
                    id,
                };
            }
        }
    }

    match eval_up_to(env, session, &items, offset) {
        Ok((v, pos)) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Ok(Some(v.display(env))),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: Some(pos),
            id: None,
        },
        Err(EvalUpToErr::EvalError(e)) => err_to_response(e, env, id),
        Err(EvalUpToErr::NoExpressionFound) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Ok(Some("Did not find an expression to evaluate.".to_owned())),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id,
        },
        Err(EvalUpToErr::NoValueAvailable) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Ok(Some(
                    "No previous value saved for this expression".to_owned(),
                )),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id,
        },
    }
}

fn err_to_response(e: EvalError, env: &Env, id: Option<RequestId>) -> Response {
    match e {
        EvalError::ResumableError(position, e) => {
            // TODO: use the original SourceString rather than reconstructing.
            let stack = format_error_with_stack(&e, &position, &env.stack.0);

            Response {
                kind: ResponseKind::Evaluate {
                    warnings: vec![],
                    value: Err(vec![ResponseError {
                        position: Some(position),
                        message: format!("Error: {}", e.as_string()),
                        stack: Some(stack),
                    }]),
                    stack_frame_name: Some(env.top_frame_name()),
                },
                position: None,
                id,
            }
        }
        EvalError::AssertionFailed(position, message) => {
            let stack = format_error_with_stack(&message, &position, &env.stack.0);

            Response {
                kind: ResponseKind::Evaluate {
                    warnings: vec![],
                    value: Err(vec![ResponseError {
                        position: Some(position),
                        message: "Assertion failed".to_owned(),
                        stack: Some(stack),
                    }]),
                    stack_frame_name: Some(env.top_frame_name()),
                },
                position: None,
                id,
            }
        }
        EvalError::Interrupted => Response {
            kind: ResponseKind::Interrupted {
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
        EvalError::ReachedTickLimit(position) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: Some(position.clone()),
                    message: "Reached the tick limit.".to_owned(),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: Some(position),
            id,
        },
        EvalError::ForbiddenInSandbox(position) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: Some(position),
                    message: "Tried to execute unsafe code in sandboxed mode.".to_owned(),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id,
        },
    }
}

pub(crate) fn print_as_json(res: &Response, pretty_print_json: bool) {
    let serialized = if pretty_print_json {
        serde_json::to_string_pretty(&res)
    } else {
        serde_json::to_string(&res)
    }
    .unwrap();
    println!("{}", serialized);
}

/// Process a request and print the response as JSON on stdout.
pub(crate) fn handle_request(
    req_src: &str,
    pretty_print: bool,
    interrupted: Arc<AtomicBool>,
    sender: Sender<String>,
) {
    if let Ok(Request::Interrupt) = serde_json::from_str::<Request>(req_src) {
        interrupted.store(true, std::sync::atomic::Ordering::Relaxed);
        let res = Response {
            kind: ResponseKind::Interrupted {
                stack_frame_name: None,
            },
            position: None,
            id: None,
        };
        print_as_json(&res, pretty_print);
        return;
    }

    sender.send(req_src.to_owned()).unwrap();
}

fn handle_request_in_worker(req_src: &str, env: &mut Env, session: &mut Session) {
    let Ok(req) = serde_json::from_str::<Request>(req_src) else {
        let info = match serde_json::from_str::<serde_json::Value>(req_src) {
            Ok(_) => "valid JSON, but not a valid request",
            Err(_) => "malformed JSON",
        };

        let res = Response {
            kind: ResponseKind::MalformedRequest { message: format!(
                "Invalid request ({info}). A valid request looks like: {}. The request received was:\n\n{req_src}",
            sample_request_as_json(),
        ) },
            position: None,
            id: None,
        };

        print_as_json(&res, session.pretty_print_json);
        return;
    };

    let res = match req {
        Request::Load {
            input,
            path,
            offset,
            end_offset,
            id,
        } => handle_load_request(id, &path, &input, offset, end_offset, env),
        Request::Interrupt => {
            // Nothing to do, handled outside this threaad so it
            // doesn't require locking env.
            return;
        }
        Request::Run {
            path,
            input,
            offset,
            end_offset,
            id,
        } => handle_run_request(input, env, session, id, path, offset, end_offset),
        Request::EvalUpTo {
            path,
            src,
            offset,
            id,
        } => handle_eval_up_to_request(path.as_ref(), &src, offset, env, session, id),
    };

    print_as_json(&res, session.pretty_print_json);
}

fn handle_run_request(
    input: String,
    env: &mut Env,
    session: &mut Session,
    id: Option<usize>,
    path: Option<PathBuf>,
    offset: Option<usize>,
    end_offset: Option<usize>,
) -> Response {
    match Command::from_string(&input) {
        Ok(command) => {
            let mut out_buf: Vec<u8> = vec![];
            match run_command(&mut out_buf, &command, env, session) {
                Ok(()) => Response {
                    kind: ResponseKind::RunCommand {
                        message: format!("{}", String::from_utf8_lossy(&out_buf)),
                        stack_frame_name: Some(env.top_frame_name()),
                    },
                    position: None,
                    id,
                },
                Err(EvalAction::Abort) => Response {
                    kind: ResponseKind::RunCommand {
                        message: "Aborted".to_owned(),
                        stack_frame_name: Some(env.top_frame_name()),
                    },
                    position: None,
                    id,
                },
                Err(EvalAction::Resume) => eval_to_response(env, session),
                Err(EvalAction::RunTest(name)) => {
                    let test_opt = env.tests.get(&name).cloned();
                    match test_opt {
                        Some(test) => {
                            push_test_stackframe(&test, env);
                            eval_to_response(env, session)
                        }
                        None => Response {
                            kind: ResponseKind::MalformedRequest {
                                message: format!("No such test: {}", name),
                            },
                            position: None,
                            id,
                        },
                    }
                }
                Err(EvalAction::Replace(expr)) => {
                    let stack_frame = env.stack.0.last_mut().unwrap();

                    stack_frame.evalled_values.pop();
                    stack_frame
                        .exprs_to_eval
                        .push((ExpressionState::NotEvaluated, expr.into()));

                    eval_to_response(env, session)
                }
                Err(EvalAction::Skip) => {
                    let stack_frame = env.stack.0.last_mut().unwrap();

                    stack_frame
                        .exprs_to_eval
                        .pop()
                        .expect("Tried to skip an expression, but none in this frame.");

                    eval_to_response(env, session)
                }
            }
        }
        Err(CommandParseError::NoSuchCommand(s)) => {
            let mut out_buf: Vec<u8> = vec![];
            print_available_commands(&s, &mut out_buf).unwrap();

            Response {
                kind: ResponseKind::RunCommand {
                    message: format!("{}", String::from_utf8_lossy(&out_buf)),
                    stack_frame_name: Some(env.top_frame_name()),
                },
                position: None,
                id,
            }
        }
        Err(CommandParseError::NotCommandSyntax) => {
            handle_run_eval_request(path.as_ref(), &input, offset, end_offset, env, session, id)
        }
    }
}

fn handle_run_eval_request(
    path: Option<&PathBuf>,
    input: &str,
    offset: Option<usize>,
    end_offset: Option<usize>,
    env: &mut Env,
    session: &mut Session,
    id: Option<RequestId>,
) -> Response {
    let (items, errors) = parse_toplevel_items_from_span(
        &path
            .cloned()
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        input,
        &mut env.id_gen,
        offset.unwrap_or(0),
        end_offset.unwrap_or(input.len()),
    );

    if !errors.is_empty() {
        return as_error_response(errors, input);
    }

    let (mut diagnostics, new_syms) = load_toplevel_items(&items, env);
    diagnostics.extend(check_toplevel_items_in_env(&items, env));

    let test_summary = match eval_tests_until_error(&items, env, session) {
        Ok(s) => s,
        Err(e) => return err_to_response(e, env, id),
    };

    match eval_toplevel_exprs_then_stop(&items, env, session) {
        Ok(value) => {
            let definition_summary = if new_syms.is_empty() {
                "".to_owned()
            } else if new_syms.len() == 1 {
                format!("Loaded {}", new_syms[0].name)
            } else {
                format!("Loaded {} definitions", new_syms.len())
            };

            let total_tests = test_summary.tests.len();
            let tests_failed = test_summary
                .tests
                .iter()
                .filter(|(_, err)| err.is_some())
                .count();
            let tests_passed = total_tests - tests_failed;

            let test_summary = if total_tests == 0 {
                "".to_owned()
            } else {
                format!(
                    "{total_tests} {} ({tests_passed} passed, {tests_failed} failed)",
                    if total_tests == 1 { "test" } else { "tests" }
                )
            };

            let test_summary = match (test_summary.is_empty(), definition_summary.is_empty()) {
                (true, _) => "".to_owned(),
                (false, true) => format!("Ran {test_summary}"),
                (false, false) => format!(", ran {test_summary}"),
            };

            let summary = format!("{definition_summary}{test_summary}");

            let value_summary = if let Some(last_value) = value {
                Some(if summary.is_empty() {
                    last_value.display(env)
                } else {
                    format!(
                        "{summary}, and the expression evaluated to {}.",
                        last_value.display(env)
                    )
                })
            } else if summary.is_empty() {
                None
            } else {
                Some(format!("{summary}."))
            };

            Response {
                kind: ResponseKind::Evaluate {
                    warnings: diagnostics.into_iter().map(Into::into).collect::<Vec<_>>(),
                    value: Ok(value_summary),
                    stack_frame_name: Some(env.top_frame_name()),
                },
                position: None,
                id,
            }
        }
        Err(e) => err_to_response(e, env, id),
    }
}

fn eval_to_response(env: &mut Env, session: &Session) -> Response {
    match eval(env, session) {
        Ok(result) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Ok(Some(result.display(env))),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
        Err(EvalError::ResumableError(position, e)) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: Some(position),
                    message: format!("Error: {}", e.as_string()),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
        Err(EvalError::AssertionFailed(position, message)) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    message: message.as_string(),
                    position: Some(position),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
        Err(EvalError::Interrupted) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: None,
                    message: "Interrupted".to_owned(),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
        Err(EvalError::ReachedTickLimit(position)) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: Some(position.clone()),
                    message: "Reached the tick limit.".to_owned(),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: Some(position),
            id: None,
        },
        Err(EvalError::ForbiddenInSandbox(position)) => Response {
            kind: ResponseKind::Evaluate {
                warnings: vec![],
                value: Err(vec![ResponseError {
                    position: Some(position),
                    message: "Tried to execute unsafe code in sandboxed mode.".to_owned(),
                    stack: None,
                }]),
                stack_frame_name: Some(env.top_frame_name()),
            },
            position: None,
            id: None,
        },
    }
}

pub(crate) fn json_session(interrupted: Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready {
            message: "The Garden: Good programs take time to grow.".to_owned(),
        },
        position: None,
        id: None,
    };
    let pretty_print_json = false;

    print_as_json(&response, pretty_print_json);

    let pretty_print_json = false;
    let session = Session {
        interrupted: Arc::clone(&interrupted),
        stdout_mode: StdoutMode::WriteJson,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json,
    };

    let (sender, receiver) = channel::<String>();

    start_eval_thread(session, receiver);

    loop {
        let mut line = String::new();
        let stdin = std::io::stdin();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");

        if line.trim() == "" {
            // TODO: Once Emacs flushes properly, we won't see blank lines.
            continue;
        }

        if let Some(length_str) = line.trim().strip_prefix("Content-Length: ") {
            let length: usize = length_str.parse().expect("TODO: handle malformed length");

            let mut buf = vec![0; length];
            stdin
                .lock()
                .read_exact(&mut buf)
                .expect("Could not read payload");

            let buf_str = String::from_utf8(buf).unwrap();

            handle_request(
                &buf_str,
                pretty_print_json,
                Arc::clone(&interrupted),
                sender.clone(),
            );
        } else {
            let err_response = Response {
                kind: ResponseKind::MalformedRequest {
                    message: format!(
                        "Invalid request (header missing). A valid request looks like: {}. The request received was:\n\n{}",
                        sample_request_as_json(),
                        line,
                    )
                },
                position: None,
                id: None,
            };

            print_as_json(&err_response, pretty_print_json);
        }
    }
}
