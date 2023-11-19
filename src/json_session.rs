use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Instant;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::ast::{self, SourceString};
use crate::diagnostics::{format_error, format_parse_error, Warning};
use crate::env::Env;
use crate::eval::{eval_env, eval_all_toplevel_items, push_test_stackframe};
use crate::parse::{parse_toplevel_items_from_span, ParseError};
use crate::{
    commands::{print_available_commands, run_command, Command, CommandParseError, EvalAction},
    eval::{EvalError, Session},
};

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
enum Method {
    Run,
}

#[derive(Debug, Deserialize, Serialize)]
struct Request {
    method: Method,
    input: String,
    path: Option<PathBuf>,
    offset: Option<usize>,
    end_offset: Option<usize>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ResponseKind {
    Evaluate,
    RunCommand,
    Ready,
    MalformedRequest,
    Printed,
    // TODO: find a nice way to suspend the interpreter.
    InteractivePrompt,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct ResponseError {
    position: Option<ast::Position>,
    message: String,
    stack: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct Response {
    pub(crate) kind: ResponseKind,
    pub(crate) value: Result<String, ResponseError>,
    pub(crate) warnings: Vec<Warning>,
}

pub(crate) fn sample_request_as_json() -> String {
    serde_json::to_string(&Request {
        method: Method::Run,
        input: "1 + 2".into(),
        path: Some(PathBuf::from("/foo/bar.gdn")),
        offset: Some(100),
        end_offset: Some(110),
    })
    .unwrap()
}

fn handle_eval_request(
    req: Request,
    env: &mut Env,
    session: &mut Session,
    complete_src: &mut String,
) -> Response {
    complete_src.push_str(&req.input);

    match parse_toplevel_items_from_span(
        &req.path
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        &req.input,
        req.offset.unwrap_or(0),
        req.end_offset.unwrap_or(req.input.len()),
    ) {
        Ok(items) => match eval_all_toplevel_items(&items, env, session) {
            Ok(eval_summary) => {
                let definition_summary = if eval_summary.new_syms.len() == 1 {
                    format!("Loaded {}", eval_summary.new_syms[0].0)
                } else {
                    format!("Loaded {} definitions", eval_summary.new_syms.len())
                };

                let value_summary = if let Some(last_value) = eval_summary.values.last() {
                    if eval_summary.new_syms.is_empty() {
                        last_value.display(env)
                    } else {
                        format!(
                            "{}, and evaluated {}",
                            definition_summary,
                            last_value.display(env)
                        )
                    }
                } else {
                    format!("{}.", definition_summary)
                };

                Response {
                    kind: ResponseKind::Evaluate,
                    value: Ok(value_summary),
                    warnings: eval_summary.warnings,
                }
            }
            Err(EvalError::ResumableError(position, e)) => {
                // TODO: print the whole stack.
                // TODO: use the original SourceString rather than reconstructing.
                let stack = Some(format_error(
                    &e,
                    &position,
                    &SourceString {
                        src: req.input,
                        offset: 0,
                    },
                ));
                Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message: format!("Error: {}", e.0),
                        stack,
                    }),
                    warnings: vec![],
                }
            }
            Err(EvalError::Interrupted) => Response {
                kind: ResponseKind::Evaluate,
                value: Err(ResponseError {
                    position: None,
                    message: "Interrupted".to_string(),
                    stack: None,
                }),
                warnings: vec![],
            },
        },
        Err(e) => match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_parse_error(
                    &message,
                    &position,
                    &SourceString {
                        src: req.input,
                        offset: 0,
                    },
                ));
                Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message: message.0,
                        stack,
                    }),
                    warnings: vec![],
                }
            }
            ParseError::Incomplete { message, .. } => Response {
                kind: ResponseKind::Evaluate,
                value: Err(ResponseError {
                    position: None,
                    message: message.0,
                    stack: None,
                }),
                warnings: vec![],
            },
        },
    }
}

fn handle_request(
    req: Request,
    env: &mut Env,
    session: &mut Session,
    complete_src: &mut String,
) -> Response {
    match req.method {
        Method::Run => match Command::from_string(&req.input) {
            Ok(command) => {
                let mut out_buf: Vec<u8> = vec![];
                match run_command(&mut out_buf, &command, env, session) {
                    Ok(()) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok(format!("{}", String::from_utf8_lossy(&out_buf))),
                        warnings: vec![],
                    },
                    Err(EvalAction::Abort) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok("Aborted".to_string()),
                        warnings: vec![],
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
                                kind: ResponseKind::MalformedRequest,
                                value: Err(ResponseError {
                                    position: None,
                                    message: format!("No such test: {}", name),
                                    stack: None,
                                }),
                                warnings: vec![],
                            },
                        }
                    }
                    Err(EvalAction::Replace(expr)) => {
                        let stack_frame = env.stack.last_mut().unwrap();

                        stack_frame.evalled_values.pop();
                        stack_frame.exprs_to_eval.push((false, expr));

                        eval_to_response(env, session)
                    }
                    Err(EvalAction::Skip) => {
                        let stack_frame = env.stack.last_mut().unwrap();

                        stack_frame
                            .exprs_to_eval
                            .pop()
                            .expect("Tried to skip an expression, but none in this frame.");

                        eval_to_response(env, session)
                    }
                }
            }
            Err(CommandParseError::NoSuchCommand) => {
                let mut out_buf: Vec<u8> = vec![];
                write!(&mut out_buf, "No such command. ").unwrap();
                print_available_commands(&mut out_buf);

                Response {
                    kind: ResponseKind::RunCommand,
                    value: Err(ResponseError {
                        position: None,
                        message: format!("{}", String::from_utf8_lossy(&out_buf)),
                        stack: None,
                    }),
                    warnings: vec![],
                }
            }
            Err(CommandParseError::NotCommandSyntax) => {
                handle_eval_request(req, env, session, complete_src)
            }
        },
    }
}

fn eval_to_response(env: &mut Env, session: &mut Session<'_>) -> Response {
    match eval_env(env, session) {
        Ok(result) => Response {
            kind: ResponseKind::Evaluate,
            value: Ok(result.display(env)),
            warnings: vec![],
        },
        Err(EvalError::ResumableError(position, e)) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: Some(position),
                message: format!("Error: {}", e.0),
                stack: None,
            }),
            warnings: vec![],
        },
        Err(EvalError::Interrupted) => Response {
            kind: ResponseKind::Evaluate,
            value: Err(ResponseError {
                position: None,
                message: "Interrupted".to_string(),
                stack: None,
            }),
            warnings: vec![],
        },
    }
}

pub(crate) fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready,
        value: Ok("The Garden: Good programs take time to grow.".into()),
        warnings: vec![],
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut env = Env::default();
    let mut complete_src = String::new();
    let mut session = Session {
        history: String::new(),
        interrupted,
        has_attached_stdout: false,
        start_time: Instant::now(),
        trace_exprs: false,
    };

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

            let response = match serde_json::from_str::<Request>(&buf_str) {
                Ok(req) => handle_request(req, &mut env, &mut session, &mut complete_src),
                Err(_) => Response {
                    kind: ResponseKind::MalformedRequest,
                    value: Err(ResponseError {
                        position: None,
                        message: format!(
                            "Invalid request (JSON decode failed). A valid request looks like: {}. The request received was:\n\n{}",
                            sample_request_as_json(),
                            buf_str,
                        ),
                        stack: None,
                    }),warnings: vec![],
                },
            };
            let serialized = serde_json::to_string(&response).unwrap();
            println!("{}", serialized);
        } else {
            let err_response = Response {
                kind: ResponseKind::MalformedRequest,
                value: Err(ResponseError {
                    position: None,
                    message: format!(
                        "Invalid request (header missing). A valid request looks like: {}. The request received was:\n\n{}",
                        sample_request_as_json(),
                        line,
                    ),
                    stack: None,
                }),
                warnings: vec![],
            };
            let serialized = serde_json::to_string(&err_response).unwrap();
            println!("{}", serialized);
        }
    }
}
