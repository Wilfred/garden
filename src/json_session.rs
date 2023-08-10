use std::io::Write;
use std::path::PathBuf;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::ast;
use crate::eval::{eval_env, ToplevelEvalResult};
use crate::parse::{format_error, parse_def_or_expr_from_span, ParseError};
use crate::{
    commands::{print_available_commands, run_command, Command, CommandError, CommandParseError},
    eval::{eval_def_or_exprs, Env, EvalError, Session},
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
pub enum ResponseKind {
    Evaluate,
    RunCommand,
    Ready,
    MalformedRequest,
    Printed,
    // TODO: find a nice way to suspend the interpreter.
    InteractivePrompt,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ResponseError {
    position: Option<ast::Position>,
    message: String,
    stack: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Response {
    pub kind: ResponseKind,
    pub value: Result<String, ResponseError>,
}

pub fn sample_request_as_json() -> String {
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

    match parse_def_or_expr_from_span(
        &req.path
            .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
        &req.input,
        req.offset.unwrap_or(0),
        req.end_offset.unwrap_or(req.input.len()),
    ) {
        Ok(exprs) => match eval_def_or_exprs(&exprs, env, session) {
            Ok(result) => {
                let value_summary = match result {
                    ToplevelEvalResult::Value(value) => format!("{}", value),
                    ToplevelEvalResult::Definition(summary) => summary,
                };
                Response {
                    kind: ResponseKind::Evaluate,
                    value: Ok(value_summary),
                }
            }
            Err(EvalError::ResumableError(position, e)) => {
                // TODO: print the whole stack.
                let stack = Some(format_error(&e, &position, &req.input));
                Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message: format!("Error: {}", e),
                        stack,
                    }),
                }
            }
            Err(EvalError::Interrupted) => Response {
                kind: ResponseKind::Evaluate,
                value: Err(ResponseError {
                    position: None,
                    message: "Interrupted".to_string(),
                    stack: None,
                }),
            },
            Err(EvalError::Stop(_)) => {
                todo!();
            }
        },
        Err(e) => match e {
            ParseError::Invalid {
                position,
                message,
                additional: _,
            } => {
                let stack = Some(format_error(&message, &position, &req.input));
                Response {
                    kind: ResponseKind::Evaluate,
                    value: Err(ResponseError {
                        position: Some(position),
                        message,
                        stack,
                    }),
                }
            }
            ParseError::Incomplete(message) => Response {
                kind: ResponseKind::Evaluate,
                value: Err(ResponseError {
                    position: None,
                    message,
                    stack: None,
                }),
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
                    },
                    Err(CommandError::Abort) => Response {
                        kind: ResponseKind::RunCommand,
                        value: Ok("Aborted".to_string()),
                    },
                    Err(CommandError::Resume) => {
                        let stack_frame = env.stack.last_mut().unwrap();
                        if let Some((_, expr)) = stack_frame.exprs_to_eval.pop() {
                            assert!(matches!(expr.1, ast::Expression_::Stop(_)));

                            match eval_env(env, session) {
                                Ok(result) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Ok(format!("{}", result)),
                                },
                                Err(EvalError::ResumableError(position, e)) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Err(ResponseError {
                                        position: Some(position),
                                        message: format!("Error: {}", e),
                                        stack: None,
                                    }),
                                },
                                Err(EvalError::Interrupted) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Err(ResponseError {
                                        position: None,
                                        message: "Interrupted".to_string(),
                                        stack: None,
                                    }),
                                },
                                Err(EvalError::Stop(_)) => {
                                    todo!();
                                }
                            }
                        } else {
                            Response {
                                kind: ResponseKind::Evaluate,
                                value: Ok("(nothing to resume)".into()),
                            }
                        }
                    }
                    Err(CommandError::Replace(_)) => todo!(),
                    Err(CommandError::Skip) => todo!(),
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
                }
            }
            Err(CommandParseError::NotCommandSyntax) => {
                handle_eval_request(req, env, session, complete_src)
            }
        },
    }
}

pub fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready,
        value: Ok("The Garden: Good programs take time to grow.".into()),
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut env = Env::default();
    let mut complete_src = String::new();
    let mut session = Session {
        history: String::new(),
        interrupted,
        has_attached_stdout: false,
    };

    loop {
        let mut line = String::new();
        let stdin = std::io::stdin();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");

        let response = match serde_json::from_str::<Request>(&line) {
            Ok(req) => handle_request(req, &mut env, &mut session, &mut complete_src),
            Err(_) => Response {
                kind: ResponseKind::MalformedRequest,
                value: Err(ResponseError {
                    position: None,
                    message: format!(
                        "Could not parse request: {}. A valid request looks like: {}",
                        line,
                        sample_request_as_json(),
                    ),
                    stack: None,
                }),
            },
        };
        let serialized = serde_json::to_string(&response).unwrap();
        println!("{}", serialized);
    }
}
