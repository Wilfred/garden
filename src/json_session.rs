use std::io::Write;
use std::path::PathBuf;
use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::eval::eval_env;
use crate::parse::{Expression_, Position};
use crate::{
    commands::{print_available_commands, run_command, Command, CommandError, CommandParseError},
    eval::{eval_def_or_exprs, Env, EvalError, Session},
    parse::parse_def_or_expr_from_str,
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
pub struct Response {
    pub kind: ResponseKind,
    pub value: Result<String, (Option<Position>, String)>,
}

pub fn sample_request_as_json() -> String {
    serde_json::to_string(&Request {
        method: Method::Run,
        input: "1 + 2".into(),
        path: Some(PathBuf::from("/foo/bar.gdn")),
    })
    .unwrap()
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
            Ok(req) => match req.method {
                Method::Run => match Command::from_string(&req.input) {
                    Ok(command) => {
                        let mut out_buf: Vec<u8> = vec![];
                        match run_command(&mut out_buf, &command, &mut env, &session) {
                            Ok(()) => Response {
                                kind: ResponseKind::RunCommand,
                                value: Ok(format!("{}", String::from_utf8_lossy(&out_buf))),
                            },
                            Err(CommandError::Abort) => Response {
                                kind: ResponseKind::RunCommand,
                                value: Ok(format!("Aborted")),
                            },
                            Err(CommandError::Resume) => {
                                let stack_frame = env.stack.last_mut().unwrap();
                                if let Some((_, expr)) = stack_frame.exprs_to_eval.pop() {
                                    assert!(matches!(expr.1, Expression_::Stop(_)));

                                    match eval_env(&mut env, &mut session) {
                                        Ok(result) => Response {
                                            kind: ResponseKind::Evaluate,
                                            value: Ok(format!("{}", result)),
                                        },
                                        Err(EvalError::ResumableError(position, e)) => Response {
                                            kind: ResponseKind::Evaluate,
                                            value: Err((Some(position), (format!("Error: {}", e)))),
                                        },
                                        Err(EvalError::Interrupted) => Response {
                                            kind: ResponseKind::Evaluate,
                                            value: Err((None, format!("Interrupted"))),
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
                            value: Err((None, format!("{}", String::from_utf8_lossy(&out_buf)))),
                        }
                    }
                    Err(CommandParseError::NotCommandSyntax) => {
                        complete_src.push_str(&req.input);
                        // TODO: JSON requests should pass the path.
                        match parse_def_or_expr_from_str(
                            &req.path
                                .unwrap_or_else(|| PathBuf::from("__json_session_unnamed__")),
                            &req.input,
                        ) {
                            Ok(exprs) => match eval_def_or_exprs(&exprs, &mut env, &mut session) {
                                Ok(result) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Ok(format!("{}", result)),
                                },
                                Err(EvalError::ResumableError(position, e)) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Err((Some(position), format!("Error: {}", e))),
                                },
                                Err(EvalError::Interrupted) => Response {
                                    kind: ResponseKind::Evaluate,
                                    value: Err((None, format!("Interrupted"))),
                                },
                                Err(EvalError::Stop(_)) => {
                                    todo!();
                                }
                            },
                            Err(e) => Response {
                                kind: ResponseKind::Evaluate,
                                value: Err((None, format!("Could not parse input: {:?}", e))),
                            },
                        }
                    }
                },
            },
            Err(_) => Response {
                kind: ResponseKind::MalformedRequest,
                value: Err((
                    None,
                    format!(
                        "Could not parse request: {}. A valid request looks like: {}",
                        line,
                        sample_request_as_json(),
                    ),
                )),
            },
        };
        let serialized = serde_json::to_string(&response).unwrap();
        println!("{}", serialized);
    }
}
