use std::{
    io::BufRead,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use serde::{Deserialize, Serialize};

use crate::{
    commands::{run_command, Command, CommandError},
    eval::{eval_def_or_exprs, Env, EvalError, Session},
    parse::parse_def_or_expr_from_str,
};

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
enum Method {
    Evaluate,
    RunCommand,
}

#[derive(Debug, Deserialize, Serialize)]
struct Request {
    method: Method,
    input: String,
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
    pub value: Result<String, String>,
}

pub fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response {
        kind: ResponseKind::Ready,
        value: Ok("ready".into()),
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
        if interrupted.load(Ordering::SeqCst) {
            break;
        }

        let mut line = String::new();
        let stdin = std::io::stdin();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");

        let response = match serde_json::from_str::<Request>(&line) {
            Ok(req) => match req.method {
                Method::Evaluate => {
                    complete_src.push_str(&req.input);
                    match parse_def_or_expr_from_str(&req.input) {
                        Ok(stmts) => match eval_def_or_exprs(&stmts, &mut env, &mut session) {
                            Ok(result) => Response {
                                kind: ResponseKind::Evaluate,
                                value: Ok(format!("{}", result)),
                            },
                            Err(EvalError::ResumableError(e)) => Response {
                                kind: ResponseKind::Evaluate,
                                value: Err(format!("Error: {}", e)),
                            },
                            Err(EvalError::Interrupted) => Response {
                                kind: ResponseKind::Evaluate,
                                value: Err(format!("Interrupted")),
                            },
                            Err(EvalError::Stop(_)) => {
                                todo!();
                            }
                        },
                        Err(e) => Response {
                            kind: ResponseKind::Evaluate,
                            value: Err(format!("Could not parse input: {:?}", e)),
                        },
                    }
                }
                Method::RunCommand => match Command::from_string(&req.input) {
                    Some(command) => {
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
                            Err(CommandError::Resume) => todo!(),
                            Err(CommandError::Replace(_)) => todo!(),
                            Err(CommandError::Skip) => todo!(),
                        }
                    }
                    None => Response {
                        // TODO: report the valid errors
                        kind: ResponseKind::RunCommand,
                        value: Err(format!("No such command {:?}", &req.input)),
                    },
                },
            },
            Err(_) => Response {
                kind: ResponseKind::MalformedRequest,
                value: Err(format!(
                    "Could not parse request: {}. A valid request looks like: {}",
                    line,
                    serde_json::to_string(&Request {
                        method: Method::RunCommand,
                        input: ":help".into(),
                    })
                    .unwrap()
                )),
            },
        };
        let serialized = serde_json::to_string(&response).unwrap();
        println!("{}", serialized);
    }
}
