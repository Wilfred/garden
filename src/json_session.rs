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
    eval::{Env, EvalError},
};
use crate::{eval::eval_stmts, parse::parse_toplevel_from_str};

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
enum Response {
    Error { message: String },
    Success { result: String },
}

pub fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response::Success {
        result: "ready".into(),
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut env = Env::default();
    let mut complete_src = String::new();

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
                    match parse_toplevel_from_str(&req.input) {
                        Ok(stmts) => {
                            match eval_stmts(&stmts, &mut env, &complete_src, &interrupted) {
                                Ok(result) => Response::Success {
                                    result: format!("{}", result),
                                },
                                Err(EvalError::Aborted) => Response::Error {
                                    message: format!("Aborted"),
                                },
                                Err(EvalError::UserError(e)) => Response::Error {
                                    message: format!("Error: {}", e),
                                },
                            }
                        }
                        Err(e) => Response::Error {
                            message: format!("Could not parse input: {:?}", e),
                        },
                    }
                }
                Method::RunCommand => match Command::from_string(&req.input) {
                    Some(command) => {
                        let mut out_buf: Vec<u8> = vec![];
                        match run_command(&mut out_buf, &command, &mut env, &complete_src) {
                            Ok(()) => Response::Success {
                                result: format!("{}", String::from_utf8_lossy(&out_buf)),
                            },
                            Err(CommandError::Abort) => Response::Success {
                                result: format!("Aborted"),
                            },
                        }
                    }
                    None => Response::Error {
                        // TODO: report the valid errors
                        message: format!("No such command {:?}", &req.input),
                    },
                },
            },
            Err(_) => Response::Error {
                message: format!(
                    "Could not parse request: {}. A valid request looks like: {}",
                    line,
                    serde_json::to_string(&Request {
                        method: Method::RunCommand,
                        input: ":help".into(),
                    })
                    .unwrap()
                ),
            },
        };
        let serialized = serde_json::to_string(&response).unwrap();
        println!("{}", serialized);
    }
}
