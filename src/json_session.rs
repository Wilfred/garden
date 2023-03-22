use std::{
    io::BufRead,
    sync::{atomic::AtomicBool, Arc},
};

use serde::{Deserialize, Serialize};

use crate::eval::{Env, EvalError};
use crate::{eval::eval_stmts, parse::parse_toplevel_from_str};

#[derive(Debug, Deserialize, Serialize)]
struct EvalRequest {
    input: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Response {
    error: bool,
    message: String,
}

pub fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = Response {
        error: false,
        message: "ready".into(),
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut env = Env::default();
    let mut complete_src = String::new();

    let mut line = String::new();
    let stdin = std::io::stdin();
    stdin
        .lock()
        .read_line(&mut line)
        .expect("Could not read line");

    let response = match serde_json::from_str::<EvalRequest>(&line) {
        Ok(req) => match parse_toplevel_from_str(&req.input) {
            Ok(stmts) => {
                let e = 1;
                match eval_stmts(&stmts, &mut env, &complete_src, &interrupted) {
                    Ok(result) => Response {
                        error: false,
                        message: format!("result: {}", result),
                    },
                    Err(EvalError::Aborted) => Response {
                        error: true,
                        message: format!("Aborted"),
                    },
                    Err(EvalError::UserError(e)) => Response {
                        error: true,
                        message: format!("Error: {}", e),
                    },
                }
            }
            Err(e) => Response {
                error: true,
                message: format!("Could not parse input: {:?}", e),
            },
        },
        Err(_) => Response {
            error: true,
            message: format!("Could not parse request: {}", line),
        },
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);
}
