use std::{
    io::BufRead,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use serde::{Deserialize, Serialize};

use crate::eval::{Env, EvalError};
use crate::{eval::eval_stmts, parse::parse_toplevel_from_str};

#[derive(Debug, Deserialize, Serialize)]
enum Method {
    Evaluate,
    RunCommand,
}

#[derive(Debug, Deserialize, Serialize)]
struct EvalRequest {
    method: Method,
    input: String,
}

#[derive(Debug, Deserialize, Serialize)]
enum EvalResponse {
    Error { message: String },
    Success { result: String },
}

pub fn json_session(interrupted: &Arc<AtomicBool>) {
    let response = EvalResponse::Success {
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

        let response = match serde_json::from_str::<EvalRequest>(&line) {
            Ok(req) => match parse_toplevel_from_str(&req.input) {
                Ok(stmts) => match eval_stmts(&stmts, &mut env, &complete_src, &interrupted) {
                    Ok(result) => EvalResponse::Success {
                        result: format!("{}", result),
                    },
                    Err(EvalError::Aborted) => EvalResponse::Error {
                        message: format!("Aborted"),
                    },
                    Err(EvalError::UserError(e)) => EvalResponse::Error {
                        message: format!("Error: {}", e),
                    },
                },
                Err(e) => EvalResponse::Error {
                    message: format!("Could not parse input: {:?}", e),
                },
            },
            Err(_) => EvalResponse::Error {
                message: format!(
                    "Could not parse request: {}. A valid request looks like: {}",
                    line,
                    serde_json::to_string(&EvalRequest {
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
