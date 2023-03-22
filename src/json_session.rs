use std::io::BufRead;

use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
struct EvalRequest {
    input: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct Response {
    error: bool,
    message: String,
}

pub fn json_session() {
    let response = Response {
        error: false,
        message: "ready".into(),
    };
    let serialized = serde_json::to_string(&response).unwrap();
    println!("{}", serialized);

    let mut line = String::new();
    let stdin = std::io::stdin();
    stdin
        .lock()
        .read_line(&mut line)
        .expect("Could not read line");

    match serde_json::from_str::<EvalRequest>(&line) {
        Ok(req) => println!("{:?}", req),
        Err(_) => {
            let response = Response {
                error: true,
                message: format!("Could not parse request: {}", line),
            };
            let serialized = serde_json::to_string(&response).unwrap();
            println!("{}", serialized);
        }
    };
}
