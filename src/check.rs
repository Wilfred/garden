use serde::Serialize;
use std::path::Path;

use crate::parse::{self, parse_toplevel_items};

#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    line_number: usize,
    message: String,
    start_offset: usize,
    end_offset: usize,
}

pub fn check(path: &Path, src: &str) {
    let errors = match parse_toplevel_items(path, src) {
        Ok(_) => vec![],
        Err(e) => match e {
            parse::ParseError::Invalid {
                position, message, ..
            } => {
                // Expose line numbers as 1-indexed.
                vec![CheckDiagnostic {
                    line_number: position.line_number + 1,
                    message: message.0,
                    start_offset: position.start_offset,
                    end_offset: position.end_offset,
                }]
            }
            parse::ParseError::Incomplete {
                message, position, ..
            } => {
                // TODO: last line would be better?
                vec![CheckDiagnostic {
                    line_number: 1,
                    message: message.0,
                    start_offset: position.start_offset,
                    end_offset: position.end_offset,
                }]
            }
        },
    };

    for error in &errors {
        println!(
            "{}",
            serde_json::to_string(error).expect("TODO: can this ever fail?")
        );
    }

    if !errors.is_empty() {
        std::process::exit(1);
    }
}
