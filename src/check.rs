use serde::Serialize;
use std::path::Path;

use crate::parse::{self, parse_toplevel_items};

#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    line_number: usize,
}

pub fn check(path: &Path, src: &str) {
    let errors = match parse_toplevel_items(path, src) {
        Ok(_) => vec![],
        Err(e) => match e {
            parse::ParseError::Invalid { position, .. } => {
                // Expose line numbers as 1-indexed.
                vec![CheckDiagnostic {
                    line_number: position.line_number + 1,
                }]
            }
            parse::ParseError::Incomplete(_) => {
                // TODO: last line would be better?
                vec![CheckDiagnostic { line_number: 1 }]
            }
        },
    };

    let error_json = serde_json::to_string(&errors).expect("TODO: can this ever fail?");
    println!("{}", error_json);
    if !errors.is_empty() {
        std::process::exit(1);
    }
}
