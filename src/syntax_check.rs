use serde::Serialize;
use std::path::Path;

use garden_lang_parser::{ast::ToplevelItem, parse_toplevel_items, ParseError};

use crate::{checks::check_defs, diagnostics::Warning};

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    line_number: usize,
    message: String,
    start_offset: usize,
    end_offset: usize,
    severity: Severity,
}

pub(crate) fn check(path: &Path, src: &str) {
    let mut diagnostics = vec![];

    let items = match parse_toplevel_items(path, src) {
        Ok(items) => items,
        Err(e) => {
            match e {
                ParseError::Invalid {
                    position, message, ..
                } => {
                    // Expose line numbers as 1-indexed.
                    diagnostics.push(CheckDiagnostic {
                        line_number: position.line_number + 1,
                        message: message.0,
                        start_offset: position.start_offset,
                        end_offset: position.end_offset,
                        severity: Severity::Error,
                    });
                }
                ParseError::Incomplete {
                    message, position, ..
                } => {
                    // TODO: last line would be better?
                    diagnostics.push(CheckDiagnostic {
                        line_number: 1,
                        message: message.0,
                        start_offset: position.start_offset,
                        end_offset: position.end_offset,
                        severity: Severity::Error,
                    });
                }
            };
            vec![]
        }
    };

    let mut defs = vec![];
    for item in items {
        match item {
            ToplevelItem::Def(def) => {
                defs.push(def);
            }
            ToplevelItem::Expr(_) => {}
        }
    }

    for Warning { message, position } in check_defs(&defs) {
        diagnostics.push(CheckDiagnostic {
            line_number: position.line_number + 1,
            message,
            start_offset: position.start_offset,
            end_offset: position.end_offset,
            severity: Severity::Warning,
        });
    }

    for diagnostic in &diagnostics {
        println!(
            "{}",
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        );
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
