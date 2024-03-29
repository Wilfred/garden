use serde::Serialize;
use std::path::Path;

use garden_lang_parser::{
    ast::{SourceString, ToplevelItem},
    diagnostics::ErrorMessage,
    parse_toplevel_items,
    position::Position,
    ParseError,
};

use crate::{
    checks::check_defs,
    diagnostics::{format_parse_error, Warning},
};

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    #[serde(skip)]
    position: Position,
    line_number: usize,
    message: ErrorMessage,
    start_offset: usize,
    end_offset: usize,
    severity: Severity,
}

pub(crate) fn check(path: &Path, src: &str, json: bool) {
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
                        position: position.clone(),
                        line_number: position.line_number + 1,
                        message,
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
                        position: position.clone(),
                        line_number: 1,
                        message,
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
            position: position.clone(),
            line_number: position.line_number + 1,
            message: ErrorMessage(message),
            start_offset: position.start_offset,
            end_offset: position.end_offset,
            severity: Severity::Warning,
        });
    }

    let src_string = SourceString {
        src: src.to_owned(),
        offset: 0,
    };
    for diagnostic in &diagnostics {
        let s = if json {
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        } else {
            format_parse_error(&diagnostic.message, &diagnostic.position, &src_string)
        };

        println!("{}", s);
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
