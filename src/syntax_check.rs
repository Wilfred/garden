use serde::Serialize;
use std::path::Path;

use garden_lang_parser::{
    ast::{SourceString, SyntaxIdGenerator},
    diagnostics::ErrorMessage,
    parse_toplevel_items,
    position::Position,
    ParseError,
};

use crate::{
    checks::check_toplevel_items,
    diagnostics::{format_diagnostic, Diagnostic, Level},
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

    let mut id_gen = SyntaxIdGenerator::default();
    let (items, errors) = parse_toplevel_items(path, src, &mut id_gen);

    for e in errors.into_iter() {
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
    }

    for Diagnostic {
        message,
        position,
        level,
    } in check_toplevel_items(&items)
    {
        // TODO: merge Level and Severity types.
        let severity = match level {
            Level::Warning => Severity::Warning,
            Level::Error => Severity::Error,
        };

        diagnostics.push(CheckDiagnostic {
            position: position.clone(),
            line_number: position.line_number + 1,
            message: ErrorMessage(message),
            start_offset: position.start_offset,
            end_offset: position.end_offset,
            severity,
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
            let level = match diagnostic.severity {
                Severity::Error => Level::Error,
                Severity::Warning => Level::Warning,
            };

            format_diagnostic(
                &diagnostic.message,
                &diagnostic.position,
                level,
                &src_string,
            )
        };

        println!("{}", s);
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
