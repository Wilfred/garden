use serde::Serialize;
use std::path::Path;

use garden_lang_parser::{
    ast::{IdGenerator, SourceString},
    diagnostics::ErrorMessage,
    parse_toplevel_items,
    position::Position,
    ParseError,
};

use crate::{
    checks::check_toplevel_items,
    diagnostics::{format_diagnostic, Diagnostic, Level},
    Env,
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
    // TODO: Could we just use Position here? The only downside of
    // using Position directly is that a file with a large number of
    // errors will repeat the same file name many times in the JSON
    // output.
    /// 1-indexed.
    line_number: usize,
    end_line_number: usize,
    column: usize,
    end_column: usize,
    message: String,
    severity: Severity,
}

pub(crate) fn check(path: &Path, src: &str, json: bool) {
    let mut diagnostics = vec![];

    let mut id_gen = IdGenerator::default();
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
                    end_line_number: position.end_line_number + 1,
                    column: position.column,
                    end_column: position.end_column,
                    message: message.as_string(),
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
                    end_line_number: position.end_line_number + 1,
                    column: position.column,
                    end_column: position.end_column,
                    message: message.as_string(),
                    severity: Severity::Error,
                });
            }
        };
    }

    let env = Env::new(id_gen);
    for Diagnostic {
        message,
        position,
        level,
    } in check_toplevel_items(&items, &env)
    {
        // TODO: merge Level and Severity types.
        let severity = match level {
            Level::Warning => Severity::Warning,
            Level::Error => Severity::Error,
        };

        diagnostics.push(CheckDiagnostic {
            position: position.clone(),
            line_number: position.line_number + 1,
            end_line_number: position.end_line_number + 1,
            column: position.column,
            end_column: position.end_column,
            message,
            severity,
        });
    }

    let src_string = SourceString {
        src: src.to_owned(),
        offset: 0,
    };

    for (i, diagnostic) in diagnostics.iter().enumerate() {
        let s = if json {
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        } else {
            let level = match diagnostic.severity {
                Severity::Error => Level::Error,
                Severity::Warning => Level::Warning,
            };

            format_diagnostic(
                &ErrorMessage(vec![diagnostic.message.clone()]),
                &diagnostic.position,
                level,
                &src_string,
            )
        };

        println!("{}{}", if i == 0 { "" } else { "\n" }, s);
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
