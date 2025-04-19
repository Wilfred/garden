use serde::Serialize;
use std::io::IsTerminal;
use std::path::Path;

use crate::checks::check_toplevel_items_in_env;
use crate::load_toplevel_items;
use crate::parser::ast::Vfs;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::{
    ast::IdGenerator, diagnostics::ErrorMessage, parse_toplevel_items, position::Position,
    ParseError,
};
use crate::{
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
    let use_color = std::io::stdout().is_terminal() && !json;

    let mut diagnostics = vec![];

    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

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
                    message: if use_color {
                        message.as_styled_string()
                    } else {
                        message.as_string()
                    },
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
                    message: if use_color {
                        message.as_styled_string()
                    } else {
                        message.as_string()
                    },
                    severity: Severity::Error,
                });
            }
        };
    }

    let mut env = Env::new(id_gen, vfs);
    let ns = env.current_namespace();
    let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, Some(ns));
    raw_diagnostics.extend(check_toplevel_items_in_env(&items, &env));

    for Diagnostic {
        message,
        position,
        level,
    } in raw_diagnostics
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
            message: if use_color {
                message.as_styled_string()
            } else {
                message.as_string()
            },
            severity,
        });
    }

    for (i, diagnostic) in diagnostics.iter().enumerate() {
        let s = if json {
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        } else {
            let level = match diagnostic.severity {
                Severity::Error => Level::Error,
                Severity::Warning => Level::Warning,
            };

            format_diagnostic(
                &ErrorMessage(vec![Text(diagnostic.message.clone())]),
                &diagnostic.position,
                level,
                &env.vfs,
            )
        };

        println!("{}{}", if i == 0 { "" } else { "\n" }, s);
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
