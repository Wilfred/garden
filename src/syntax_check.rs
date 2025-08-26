use serde::Serialize;
use std::io::IsTerminal;
use std::path::Path;

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::{format_diagnostic, Diagnostic, Severity};
use crate::parser::ast::IdGenerator;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::parser::{parse_toplevel_items, ParseError};
use crate::{load_toplevel_items, Env};

/// A diagnostic that can be serialized. Messages and positions are
/// stored in an IDE friendly format.
///
/// See also `Diagnostic`.
#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    #[serde(skip)]
    position: Position,
    #[serde(skip)]
    notes: Vec<(ErrorMessage, Position)>,
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
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    for e in errors.into_iter() {
        match e {
            ParseError::Invalid {
                position,
                message,
                additional,
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
                    notes: additional,
                });
            }
            ParseError::Incomplete {
                message, position, ..
            } => {
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
                    notes: vec![],
                });
            }
        };
    }

    let mut env = Env::new(id_gen, vfs);

    // If we have syntax errors, don't bother checking anything else,
    // as it has a high false positive rate and it's hard to see what
    // needs fixing next.
    if diagnostics.is_empty() {
        let ns = env.get_or_create_namespace(path);
        let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, ns.clone());
        raw_diagnostics.extend(check_toplevel_items_in_env(&vfs_path, &items, &env, ns));

        for Diagnostic {
            message,
            position,
            severity,
            notes,
        } in raw_diagnostics
        {
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
                notes,
            });
        }
    }

    for (i, diagnostic) in diagnostics.iter().enumerate() {
        let s = if json {
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        } else {
            format_diagnostic(
                &ErrorMessage(vec![Text(diagnostic.message.clone())]),
                &diagnostic.position,
                &env.project_root,
                diagnostic.severity,
                &diagnostic.notes,
                &env.vfs,
            )
        };

        println!("{}{}", if i == 0 { "" } else { "\n" }, s);
    }

    if !diagnostics.is_empty() {
        std::process::exit(1);
    }
}
