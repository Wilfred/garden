//! Check Garden code blocks in markdown files for syntax and
//! semantic errors, equivalent to calling `reflect::check_snippet`
//! on each block.

use std::io::IsTerminal;
use std::path::Path;

use crate::checks::check_toplevel_items;
use crate::diagnostics::{format_diagnostic, Severity};
use crate::env::Env;
use crate::parser::ast::IdGenerator;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::vfs::Vfs;
use crate::parser::{parse_toplevel_items_from_span, ParseError};
use crate::run_code_blocks::extract_code_blocks;

pub(crate) fn check_snippets(path: &Path, src: &str) {
    let use_color = std::io::stdout().is_terminal();

    let blocks = extract_code_blocks(src);

    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let env = Env::new(id_gen.clone(), vfs);

    let mut had_error = false;
    let mut first = true;

    for block in &blocks {
        let (items, parse_errors) = parse_toplevel_items_from_span(
            &vfs_path,
            src,
            &mut id_gen,
            block.start_offset,
            block.end_offset,
        );

        for err in parse_errors {
            had_error = true;
            let (message, position) = match err {
                ParseError::Invalid {
                    message, position, ..
                } => (message, position),
                ParseError::Incomplete { message, position } => (message, position),
            };

            let message_str = if use_color {
                message.as_styled_string()
            } else {
                message.as_string()
            };
            let formatted = format_diagnostic(
                &ErrorMessage(vec![Text(message_str)]),
                &position,
                &env.project_root,
                Severity::Error,
                &[],
                &env.vfs,
            );
            println!("{}{}", if first { "" } else { "\n" }, formatted);
            first = false;
        }

        for diagnostic in check_toplevel_items(&vfs_path, &items, &env) {
            had_error = true;

            let message_str = if use_color {
                diagnostic.message.as_styled_string()
            } else {
                diagnostic.message.as_string()
            };
            let formatted = format_diagnostic(
                &ErrorMessage(vec![Text(message_str)]),
                &diagnostic.position,
                &env.project_root,
                diagnostic.severity,
                &diagnostic.notes,
                &env.vfs,
            );
            println!("{}{}", if first { "" } else { "\n" }, formatted);
            first = false;
        }
    }

    if had_error {
        std::process::exit(1);
    }
}
