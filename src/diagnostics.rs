//! Error and warning data types, along with logic to display them.

use std::io::IsTerminal as _;
use std::path::PathBuf;

use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::env::StackFrame;
use crate::eval::EnclosingSymbol;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;

#[derive(Debug, Deserialize, Serialize, Clone, Copy)]
pub(crate) enum Level {
    Warning,
    Error,
}

#[derive(Debug)]
pub(crate) struct Diagnostic {
    pub(crate) message: ErrorMessage,
    pub(crate) position: Position,
    pub(crate) level: Level,
    pub(crate) notes: Vec<(ErrorMessage, Position)>,
}

pub(crate) fn format_error_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
    vfs: &Vfs,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let mut res = String::new();

    res.push_str(&format!(
        "{}: {}\n",
        if use_color {
            "Error".bold().red().to_string()
        } else {
            "Error".to_owned()
        },
        message.as_string()
    ));

    // For the topmost (most recently called) stack frame, the
    // relevant position is where the error occurred.
    let top_stack = stack.last().unwrap();
    res.push_str(&format_pos_in_fun(
        position,
        vfs,
        Some(&top_stack.enclosing_name),
        None,
        true,
        true,
    ));

    // For the rest of the stack, we want the positions of calls.
    for (callee_stack_frame, caller_stack_frame) in stack.iter().rev().tuple_windows() {
        if let Some(pos) = &callee_stack_frame.caller_pos {
            res.push('\n');
            res.push_str(&format_pos_in_fun(
                pos,
                vfs,
                Some(&caller_stack_frame.enclosing_name),
                None,
                false,
                true,
            ));
        }
    }

    res
}

pub(crate) fn format_diagnostic(
    message: &ErrorMessage,
    position: &Position,
    project_root: Option<&PathBuf>,
    level: Level,
    vfs: &Vfs,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let level_s = match level {
        Level::Warning => {
            if use_color {
                "Warning".bold().yellow().to_string()
            } else {
                "Warning".to_owned()
            }
        }
        Level::Error => {
            if use_color {
                "Error".bold().red().to_string()
            } else {
                "Error".to_owned()
            }
        }
    };

    let mut res = format!(
        "{}: {}\n",
        level_s,
        if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        }
    );
    res.push_str(&format_pos_in_fun(
        position,
        vfs,
        None,
        project_root,
        true,
        matches!(level, Level::Error),
    ));
    res
}

fn format_pos_in_fun(
    position: &Position,
    vfs: &Vfs,
    enclosing_symbol: Option<&EnclosingSymbol>,
    project_root: Option<&PathBuf>,
    underline: bool,
    is_error: bool,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let mut res = String::new();

    let mut pos_path = position.path.to_path_buf();
    if let Some(root) = project_root {
        if let Ok(rel_path) = pos_path.strip_prefix(root) {
            pos_path = rel_path.to_path_buf();
        }
    }

    let formatted_pos = format!("--> {}:{}", pos_path.display(), position.line_number + 1,);

    if use_color {
        res.push_str(&formatted_pos.dimmed().to_string());
    } else {
        res.push_str(&formatted_pos);
    }

    if let Some(enclosing_sym) = enclosing_symbol {
        let mut signature = format!("\t {}", enclosing_sym);
        if use_color {
            signature = signature.bold().dimmed().to_string();
        }

        res.push_str(&signature);
    }
    res.push('\n');

    let offset = position.start_offset;
    let end_offset = position.end_offset;

    let src = match vfs.file_src(&position.vfs_path) {
        Some(src) => src,
        None => "",
    };

    let s_lines: Vec<_> = src.lines().collect();
    if s_lines.is_empty() {
        // Nothing to do.
    } else if offset >= src.len() || end_offset >= src.len() {
        // TODO: this can occur when the Vfs is stale relative to the
        // last time these functions were re-evaluated.
        let relevant_line = s_lines[0].to_owned();
        res.push_str(&relevant_line);
    } else {
        let line_positions = LinePositions::from(src);

        for (i, span) in line_positions
            .from_region(offset, end_offset)
            .iter()
            .enumerate()
        {
            // .lines() in the Rust stdlib discards the trailing
            // newline, so we end up with s_lines not having the last line.
            let Some(relevant_line) = s_lines.get(span.line.as_usize()) else {
                continue;
            };

            if i != 0 {
                res.push('\n');
            }

            res.push_str(relevant_line);

            if underline {
                res.push('\n');
                res.push_str(&" ".repeat(span.start_col as usize));

                let carets = "^".repeat((span.end_col - span.start_col) as usize);
                if use_color {
                    res.push_str(&if is_error {
                        carets.red().to_string()
                    } else {
                        carets.yellow().to_string()
                    });
                } else {
                    res.push_str(&carets);
                }
            }
        }
    }

    res
}
