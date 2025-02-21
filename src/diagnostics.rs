//! Error and warning data types, along with logic to display them.

use std::io::IsTerminal as _;

use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::env::StackFrame;
use crate::eval::EnclosingSymbol;
use garden_lang_parser::position::Position;
use garden_lang_parser::{ast::SourceString, diagnostics::ErrorMessage};

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
}

pub(crate) fn format_error_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
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
        &top_stack.src,
        Some(&top_stack.enclosing_name),
        true,
        true,
    ));

    // For the rest of the stack, we want the positions of calls.
    for (callee_stack_frame, caller_stack_frame) in stack.iter().rev().tuple_windows() {
        if let Some(pos) = &callee_stack_frame.caller_pos {
            res.push('\n');
            res.push_str(&format_pos_in_fun(
                pos,
                &caller_stack_frame.src,
                Some(&caller_stack_frame.enclosing_name),
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
    level: Level,
    src_string: &SourceString,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let level_s = match level {
        Level::Warning => "Warning",
        Level::Error => "Error",
    };

    let mut res = format!(
        "{}: {}\n",
        if use_color {
            level_s.bold().to_string()
        } else {
            level_s.to_owned()
        },
        if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        }
    );
    res.push_str(&format_pos_in_fun(
        position,
        src_string,
        None,
        true,
        matches!(level, Level::Error),
    ));
    res
}

fn format_pos_in_fun(
    position: &Position,
    src_string: &SourceString,
    enclosing_symbol: Option<&EnclosingSymbol>,
    underline: bool,
    is_error: bool,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let mut res = String::new();

    let formatted_pos = format!(
        "--> {}:{}",
        position.path.display(),
        position.line_number + 1,
    );

    if use_color {
        res.push_str(&formatted_pos.dimmed().to_string());
    } else {
        res.push_str(&formatted_pos);
    }

    if let Some(enclosing_sym) = enclosing_symbol {
        let signature = format!("\t {}", enclosing_sym);
        res.push_str(&signature.bold().dimmed().to_string());
    }
    res.push('\n');

    // TODO: this is the line number relative to the start of
    // the SourceString, not the start of the file.
    let offset = std::cmp::max(
        position.start_offset as isize - src_string.offset as isize,
        0,
    ) as usize;
    let end_offset =
        std::cmp::max(position.end_offset as isize - src_string.offset as isize, 0) as usize;

    let src = &src_string.src;
    let s_lines: Vec<_> = src.lines().collect();
    if offset >= src.len() {
        // TODO: this occurs when we are using the wrong
        // SourceString, such as the main function wrapper. We
        // should find the relevant SourceString instead.
        let relevant_line = s_lines[0].to_owned();
        res.push_str(&relevant_line);
    } else {
        let line_positions = LinePositions::from(src.as_str());

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
