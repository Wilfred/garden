//! Error and warning data types, along with logic to display them.

use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::eval::StackFrame;
use garden_lang_parser::{
    ast::{Position, SourceString, SymbolName},
    diagnostics::ErrorMessage,
};

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct Warning {
    pub(crate) message: String,
}

pub(crate) fn format_error_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
) -> String {
    let mut res = String::new();

    res.push_str(&format!("{}: {}\n\n", "Error".bold().red(), message.0));

    // For the topmost (most recently called) stack frame, the
    // relevant position is where the error occurred.
    let top_stack = stack.last().unwrap();
    res.push_str(&format_pos_in_fun(
        position,
        &top_stack.src,
        Some(&top_stack.enclosing_name),
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
            ));
        }
    }

    res
}

pub(crate) fn format_error(
    message: &ErrorMessage,
    position: &Position,
    src_string: &SourceString,
) -> String {
    let mut res = format!("Error: {}\n\n", message.0);
    res.push_str(&format_pos_in_fun(position, src_string, None, true));
    res
}

pub(crate) fn format_parse_error(
    message: &ErrorMessage,
    position: &Position,
    src_string: &SourceString,
) -> String {
    format_error(message, position, src_string)
}

fn format_pos_in_fun(
    position: &Position,
    src_string: &SourceString,
    name: Option<&SymbolName>,
    underline: bool,
) -> String {
    let mut res = String::new();

    res.push_str(
        &format!(
            "--> {}:{}",
            position.path.display(),
            position.line_number + 1,
        )
        .dimmed()
        .to_string(),
    );

    if let Some(name) = name {
        res.push_str(&format!("\t{}", name.0.bold()).dimmed().to_string());
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
        for span in line_positions.from_offsets(offset, end_offset) {
            let relevant_line = s_lines[span.line.as_usize()].to_owned();
            res.push_str(&relevant_line);

            if underline {
                res.push('\n');
                res.push_str(&" ".repeat(span.start_col as usize));
                res.push_str(
                    &"^".repeat((span.end_col - span.start_col) as usize)
                        .red()
                        .to_string(),
                );
            }
        }
    }

    res
}
