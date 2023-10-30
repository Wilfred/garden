use ariadne::{Label, Report, ReportKind, Source};
use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;

use crate::{
    ast::{Position, SourceString, SymbolName},
    eval::{ErrorMessage, StackFrame},
};

pub fn format_error_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
) -> String {
    let mut res = String::new();

    res.push_str(&format!("Error: {}\n\n", message.0));

    // For the topmost (most recently called) stack frame, the
    // relevant position is where the error occurred.
    let top_stack = stack.last().unwrap();
    res.push_str(&format_pos_in_fun(
        position,
        &top_stack.src,
        &top_stack.enclosing_name,
        true,
    ));

    // For the rest of the stack, we want the positions of calls.
    for (callee_stack_frame, caller_stack_frame) in stack.iter().rev().tuple_windows() {
        if let Some(pos) = &callee_stack_frame.caller_pos {
            res.push('\n');
            res.push_str(&format_pos_in_fun(
                pos,
                &caller_stack_frame.src,
                &caller_stack_frame.enclosing_name,
                false,
            ));
        }
    }

    res
}

pub fn format_error(
    message: &ErrorMessage,
    position: &Position,
    src_string: &SourceString,
) -> String {
    let src = &src_string.src;
    let mut res = Vec::new();

    let path_str = position.path.display().to_string();
    // TODO: roll our own error formatting.
    let r = Report::build(ReportKind::Error, &path_str, position.start_offset)
        .with_label(Label::new((
            &path_str,
            position.start_offset..position.end_offset,
        )))
        .finish();

    r.write((&path_str, Source::from(src)), &mut res).unwrap();
    format!("Error: {}\n{}", message.0, String::from_utf8_lossy(&res))
}

pub fn format_parse_error(
    message: &ErrorMessage,
    position: &Position,
    src_string: &SourceString,
) -> String {
    format_error(message, position, src_string)
}

fn format_pos_in_fun(
    position: &Position,
    src_string: &SourceString,
    name: &SymbolName,
    underline: bool,
) -> String {
    let mut res = String::new();

    res.push_str(
        &format!(
            "--> {}:{}\t{}\n",
            position.path.display(),
            position.line_number + 1,
            name.0.bold(),
        )
        .dimmed()
        .to_string(),
    );

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
