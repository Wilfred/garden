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
    let src_string = top_stack
        .enclosing_fun
        .as_ref()
        .map(|fi| fi.src_string.clone());
    res.push_str(&format_pos_in_fun(
        position,
        src_string.as_ref(),
        &top_stack.enclosing_name,
    ));

    // For the rest of the stack, we want the positions of calls.
    for (callee_stack_frame, caller_stack_frame) in stack.iter().rev().tuple_windows() {
        if let Some(pos) = &callee_stack_frame.caller_pos {
            res.push('\n');
            res.push_str(&format_pos_in_fun(
                pos,
                Some(&caller_stack_frame.src),
                &caller_stack_frame.enclosing_name,
            ));
        }
    }

    res
}

pub fn format_error(message: &ErrorMessage, position: &Position, src: &str) -> String {
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
    format!(
        "Error: {}\n{}",
        message.0,
        String::from_utf8_lossy(&res)
    )
}

pub fn format_parse_error(message: &ErrorMessage, position: &Position, src: &str) -> String {
    format_error(message, position, src)
}

fn format_pos_in_fun(
    position: &Position,
    src_string: Option<&SourceString>,
    name: &SymbolName,
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

    let relevant_line = match src_string {
        Some(src_string) => {
            let src = &src_string.src;
            let line_positions = LinePositions::from(src.as_str());

            // TODO: this is the line number relative to the start of
            // the SourceString, not the start of the file.
            let offset = std::cmp::max(
                position.start_offset as isize - src_string.offset as isize,
                0,
            ) as usize;
            let line_num = if offset >= src.len() {
                // TODO: this occurs when we are using the wrong
                // SourceString, such as the main function wrapper. We
                // should find the relevant SourceString instead.
                0.into()
            } else {
                line_positions.from_offset(offset)
            };

            let s_lines: Vec<_> = src.lines().collect();
            s_lines[line_num.as_usize()].to_owned()
        }
        None => "??? no source found (FunInfo is None) ???".to_owned(),
    };

    res.push_str(&relevant_line);
    res
}
