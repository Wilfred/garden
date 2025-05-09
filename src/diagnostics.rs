//! Error and warning data types, along with logic to display them.

use std::io::IsTerminal as _;
use std::path::PathBuf;
use std::rc::Rc;

use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::env::StackFrame;
use crate::eval::EnclosingSymbol;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::lex::lex;
use crate::parser::position::Position;
use crate::parser::vfs::{Vfs, VfsId};
use crate::VfsPathBuf;

#[derive(Debug, Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Severity {
    Warning,
    Error,
}

#[derive(Debug)]
pub(crate) struct Diagnostic {
    pub(crate) message: ErrorMessage,
    pub(crate) position: Position,
    pub(crate) severity: Severity,
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
        None,
        true,
        0,
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
                None,
                true,
                0,
            ));
        }
    }

    res
}

pub(crate) fn format_diagnostic(
    message: &ErrorMessage,
    position: &Position,
    project_root: Option<&PathBuf>,
    severity: Severity,
    notes: &[(ErrorMessage, Position)],
    vfs: &Vfs,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let severity_s = format_severity(severity, use_color);

    let mut s = format!(
        "{}: {}\n",
        severity_s,
        if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        }
    );
    s.push_str(&format_pos_in_fun(
        position,
        vfs,
        None,
        project_root,
        true,
        None,
        matches!(severity, Severity::Error),
        2,
    ));

    for (message, position) in notes {
        s.push('\n');

        s.push_str(&format_pos_in_fun(
            position,
            vfs,
            None,
            project_root,
            true,
            Some(if use_color {
                message.as_styled_string()
            } else {
                message.as_string()
            }),
            false,
            2,
        ));
    }

    s
}

fn format_severity(severity: Severity, use_color: bool) -> String {
    match severity {
        Severity::Warning => {
            if use_color {
                "Warning".bold().yellow().to_string()
            } else {
                "Warning".to_owned()
            }
        }
        Severity::Error => {
            if use_color {
                "Error".bold().red().to_string()
            } else {
                "Error".to_owned()
            }
        }
    }
}

fn format_note_severity(use_color: bool) -> String {
    if use_color {
        "Note".dimmed().to_string()
    } else {
        "Note".to_owned()
    }
}

fn format_pos_in_fun(
    position: &Position,
    vfs: &Vfs,
    enclosing_symbol: Option<&EnclosingSymbol>,
    project_root: Option<&PathBuf>,
    underline: bool,
    underline_msg: Option<String>,
    is_error: bool,
    context_lines: usize,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let mut res = String::new();

    let mut pos_path = position.path.to_path_buf();
    if let Some(root) = project_root {
        if let Ok(rel_path) = pos_path.strip_prefix(root) {
            pos_path = rel_path.to_path_buf();
        }
    }

    let margin_width = 4;

    let formatted_pos = format!(
        "{}| {}:{}:{}",
        "-".repeat(margin_width - 1),
        pos_path.display(),
        position.line_number + 1,
        position.column + 1
    );

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

        res.push_str(&format_margin_num("?", margin_width, use_color));
        res.push_str(&format_src_line(&relevant_line, use_color));
    } else {
        let line_positions = LinePositions::from(src);

        let spans = line_positions.from_region(offset, end_offset);

        let mut is_first = true;

        if let Some(span) = spans.first() {
            let min_line =
                std::cmp::max(span.line.as_usize() as isize - context_lines as isize, 0) as usize;
            for line_i in min_line..span.line.as_usize() {
                let Some(relevant_line) = s_lines.get(line_i) else {
                    continue;
                };

                if is_first {
                    is_first = false;
                } else {
                    res.push('\n');
                }

                res.push_str(&format_margin_num(
                    &format!("{}", line_i + 1),
                    margin_width,
                    use_color,
                ));
                res.push_str(&format_src_line(relevant_line, use_color));
            }
        }

        let mut has_inline_carets = false;

        for span in &spans {
            // .lines() in the Rust stdlib discards the trailing
            // newline, so we end up with s_lines not having the last line.
            let Some(relevant_line) = s_lines.get(span.line.as_usize()) else {
                continue;
            };

            if is_first {
                is_first = false;
            } else {
                res.push('\n');
            }

            res.push_str(&format_margin_num(
                &span.line.display(),
                margin_width,
                use_color,
            ));
            res.push_str(&format_src_line(relevant_line, use_color));

            if underline {
                res.push('\n');

                let short_next_line = match s_lines.get(span.line.as_usize() + 1) {
                    Some(line) => {
                        if line.len() < span.start_col as usize {
                            Some(line)
                        } else {
                            None
                        }
                    }
                    None => None,
                };

                // If we're printing context and the following line
                // has space for carets, use that line rather than
                // inserting a line for the carets.
                match short_next_line {
                    Some(next_line) if context_lines > 0 && spans.len() == 1 => {
                        has_inline_carets = true;

                        res.push_str(&format_margin_num(
                            &format!("{}", span.line.as_usize() + 2),
                            margin_width,
                            use_color,
                        ));
                        res.push_str(next_line);
                        res.push_str(&" ".repeat(span.start_col as usize - next_line.len()));
                    }
                    _ => {
                        res.push_str(&format_margin_num("", margin_width, use_color));
                        res.push_str(&" ".repeat(span.start_col as usize));
                    }
                }

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

                if let Some(ref msg) = underline_msg {
                    res.push(' ');
                    res.push_str(&format_note_severity(use_color));
                    res.push_str(": ");
                    res.push_str(msg);
                }
            }
        }

        if let Some(span) = spans.last() {
            for line_i in span.line.as_usize() + 1..span.line.as_usize() + 1 + context_lines {
                let Some(relevant_line) = s_lines.get(line_i) else {
                    break;
                };
                if line_i == span.line.as_usize() + 1 && has_inline_carets {
                    continue;
                }

                res.push('\n');

                res.push_str(&format_margin_num(
                    &format!("{}", line_i + 1),
                    margin_width,
                    use_color,
                ));
                res.push_str(relevant_line);
            }
        }
    }

    res
}

fn format_margin_num(num: &str, margin_width: usize, use_color: bool) -> String {
    let s = format!("{:width$}| ", num, width = margin_width - 1);

    if use_color {
        s.dimmed().to_string()
    } else {
        s
    }
}

fn format_src_line(line: &str, use_color: bool) -> String {
    if !use_color {
        return line.to_owned();
    }

    // TODO: this naively assumes that there are no multiline string literals
    // that start on the previous line.

    let vfs_path = VfsPathBuf {
        path: Rc::new(PathBuf::from("irrelevant")),
        id: VfsId(0),
    };
    let (mut stream, _errs) = lex(&vfs_path, line);

    let mut tokens = vec![];
    while let Some(token) = stream.pop() {
        tokens.push(token);
    }

    match tokens.last() {
        Some(last_token) => {
            // Comments are stored on the *following* token. If this line ends
            // with a comment, the last token will come before the comment.
            let (before_comment, after_comment) = line.split_at(last_token.position.end_column);

            let mut s = String::new();
            s.push_str(before_comment);
            s.push_str(&after_comment.dimmed().to_string());
            s
        }
        None => {
            // No tokens, this line is pure whitespace or only a
            // comment.
            line.dimmed().to_string()
        }
    }
}
