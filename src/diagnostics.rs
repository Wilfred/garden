//! Error and warning data types, along with logic to display them.

use std::collections::{BTreeMap, BTreeSet};
use std::io::IsTerminal as _;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use itertools::Itertools;
use line_numbers::LinePositions;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::env::{EnclosingSymbol, StackFrame};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::lex::{lex, STRING_RE};
use crate::parser::position::Position;
use crate::parser::vfs::{to_project_relative, Vfs, VfsId};
use crate::parser::KEYWORDS;
use crate::VfsPathBuf;

#[derive(Debug, Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub(crate) struct Autofix {
    pub(crate) description: String,
    pub(crate) position: Position,
    pub(crate) new_text: String,
}

#[derive(Debug)]
pub(crate) struct Diagnostic {
    /// The primary message.
    pub(crate) message: ErrorMessage,
    pub(crate) position: Position,
    /// How bad it is.
    pub(crate) severity: Severity,
    /// Relevant information from other places in the program, such as
    /// "note: foo() is defined here".
    pub(crate) notes: Vec<(ErrorMessage, Position)>,
    pub(crate) fixes: Vec<Autofix>,
}

pub(crate) fn format_exception_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
    vfs: &Vfs,
    project_root: &Path,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let mut res = String::new();

    res.push_str(&format!(
        "{}: {}\n",
        if use_color {
            "Exception".bold().red().to_string()
        } else {
            "Exception".to_owned()
        },
        if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        }
    ));

    // For the topmost (most recently called) stack frame, the
    // relevant position is where the error occurred.
    let top_stack = stack.last().unwrap();
    res.push_str(&format_pos_in_fun(
        position,
        vfs,
        Some(&top_stack.enclosing_name),
        project_root,
        true,
        None,
        true,
        false,
        0,
        &[],
    ));

    // For the rest of the stack, we want the positions of calls.
    for (callee_stack_frame, caller_stack_frame) in stack.iter().rev().tuple_windows() {
        if let Some(pos) = &callee_stack_frame.caller_pos {
            res.push('\n');
            res.push_str(&format_pos_in_fun(
                pos,
                vfs,
                Some(&caller_stack_frame.enclosing_name),
                project_root,
                true,
                None,
                false,
                false,
                0,
                &[],
            ));
        }
    }

    res
}

pub(crate) fn format_diagnostic(
    message: &ErrorMessage,
    position: &Position,
    project_root: &Path,
    severity: Severity,
    notes: &[(ErrorMessage, Position)],
    vfs: &Vfs,
) -> String {
    let use_color = std::io::stdout().is_terminal();

    let severity_s = format_severity(severity, use_color);

    let mut s = format!(
        "{}: {}\n\n",
        severity_s,
        if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        }
    );
    let mut files = vec![vec![SourceAnnotation {
        position,
        message: None,
    }]];
    for (message, position) in notes {
        let annotation = SourceAnnotation {
            position,
            message: Some(if use_color {
                message.as_styled_string()
            } else {
                message.as_string()
            }),
        };
        // Versions of a file can have different source text.
        if let Some(file) = files
            .iter_mut()
            .find(|file| file[0].position.vfs_path == position.vfs_path)
        {
            file.push(annotation);
        } else {
            files.push(vec![annotation]);
        }
    }

    let context_lines = 2;
    let mut excerpts = vec![];
    for file in files {
        let mut sorted: Vec<_> = file.into_iter().enumerate().collect();
        sorted.sort_by_key(|(_, annotation)| annotation.position.line_number);
        let mut groups: Vec<Vec<(usize, SourceAnnotation<'_>)>> = vec![];
        let mut end_line = 0;
        for (index, annotation) in sorted {
            let pos = annotation.position;
            if groups.is_empty()
                || pos.line_number.saturating_sub(context_lines) > end_line + context_lines + 1
            {
                groups.push(vec![]);
                end_line = pos.end_line_number;
            } else {
                end_line = end_line.max(pos.end_line_number);
            }
            groups.last_mut().unwrap().push((index, annotation));
        }
        for group in &mut groups {
            group.sort_by_key(|(index, _)| *index);
        }
        groups.sort_by_key(|group| group[0].0);
        excerpts.extend(groups);
    }

    for (i, annotations) in excerpts.iter().enumerate() {
        if i > 0 {
            s.push('\n');
        }
        let first = &annotations[0].1;
        let notes: Vec<_> = annotations[1..]
            .iter()
            .map(|(_, annotation)| annotation)
            .collect();
        s.push_str(&format_pos_in_fun(
            first.position,
            vfs,
            None,
            project_root,
            true,
            first.message.clone(),
            matches!(severity, Severity::Error),
            first.message.is_some(),
            context_lines,
            &notes,
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

struct SourceAnnotation<'a> {
    position: &'a Position,
    message: Option<String>,
}

/// Return a string of the source code around this position, with a
/// left margin of line numbers, and an underline of the relevant
/// portion.
fn format_pos_in_fun(
    position: &Position,
    vfs: &Vfs,
    enclosing_symbol: Option<&EnclosingSymbol>,
    project_root: &Path,
    underline: bool,
    underline_msg: Option<String>,
    is_error: bool,
    is_note: bool,
    context_lines: usize,
    notes: &[&SourceAnnotation<'_>],
) -> String {
    let use_color = std::io::stdout().is_terminal();
    let primary = SourceAnnotation {
        position,
        message: underline_msg,
    };
    let pos_path = to_project_relative(&position.path, project_root);
    let src = match vfs.file_src(&position.vfs_path) {
        Some(src) => src,
        None => "",
    };

    let s_lines: Vec<_> = src.lines().collect();
    let line_positions = LinePositions::from(src);
    let mut lines = BTreeSet::new();
    let mut highlights = BTreeMap::new();
    let mut max_line_shown = 0;

    for annotation in std::iter::once(&primary).chain(notes.iter().copied()) {
        let pos = annotation.position;
        if s_lines.is_empty()
            || pos.start_offset > pos.end_offset
            || pos.start_offset >= src.len()
            || pos.end_offset >= src.len()
        {
            continue;
        }
        let spans = line_positions.from_region(pos.start_offset, pos.end_offset);
        if let (Some(first), Some(last)) = (spans.first(), spans.last()) {
            let mut start = first.line.as_usize().saturating_sub(context_lines);
            let mut end = (last.line.as_usize() + context_lines + 1).min(s_lines.len());
            max_line_shown = max_line_shown.max(end);
            while start < first.line.as_usize() && s_lines[start].trim().is_empty() {
                start += 1;
            }
            if context_lines > 0
                && last.line.as_usize() + context_lines < s_lines.len()
                && s_lines[end - 1].trim().is_empty()
            {
                end -= 1;
            }
            lines.extend(start..end);
        }
        let single_line = spans.len() == 1;
        for span in spans {
            highlights
                .entry(span.line.as_usize())
                .or_insert_with(Vec::new)
                .push((span, annotation, single_line));
        }
    }

    if max_line_shown == 0 {
        max_line_shown = s_lines.len();
    }
    // Size the left margin to fit the largest line number we will print,
    // not the largest line number in the file overall.
    let margin_width = format!("{}", max_line_shown).len().max(1);
    let mut res = String::new();

    let formatted_pos = format!(
        "{}| {}:{}:{}",
        "-".repeat(margin_width),
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
        let mut signature = format!("\t {enclosing_sym}");
        if use_color {
            signature = signature.bold().dimmed().to_string();
        }

        res.push_str(&signature);
    }
    res.push('\n');

    if lines.is_empty() && !s_lines.is_empty() {
        res.push_str(&format_margin_num("?", margin_width, use_color));
        res.push_str(&format_src_line(s_lines[0], use_color));
    }

    let mut is_first = true;
    let mut inline_line = None;
    for line_i in lines.iter().copied() {
        if inline_line != Some(line_i) {
            if !is_first {
                res.push('\n');
            }
            res.push_str(&format_margin_num(
                &(line_i + 1).to_string(),
                margin_width,
                use_color,
            ));
            res.push_str(&format_src_line(s_lines[line_i], use_color));
        }
        is_first = false;

        if !underline {
            continue;
        }
        let Some(line_highlights) = highlights.get(&line_i) else {
            continue;
        };
        for (span, annotation, single_line) in line_highlights {
            res.push('\n');
            // A single underline can share the following source line
            // when there is space beside its text.
            let next_line = s_lines.get(line_i + 1).filter(|line| {
                context_lines > 0
                    && *single_line
                    && line_highlights.len() == 1
                    && line.len() < span.start_col as usize
            });
            if let Some(next_line) = next_line {
                inline_line = Some(line_i + 1);
                res.push_str(&format_margin_num(
                    &(line_i + 2).to_string(),
                    margin_width,
                    use_color,
                ));
                res.push_str(next_line);
                res.push_str(&" ".repeat(span.start_col as usize - next_line.len()));
            } else {
                res.push_str(&format_margin_num("", margin_width, use_color));
                res.push_str(&" ".repeat(span.start_col as usize));
            }
            let carets = "^".repeat((span.end_col - span.start_col) as usize);
            res.push_str(&if !use_color {
                carets
            } else if is_note || annotation.message.is_some() {
                carets.bold().dimmed().to_string()
            } else if is_error {
                carets.bold().red().to_string()
            } else {
                carets.bold().yellow().to_string()
            });
            if let Some(message) = &annotation.message {
                res.push_str(&format!(
                    " {}: {}",
                    format_note_severity(use_color),
                    message
                ));
            }
        }
    }
    res
}

fn format_margin_num(num: &str, margin_width: usize, use_color: bool) -> String {
    let s = format!("{:>width$}| ", num, width = margin_width);

    if use_color {
        s.dimmed().to_string()
    } else {
        s
    }
}

fn looks_like_type_name(text: &str) -> bool {
    let mut chars = text.chars();
    match chars.next() {
        Some(first_char) => first_char.is_uppercase() && chars.any(|c| c.is_lowercase()),
        None => false,
    }
}

pub(crate) fn with_syntax_highlighting(line: &str, normal_text_as_bold: bool) -> String {
    let vfs_path = VfsPathBuf {
        path: Rc::new(PathBuf::from("irrelevant")),
        id: VfsId(0),
    };
    let (mut stream, _errs) = lex(&vfs_path, line);

    let mut s = String::new();
    let mut offset = 0;
    while let Some(token) = stream.pop() {
        let before_text = &line[offset..token.position.start_offset];
        s.push_str(before_text);

        if KEYWORDS.contains(&token.text) {
            s.push_str(&token.text.bold().to_string());
        } else if STRING_RE.is_match(token.text) {
            s.push_str(&token.text.bright_blue().to_string());
        } else if looks_like_type_name(token.text) {
            s.push_str(&token.text.bright_purple().bold().to_string());
        } else {
            s.push_str(&if normal_text_as_bold {
                token.text.bold().to_string()
            } else {
                token.text.to_owned()
            });
        }

        offset = token.position.start_offset + token.text.len();
    }

    // Anything after the last token is whitespace or a comment.
    let after_text = &line[offset..];
    if normal_text_as_bold {
        s.push_str(&after_text.bold().to_string());
    } else {
        s.push_str(&after_text.dimmed().to_string());
    }

    s
}

fn format_src_line(line: &str, use_color: bool) -> String {
    if !use_color {
        return line.to_owned();
    }

    // TODO: this naively assumes that there are no multiline string literals
    // that start on the previous line.

    with_syntax_highlighting(line, false)
}
