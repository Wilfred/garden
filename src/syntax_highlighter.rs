use std::borrow::Cow;
use std::path::PathBuf;
use std::rc::Rc;

use owo_colors::OwoColorize;
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Helper;

use crate::parser::lex::{lex, STRING_RE, SYMBOL_RE};
use crate::parser::KEYWORDS;
use crate::Vfs;

/// Syntax highlighter for Garden code in the REPL.
pub(crate) struct GardenHighlighter;

impl GardenHighlighter {
    pub(crate) fn new() -> Self {
        Self
    }
}

impl Completer for GardenHighlighter {
    type Candidate = String;
}

impl Hinter for GardenHighlighter {
    type Hint = String;
}

impl Validator for GardenHighlighter {}

impl Highlighter for GardenHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut vfs = Vfs::default();
        let path = Rc::new(PathBuf::from("__highlight__"));
        let vfs_path = vfs.insert(path, line.to_owned());

        let (mut token_stream, _errors) = lex(&vfs_path, line);

        // If there are no tokens, return the line as-is
        if token_stream.is_empty() {
            return Cow::Borrowed(line);
        }

        let mut result = String::new();
        let mut last_offset = 0;

        while let Some(token) = token_stream.pop() {
            // Add any text before this token (whitespace, etc.)
            if token.position.start_offset > last_offset {
                result.push_str(&line[last_offset..token.position.start_offset]);
            }

            for (comment_pos, _comment_text) in &token.preceding_comments {
                if comment_pos.start_offset > last_offset {
                    result.push_str(&line[last_offset..comment_pos.start_offset]);
                }
                let comment_str = &line[comment_pos.start_offset..comment_pos.end_offset];
                result.push_str(&comment_str.dimmed().to_string());
                last_offset = comment_pos.end_offset;
            }

            let token_text = token.text;
            let colored = if KEYWORDS.contains(&token_text) {
                token_text.bold().to_string()
            } else if STRING_RE.is_match(token_text) {
                token_text.green().to_string()
            } else if SYMBOL_RE.is_match(token_text) {
                if token_text.to_lowercase() != token_text
                    && token_text.to_uppercase() != token_text
                {
                    // CamelCase, looks like a type.
                    token_text.purple().bold().to_string()
                } else {
                    token_text.to_owned()
                }
            } else {
                token_text.to_owned()
            };

            result.push_str(&colored);
            last_offset = token.position.end_offset;
        }

        for (comment_pos, _comment_text) in &token_stream.trailing_comments {
            if comment_pos.start_offset > last_offset {
                result.push_str(&line[last_offset..comment_pos.start_offset]);
            }
            let comment_str = &line[comment_pos.start_offset..comment_pos.end_offset];
            result.push_str(&comment_str.dimmed().to_string());
            last_offset = comment_pos.end_offset;
        }

        if last_offset < line.len() {
            result.push_str(&line[last_offset..]);
        }

        Cow::Owned(result)
    }

    fn highlight_char(
        &self,
        _line: &str,
        _pos: usize,
        _kind: rustyline::highlight::CmdKind,
    ) -> bool {
        // Always highlight on every character change
        true
    }
}

impl Helper for GardenHighlighter {}
