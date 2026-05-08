use std::borrow::Cow;

use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Helper;
use strum::IntoEnumIterator;

use crate::commands::Command;
use crate::diagnostics::with_syntax_highlighting;

/// Syntax highlighter for Garden code in the REPL.
pub(crate) struct GardenHighlighter;

impl GardenHighlighter {
    pub(crate) fn new() -> Self {
        Self
    }
}

impl Completer for GardenHighlighter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let line_to_cursor = &line[..pos];

        // Only complete command names: input starts with ':' and no space yet.
        if !line_to_cursor.starts_with(':') || line_to_cursor.contains(' ') {
            return Ok((pos, vec![]));
        }

        let candidates: Vec<String> = Command::iter()
            .map(|cmd| cmd.to_string())
            .filter(|name| name.starts_with(line_to_cursor))
            .collect();

        Ok((0, candidates))
    }
}

impl Hinter for GardenHighlighter {
    type Hint = String;
}

impl Validator for GardenHighlighter {}

impl Highlighter for GardenHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let result = with_syntax_highlighting(line, false);
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
