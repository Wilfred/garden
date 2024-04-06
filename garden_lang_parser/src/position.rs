use std::path::PathBuf;

use serde::{Deserialize, Serialize};

/// A position is an offset into source code.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// The start of this position, relative to the start of the file.
    pub start_offset: usize,
    /// The end of this position, relative to the start of the file.
    pub end_offset: usize,
    // TODO: Use LineNumber instead, finding a way to serialize it.
    pub line_number: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub path: PathBuf,
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("Position")
                .field("start_offset", &self.start_offset)
                .field("end_offset", &self.end_offset)
                .field("line_number", &self.line_number)
                .finish()
        } else {
            if self.path == PathBuf::from("/position/todo") {
                f.write_str("Position { TODO }")
            } else {
                f.write_str("Position { ... }")
            }
        }
    }
}

impl Position {
    pub fn todo() -> Self {
        Self {
            start_offset: 0,
            end_offset: 0,
            line_number: 0,
            path: PathBuf::from("/position/todo"),
        }
    }

    /// Return the merged position of `first` and `second`. Assumes
    /// that `second` occurs after `first`.
    pub fn merge(first: Self, second: Self) -> Self {
        Self {
            start_offset: first.start_offset,
            end_offset: second.end_offset,
            line_number: first.line_number,
            path: first.path,
        }
    }

    /// Format this position as a human-friendly string, e.g. "foo.gdn:123".
    pub fn as_ide_string(&self) -> String {
        format!(
            "{}:{}",
            self.path.display(),
            // 1-indexed line number
            self.line_number + 1,
        )
    }
}
