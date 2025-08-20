use std::path::{Path, PathBuf};
use std::rc::Rc;

use serde::Serialize;

use crate::parser::lex::Token;
use crate::parser::vfs::to_project_relative;

use super::vfs::VfsPathBuf;

/// A position is a range in source code. It is a span between
/// `start_offset` and `end_offset` in `path`.
#[derive(Clone, PartialEq, Eq, Serialize, PartialOrd, Ord)]
pub(crate) struct Position {
    /// The start of this position, relative to the start of the
    /// file. Measured in bytes.
    pub(crate) start_offset: usize,
    /// The end of this position, relative to the start of the
    /// file. Measured in bytes.
    ///
    /// This is an include-exclusive range. For example, a file
    /// consisting of `abc` is a single symbol with start offset 0,
    /// end offset 3, and a width of 3. However, the individual
    /// letters are at offsets 0, 1 and 2.
    pub(crate) end_offset: usize,
    // TODO: Use LineNumber instead, finding a way to serialize it.
    pub(crate) line_number: usize,
    pub(crate) end_line_number: usize,
    /// The column of the start of this position. Zero indexed.
    pub(crate) column: usize,
    pub(crate) end_column: usize,
    pub(crate) path: Rc<PathBuf>,
    /// The file where this position occurred. A Vfs path differs from
    /// a normal PathBuf in that it has an ID, so we can distinguish
    /// different versions of the same file that we saw.
    #[serde(skip)]
    pub(crate) vfs_path: VfsPathBuf,
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("VERBOSE").is_ok() {
            f.debug_struct("Position")
                .field("start_offset", &self.start_offset)
                .field("end_offset", &self.end_offset)
                .field("line_number", &self.line_number)
                .field("end_line_number", &self.end_line_number)
                .field("path", &self.path)
                .field("vfs_path", &self.vfs_path)
                .finish()
        } else {
            f.write_str("Position { ... }")
        }
    }
}

impl Position {
    pub(crate) fn todo(vfs_path: &VfsPathBuf) -> Self {
        Self {
            start_offset: 0,
            end_offset: 0,
            line_number: 0,
            column: 0,
            end_column: 0,
            end_line_number: 0,
            path: vfs_path.path.clone(),
            vfs_path: vfs_path.to_owned(),
        }
    }

    /// Return the merged position of `first` and `second`.
    pub(crate) fn merge(first: &Self, second: &Self) -> Self {
        Self {
            start_offset: first.start_offset,
            end_offset: std::cmp::max(first.end_offset, second.end_offset),
            line_number: first.line_number,
            end_line_number: std::cmp::max(first.end_line_number, second.end_line_number),
            column: first.column,
            end_column: if first.end_offset > second.end_offset {
                first.end_column
            } else {
                second.end_column
            },
            path: first.path.clone(),
            vfs_path: first.vfs_path.clone(),
        }
    }

    /// Merge the position of this token (including doc comments) with
    /// the second position.
    pub(crate) fn merge_token(first: &Token, second: &Self) -> Self {
        let mut first_pos = first.position.clone();
        if let Some((comment_pos, _)) = first.preceding_comments.first() {
            first_pos = comment_pos.clone();
        }

        Self::merge(&first_pos, second)
    }

    /// Format this position as a human-friendly string, e.g. "foo.gdn:123".
    pub(crate) fn as_ide_string(&self, project_root: &Path) -> String {
        let path = to_project_relative(&self.path, project_root);

        format!(
            "{}:{}",
            path.display(),
            // 1-indexed line number
            self.line_number + 1,
        )
    }

    pub(crate) fn contains_offset(&self, offset: usize) -> bool {
        self.start_offset <= offset && offset < self.end_offset
    }

    /// Does this position contain `offset`, if we treat it as an
    /// inclusive range?
    ///
    /// This is useful when dealing with ranges. It's nice to think of
    /// a symbol `abc` as being in the range 0-3, but it has
    /// characters at lines 0, 1 and 2.
    ///
    /// This helper allows us to deal with that scenario by checking
    /// the offset (generally the end offset)
    pub(crate) fn contains_offset_inclusive(&self, offset: usize) -> bool {
        self.start_offset <= offset && offset <= self.end_offset
    }
}
