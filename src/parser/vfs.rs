use std::path::{Path, PathBuf};
use std::rc::Rc;

use rustc_hash::FxHashMap;

use super::position::Position;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VfsId(pub u32);

/// A path in the `Vfs`, alongside a generation number so we can
/// distinguish multiple values that a file has had.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VfsPathBuf {
    pub(crate) path: Rc<PathBuf>,
    pub(crate) id: VfsId,
}

/// Stores the source code of all the files we've loaded.
#[derive(Debug, Clone, Default)]
pub(crate) struct Vfs {
    file_srcs: FxHashMap<PathBuf, Vec<String>>,
}

impl Vfs {
    pub(crate) fn insert(&mut self, path: Rc<PathBuf>, src: String) -> VfsId {
        let srcs = self.file_srcs.entry(path.to_path_buf()).or_default();
        let new_id = VfsId(srcs.len() as u32);
        srcs.push(src);
        new_id
    }

    pub(crate) fn file_src(&self, path: &Path) -> Option<&String> {
        match self.file_srcs.get(path) {
            Some(srcs) => srcs.last(),
            None => None,
        }
    }

    pub(crate) fn pos_src(&self, pos: &Position) -> Option<&str> {
        let whole_file = self.file_src(pos.path.as_path())?;
        Some(&whole_file[pos.start_offset..pos.end_offset])
    }
}
