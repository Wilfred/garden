use std::path::{Path, PathBuf};
use std::rc::Rc;

use normalize_path::NormalizePath as _;
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
    pub(crate) fn singleton(path: PathBuf, src: String) -> (Self, VfsPathBuf) {
        let mut vfs = Self::default();
        let vfs_path = vfs.insert(Rc::new(path.clone()), src);
        (vfs, vfs_path)
    }

    pub(crate) fn insert(&mut self, path: Rc<PathBuf>, src: String) -> VfsPathBuf {
        let srcs = self.file_srcs.entry(path.to_path_buf()).or_default();
        let vfs_id = VfsId(srcs.len() as u32);
        srcs.push(src);

        VfsPathBuf { path, id: vfs_id }
    }

    pub(crate) fn file_src(&self, vfs_path: &VfsPathBuf) -> Option<&String> {
        match self.file_srcs.get(&*vfs_path.path) {
            Some(srcs) => srcs.get(vfs_path.id.0 as usize),
            None => None,
        }
    }

    pub(crate) fn pos_src(&self, pos: &Position) -> Option<&str> {
        let whole_file = self.file_src(&pos.vfs_path)?;
        Some(&whole_file[pos.start_offset..pos.end_offset])
    }
}

pub(crate) fn to_abs_path(path: &Path) -> PathBuf {
    let current_dir: PathBuf = match std::env::current_dir() {
        Ok(p) => {
            if std::env::var("GDN_NO_ABS_PATH").is_ok() {
                PathBuf::from("GDN_ABS_PATH_REPLACED")
            } else {
                p
            }
        }
        Err(_) => "/".into(),
    };

    current_dir.join(path).normalize()
}
