//! On-disk temporary copies of the built-in Garden files
//! (`__prelude.gdn`, `__fs.gdn`, ...).
//!
//! The LSP and nREPL servers write the built-ins into a temporary
//! directory on startup so that editors can navigate to them via
//! go-to-def or `lookup`. The directory is removed when the value is
//! dropped.

use std::io;
use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;

use crate::eval::BUILT_IN_FILES;

/// On-disk copies of the built-in Garden files, written into a
/// temporary directory. The directory is removed when this value is
/// dropped.
pub(crate) struct TempBuiltInFiles {
    dir: PathBuf,
    paths: FxHashMap<PathBuf, PathBuf>,
}

impl TempBuiltInFiles {
    /// Write the built-in files into a fresh directory inside the
    /// system temp directory, named with `prefix` and the current
    /// process ID.
    pub(crate) fn new(prefix: &str) -> io::Result<Self> {
        let dir = std::env::temp_dir().join(format!("{prefix}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir)?;

        let mut paths = FxHashMap::default();
        for (name, content) in BUILT_IN_FILES {
            let temp_path = dir.join(name);
            std::fs::write(&temp_path, content)?;

            if let Ok(meta) = std::fs::metadata(&temp_path) {
                let mut perms = meta.permissions();
                perms.set_readonly(true);
                let _ = std::fs::set_permissions(&temp_path, perms);
            }

            paths.insert(PathBuf::from(name), temp_path);
        }

        Ok(Self { dir, paths })
    }

    /// If `path` refers to a built-in file, return the on-disk path of
    /// the temporary copy.
    pub(crate) fn resolve(&self, path: &Path) -> Option<PathBuf> {
        self.paths.get(path).cloned()
    }

    #[cfg(test)]
    pub(crate) fn dir(&self) -> &Path {
        &self.dir
    }
}

impl Drop for TempBuiltInFiles {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_built_in_and_cleans_up() {
        let dir_path;
        {
            let built_ins = TempBuiltInFiles::new("garden-built-in-files-test").unwrap();
            dir_path = built_ins.dir().to_path_buf();
            assert!(dir_path.is_dir());

            let resolved = built_ins
                .resolve(&PathBuf::from("__prelude.gdn"))
                .expect("__prelude.gdn should resolve");
            assert!(resolved.is_file());

            assert!(built_ins
                .resolve(&PathBuf::from("not_a_builtin.gdn"))
                .is_none());
        }
        assert!(!dir_path.exists());
    }
}
