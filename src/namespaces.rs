use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::{SymbolName, TypeName};
use crate::types::TypeDefAndMethods;
use crate::values::Value;

#[derive(Debug, Clone)]
pub(crate) struct NamespaceInfo {
    /// The absolute path to this loaded file. Note that built-in
    /// namespaces like the prelude are still just "__prelude.gdn".
    pub(crate) abs_path: Rc<PathBuf>,

    /// The values in this namespace: the functions and other
    /// namespaces that are in scope.
    pub(crate) values: FxHashMap<SymbolName, Value>,

    /// Symbols that are visible outside of this namespace.
    pub(crate) external_syms: FxHashSet<SymbolName>,

    pub(crate) types: FxHashMap<TypeName, TypeDefAndMethods>,
}
