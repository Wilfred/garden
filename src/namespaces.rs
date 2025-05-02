use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parser::ast::{SymbolName, TypeName};
use crate::types::TypeDefAndMethods;
use crate::values::Value;

#[derive(Debug, Clone)]
pub(crate) struct NamespaceInfo {
    pub(crate) path: Rc<PathBuf>,
    /// The absolute path to this loaded file. Note that built-in
    /// namespaces like the prelude are still just "__prelude.gdn".
    pub(crate) abs_path: Rc<PathBuf>,
    pub(crate) values: FxHashMap<SymbolName, Value>,
    pub(crate) types: FxHashMap<TypeName, TypeDefAndMethods>,
}
