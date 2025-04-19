use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parser::ast::{SymbolName, TypeName};
use crate::{types::TypeDef, values::Value};

#[derive(Debug, Clone)]
pub(crate) struct NamespaceInfo {
    pub(crate) name: String,
    pub(crate) path: Rc<PathBuf>,
    pub(crate) values: FxHashMap<SymbolName, Value>,
    pub(crate) types: FxHashMap<TypeName, TypeDef>,
}
