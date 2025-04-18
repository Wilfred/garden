use rustc_hash::FxHashMap;

use crate::parser::ast::{SymbolName, TypeName};
use crate::{types::TypeDef, values::Value};

#[derive(Debug, Clone)]
pub(crate) struct NamespaceInfo {
    pub(crate) name: String,
    pub(crate) values: FxHashMap<SymbolName, Value>,
    pub(crate) types: FxHashMap<TypeName, TypeDef>,
}
