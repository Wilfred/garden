use garden_lang_parser::ast::{SymbolName, TypeName};
use rustc_hash::FxHashMap;

use crate::{types::TypeDef, values::Value};

#[derive(Debug, Clone)]
pub(crate) struct NamespaceInfo {
    pub(crate) name: String,
    pub(crate) values: FxHashMap<SymbolName, Value>,
    pub(crate) types: FxHashMap<TypeName, TypeDef>,
}
