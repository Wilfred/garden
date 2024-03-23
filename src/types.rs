use garden_lang_parser::ast::{EnumInfo, StructInfo};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BuiltinType {
    Int,
    String,
    // TODO: these require a type parameter.
    Fun,
    List,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeDef {
    Builtin(BuiltinType),
    Enum(EnumInfo),
    Struct(StructInfo),
}
