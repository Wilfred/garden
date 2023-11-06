use crate::ast::EnumInfo;

#[derive(Debug)]
pub enum BuiltinType {
    Int,
    String,
    // TODO: these require a type parameter.
    Fun,
    List,
}

#[derive(Debug)]
pub enum Type {
    Builtin(BuiltinType),
    Enum(EnumInfo),
}

