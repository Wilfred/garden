use garden_lang_parser::ast::EnumInfo;

#[derive(Debug)]
pub(crate) enum BuiltinType {
    Int,
    String,
    // TODO: these require a type parameter.
    Fun,
    List,
}

#[derive(Debug)]
pub(crate) enum Type {
    Builtin(BuiltinType),
    Enum(EnumInfo),
}
