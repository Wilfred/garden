use std::cell::OnceCell;

use garden_lang_parser::{
    ast::{EnumInfo, StructInfo, TypeSymbol},
    position::Position,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BuiltinType {
    Int,
    String,
    Fun,
    List,
    Tuple,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeDef {
    Builtin(BuiltinType),
    Enum(EnumInfo),
    Struct(StructInfo),
}

impl TypeDef {
    pub(crate) fn params(&self) -> Vec<TypeSymbol> {
        match self {
            TypeDef::Builtin(built_in_type) => match built_in_type {
                BuiltinType::Int => vec![],
                BuiltinType::String => vec![],
                BuiltinType::Fun => vec![
                    TypeSymbol {
                        name: "Args".into(),
                        position: Position::todo(),
                        id: OnceCell::new(),
                    },
                    TypeSymbol {
                        name: "Ret".into(),
                        position: Position::todo(),
                        id: OnceCell::new(),
                    },
                ],
                BuiltinType::List => vec![TypeSymbol {
                    name: "T".into(),
                    position: Position::todo(),
                    id: OnceCell::new(),
                }],
                BuiltinType::Tuple => {
                    // TODO: tuple can actually take an arbtitrary
                    // number of arugments.
                    vec![]
                }
            },
            TypeDef::Enum(enum_info) => enum_info.type_params.clone(),
            TypeDef::Struct(struct_info) => struct_info.type_params.clone(),
        }
    }
}
