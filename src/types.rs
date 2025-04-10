use std::{path::PathBuf, rc::Rc};

use garden_lang_parser::{
    ast::{EnumInfo, StructInfo, SyntaxId, TypeSymbol},
    position::Position,
};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub(crate) enum BuiltinType {
    // TODO: do we need to denote BuiltinType values for these, now we
    // have stubs in builtins.gdn?
    Int,
    String,
    Namespace,
    List,

    Fun,
    Tuple,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeDef {
    /// A built-in type, such as Int. StructInfo includes the
    /// definition position and doc comment from builtins.gdn.
    Builtin(BuiltinType, Option<StructInfo>),
    Enum(EnumInfo),
    Struct(StructInfo),
}

impl TypeDef {
    pub(crate) fn params(&self) -> Vec<TypeSymbol> {
        let builtins_path = Rc::new(PathBuf::from("builtins.gdn"));

        match self {
            TypeDef::Builtin(built_in_type, _) => match built_in_type {
                BuiltinType::Int => vec![],
                BuiltinType::String => vec![],
                BuiltinType::Namespace => vec![],
                BuiltinType::Fun => vec![
                    TypeSymbol {
                        name: "Args".into(),
                        position: Position::todo(builtins_path.clone()),
                        id: SyntaxId(0),
                    },
                    TypeSymbol {
                        name: "Ret".into(),
                        position: Position::todo(builtins_path.clone()),
                        id: SyntaxId(0),
                    },
                ],
                BuiltinType::List => vec![TypeSymbol {
                    name: "T".into(),
                    position: Position::todo(builtins_path.clone()),
                    id: SyntaxId(0),
                }],
                BuiltinType::Tuple => {
                    // TODO: tuple can actually take an arbitrary
                    // number of arguments.
                    vec![]
                }
            },
            TypeDef::Enum(enum_info) => enum_info.type_params.clone(),
            TypeDef::Struct(struct_info) => struct_info.type_params.clone(),
        }
    }
}
