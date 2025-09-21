use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parser::ast::{EnumInfo, MethodInfo, StructInfo, SymbolName, SyntaxId, TypeSymbol};
use crate::parser::position::Position;
use crate::parser::vfs::VfsId;
use crate::VfsPathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub(crate) enum BuiltinType {
    // TODO: do we need to denote BuiltinType values for these, now we
    // have stubs in __prelude.gdn?
    Int,
    String,
    Namespace,
    List,
    Dict,

    Fun,
    Tuple,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeDef {
    /// A built-in type, such as Int. StructInfo includes the
    /// definition position and doc comment from __prelude.gdn.
    Builtin(BuiltinType, Option<StructInfo>),
    Enum(EnumInfo),
    Struct(StructInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeDefAndMethods {
    pub(crate) def: TypeDef,
    pub(crate) methods: FxHashMap<SymbolName, MethodInfo>,
}

impl TypeDef {
    pub(crate) fn params(&self) -> Vec<TypeSymbol> {
        let prelude_path = Rc::new(PathBuf::from("__prelude.gdn"));
        let dummy_vfs_path = VfsPathBuf {
            path: prelude_path,
            id: VfsId(u32::MAX),
        };

        match self {
            TypeDef::Builtin(built_in_type, _) => match built_in_type {
                BuiltinType::Int => vec![],
                BuiltinType::String => vec![],
                BuiltinType::Namespace => vec![],
                BuiltinType::Fun => vec![
                    TypeSymbol {
                        name: "Args".into(),
                        position: Position::todo(&dummy_vfs_path),
                        id: SyntaxId(0),
                    },
                    TypeSymbol {
                        name: "Ret".into(),
                        position: Position::todo(&dummy_vfs_path),
                        id: SyntaxId(0),
                    },
                ],
                BuiltinType::List => vec![TypeSymbol {
                    name: "T".into(),
                    position: Position::todo(&dummy_vfs_path),
                    id: SyntaxId(0),
                }],
                BuiltinType::Dict => vec![TypeSymbol {
                    name: "T".into(),
                    position: Position::todo(&dummy_vfs_path),
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
