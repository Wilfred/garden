use std::{collections::HashMap, path::PathBuf};

use strum::IntoEnumIterator;

use crate::eval::EnclosingSymbol;
use crate::values::{BuiltinFunctionKind, Value};
use crate::{
    eval::{eval_toplevel_defs, Bindings, StackFrame},
    types::{BuiltinType, TypeDef},
};
use garden_lang_parser::ast::{
    BuiltinMethodKind, MethodInfo, MethodKind, SourceString, Symbol, SymbolName, TestInfo,
    TypeHint, TypeName, TypeSymbol,
};
use garden_lang_parser::parse_toplevel_items;
use garden_lang_parser::position::Position;

#[derive(Debug)]
pub(crate) struct Env {
    pub(crate) file_scope: HashMap<SymbolName, Value>,
    pub(crate) methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>>,
    pub(crate) tests: HashMap<SymbolName, TestInfo>,
    types: HashMap<TypeName, TypeDef>,
    // TODO: should this be stored separately?
    pub(crate) stack: Vec<StackFrame>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope = HashMap::new();

        // Insert all the built-in functions.
        for fun_kind in BuiltinFunctionKind::iter() {
            file_scope.insert(
                SymbolName(format!("{}", fun_kind)),
                Value::BuiltinFunction(fun_kind, None),
            );
        }

        let mut methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>> = HashMap::new();

        let mut string_methods = HashMap::new();
        string_methods.insert(
            SymbolName("len".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("len".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLen, None),
            },
        );
        string_methods.insert(
            SymbolName("substring".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("substring".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringSubstring, None),
            },
        );
        string_methods.insert(
            SymbolName("concat".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("concat".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringConcat, None),
            },
        );

        methods.insert(
            TypeName {
                name: "String".into(),
            },
            string_methods,
        );

        let mut list_methods = HashMap::new();
        list_methods.insert(
            SymbolName("append".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("append".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListAppend, None),
            },
        );
        list_methods.insert(
            SymbolName("len".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("len".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListLen, None),
            },
        );
        list_methods.insert(
            SymbolName("get".to_owned()),
            MethodInfo {
                receiver_type: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("__irrelevant".to_owned()),
                },
                name_sym: Symbol {
                    position: Position::todo(),
                    name: SymbolName("get".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListGet, None),
            },
        );

        methods.insert(
            TypeName {
                name: "List".into(),
            },
            list_methods,
        );

        // Insert all the built-in types.
        let mut types = HashMap::new();
        // TODO: String literals are duplicated with type_representation.
        types.insert(
            TypeName { name: "Int".into() },
            TypeDef::Builtin(BuiltinType::Int),
        );
        types.insert(
            TypeName {
                name: "String".into(),
            },
            TypeDef::Builtin(BuiltinType::String),
        );
        types.insert(
            TypeName {
                name: "List".into(),
            },
            TypeDef::Builtin(BuiltinType::List),
        );
        types.insert(
            TypeName { name: "Fun".into() },
            TypeDef::Builtin(BuiltinType::Fun),
        );

        let mut env = Self {
            file_scope,
            methods,
            tests: HashMap::new(),
            types,
            stack: vec![StackFrame {
                caller_pos: None,
                bindings: Bindings::default(),
                bindings_next_block: vec![],
                exprs_to_eval: vec![],
                evalled_values: vec![Value::unit()],
                enclosing_fun: None,
                enclosing_name: EnclosingSymbol::Toplevel,
                src: SourceString {
                    offset: 0,
                    src: "// __toplevel__".to_owned(),
                },
            }],
        };

        let prelude_src = include_str!("prelude.gdn");
        let prelude_items = parse_toplevel_items(&PathBuf::from("prelude.gdn"), prelude_src)
            .expect("Prelude should be syntactically legal");
        eval_toplevel_defs(&prelude_items, &mut env);

        let builtins_src = include_str!("builtins.gdn");
        let builtin_items = parse_toplevel_items(&PathBuf::from("builtins.gdn"), builtins_src)
            .expect("Stubs for built-ins should be syntactically legal");
        eval_toplevel_defs(&builtin_items, &mut env);

        env
    }
}

impl Env {
    pub(crate) fn pop_to_toplevel(&mut self) {
        self.stack.truncate(1);
        self.stack[0].evalled_values.truncate(1);
        self.stack[0].bindings.block_bindings.truncate(1);
    }

    pub(crate) fn set_with_file_scope(&mut self, name: &SymbolName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }

    pub(crate) fn add_method(&mut self, method_info: &MethodInfo) {
        let type_methods = self
            .methods
            .entry(method_info.receiver_type.sym.name.clone())
            .or_default();
        type_methods.insert(method_info.name_sym.name.clone(), method_info.clone());
    }

    /// Get the type definition associated with this `name`.
    ///
    /// This handles global type definitions, and type variables are
    /// not considered here.
    pub(crate) fn get_type_def<'a>(&'a self, name: &TypeName) -> Option<&'a TypeDef> {
        self.types.get(name)
    }

    pub(crate) fn all_types(&self) -> Vec<TypeName> {
        self.types.keys().cloned().collect()
    }

    pub(crate) fn add_type(&mut self, name: TypeName, type_: TypeDef) {
        self.types.insert(name, type_);
    }
}
