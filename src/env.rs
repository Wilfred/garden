use std::cell::OnceCell;
use std::{collections::HashMap, path::PathBuf};

use strum::IntoEnumIterator;

use crate::eval::EnclosingSymbol;
use crate::garden_type::TypeVarEnv;
use crate::values::{BuiltinFunctionKind, Value};
use crate::{
    eval::{eval_toplevel_defs, Bindings, StackFrame},
    types::{BuiltinType, TypeDef},
};
use garden_lang_parser::ast::{
    BuiltinMethodKind, MethodInfo, MethodKind, SourceString, Symbol, SymbolName, SyntaxIdGenerator,
    TestInfo, TypeHint, TypeName, TypeSymbol,
};
use garden_lang_parser::parse_toplevel_items;
use garden_lang_parser::position::Position;

#[derive(Debug, Clone)]
pub(crate) struct Stack(pub(crate) Vec<StackFrame>);

impl Default for Stack {
    fn default() -> Self {
        Self(vec![StackFrame {
            caller_pos: None,
            caller_expr_id: None,
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
            type_bindings: HashMap::new(),
        }])
    }
}

impl Stack {
    pub(crate) fn pop_to_toplevel(&mut self) {
        if self.0.is_empty() {
            return;
        }

        self.0.truncate(1);
        self.0[0].evalled_values.truncate(1);
        self.0[0].bindings.block_bindings.truncate(1);
    }

    pub(crate) fn type_bindings(&self) -> TypeVarEnv {
        let Some(stack_frame) = self.0.last() else {
            return HashMap::default();
        };

        stack_frame.type_bindings.clone()
    }
}

#[derive(Debug)]
pub(crate) struct Env {
    pub(crate) file_scope: HashMap<SymbolName, Value>,
    pub(crate) methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>>,
    pub(crate) tests: HashMap<SymbolName, TestInfo>,
    types: HashMap<TypeName, TypeDef>,
    pub(crate) id_gen: SyntaxIdGenerator,
    /// The arguments used the last time each function was
    /// called. Used for eval-up-to.
    pub(crate) prev_call_args: HashMap<SymbolName, Vec<Value>>,
    /// The receiver and arguments used the last time each method was
    /// called. Used for eval-up-to.
    pub(crate) prev_method_call_args: HashMap<TypeName, HashMap<SymbolName, (Value, Vec<Value>)>>,
    // TODO: should this be stored separately?
    pub(crate) stack: Stack,
}

impl Default for Env {
    fn default() -> Self {
        let mut id_gen = SyntaxIdGenerator::default();

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
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "len"),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLen, None),
            },
        );
        string_methods.insert(
            SymbolName("substring".to_owned()),
            MethodInfo {
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "substring"),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringSubstring, None),
            },
        );
        string_methods.insert(
            SymbolName("append".to_owned()),
            MethodInfo {
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "String".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "append"),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringAppend, None),
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
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "append"),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListAppend, None),
            },
        );
        list_methods.insert(
            SymbolName("len".to_owned()),
            MethodInfo {
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "len"),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListLen, None),
            },
        );
        list_methods.insert(
            SymbolName("get".to_owned()),
            MethodInfo {
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(),
                        name: TypeName {
                            name: "List".into(),
                        },
                        id: OnceCell::new(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant"),
                name_sym: Symbol::new(Position::todo(), "get"),
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
            TypeName {
                name: "Tuple".into(),
            },
            TypeDef::Builtin(BuiltinType::Tuple),
        );
        types.insert(
            TypeName { name: "Fun".into() },
            TypeDef::Builtin(BuiltinType::Fun),
        );

        let prelude_src = include_str!("prelude.gdn");
        let (prelude_items, errors) =
            parse_toplevel_items(&PathBuf::from("prelude.gdn"), prelude_src, &mut id_gen);
        assert!(errors.is_empty(), "Prelude should be syntactically legal");

        let builtins_src = include_str!("builtins.gdn");
        let (builtin_items, errors) =
            parse_toplevel_items(&PathBuf::from("builtins.gdn"), builtins_src, &mut id_gen);
        assert!(
            errors.is_empty(),
            "Stubs for built-ins should be syntactically legal"
        );
        let mut env = Self {
            file_scope,
            methods,
            tests: HashMap::new(),
            types,
            id_gen,
            prev_call_args: HashMap::new(),
            prev_method_call_args: HashMap::new(),
            stack: Stack::default(),
        };

        eval_toplevel_defs(&prelude_items, &mut env);
        eval_toplevel_defs(&builtin_items, &mut env);

        env
    }
}

impl Env {
    pub(crate) fn set_with_file_scope(&mut self, name: &SymbolName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }

    pub(crate) fn add_method(&mut self, method_info: &MethodInfo) {
        let type_methods = self
            .methods
            .entry(method_info.receiver_hint.sym.name.clone())
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
