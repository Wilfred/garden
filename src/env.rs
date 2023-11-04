use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use strum::IntoEnumIterator;

use crate::{
    ast::{
        BuiltinMethodKind, MethodInfo, MethodKind, Position, SourceString, Symbol, SymbolName,
        TestInfo, TypeName,
    },
    eval::{eval_toplevel_defs, Bindings, StackFrame},
    parse::parse_toplevel_items,
    values::{BuiltinFunctionKind, Value},
};

#[derive(Debug)]
pub struct Env {
    pub file_scope: HashMap<SymbolName, Value>,
    pub methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>>,
    pub tests: HashMap<SymbolName, TestInfo>,
    pub types: HashSet<TypeName>,
    pub stack: Vec<StackFrame>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope = HashMap::new();

        // Insert all the built-in functions.
        for fun_kind in BuiltinFunctionKind::iter() {
            file_scope.insert(
                SymbolName(format!("{}", fun_kind)),
                Value::BuiltinFunction(fun_kind),
            );
        }

        let mut methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>> = HashMap::new();

        let mut string_methods = HashMap::new();
        string_methods.insert(
            SymbolName("len".to_owned()),
            MethodInfo {
                receiver_type: TypeName("String".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("len".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLen),
            },
        );
        string_methods.insert(
            SymbolName("substring".to_owned()),
            MethodInfo {
                receiver_type: TypeName("String".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("substring".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringSubstring),
            },
        );
        string_methods.insert(
            SymbolName("concat".to_owned()),
            MethodInfo {
                receiver_type: TypeName("String".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("concat".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringConcat),
            },
        );

        methods.insert(TypeName("String".into()), string_methods);

        let mut list_methods = HashMap::new();
        list_methods.insert(
            SymbolName("append".to_owned()),
            MethodInfo {
                receiver_type: TypeName("List".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("append".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListAppend),
            },
        );
        list_methods.insert(
            SymbolName("len".to_owned()),
            MethodInfo {
                receiver_type: TypeName("List".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("len".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListLen),
            },
        );
        list_methods.insert(
            SymbolName("get".to_owned()),
            MethodInfo {
                receiver_type: TypeName("List".into()),
                receiver_name: SymbolName("__irrelevant".to_owned()),
                name: Symbol {
                    pos: Position::todo(),
                    name: SymbolName("get".to_owned()),
                },
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListGet),
            },
        );

        methods.insert(TypeName("List".into()), list_methods);

        // Insert all the built-in types.
        let types: HashSet<_> = vec![
            // TODO: String literals are duplicated with type_representation.
            TypeName("Int".into()),
            TypeName("Bool".into()),
            TypeName("Fun".into()),
            TypeName("String".into()),
            TypeName("List".into()),
            TypeName("Void".into()),
        ]
        .into_iter()
        .collect();

        let mut env = Self {
            file_scope,
            methods,
            tests: HashMap::new(),
            types,
            stack: vec![StackFrame {
                caller_pos: None,
                bindings: Bindings::default(),
                exprs_to_eval: vec![],
                evalled_values: vec![(
                    Position {
                        // TODO: do these values make sense?
                        start_offset: 0,
                        end_offset: 0,
                        line_number: 0,
                        path: PathBuf::from("__toplevel__"),
                    },
                    Value::Void,
                )],
                enclosing_fun: None,
                enclosing_name: SymbolName("__toplevel__".to_owned()),
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

        env
    }
}

impl Env {
    pub fn pop_to_toplevel(&mut self) {
        self.stack.truncate(1);
        self.stack[0].evalled_values.truncate(1);
        self.stack[0].bindings.0.truncate(1);
    }

    pub fn set_with_file_scope(&mut self, name: &SymbolName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }

    pub fn add_method(&mut self, method_info: &MethodInfo) {
        let type_methods = self
            .methods
            .entry(method_info.receiver_type.clone())
            .or_default();
        type_methods.insert(method_info.name.name.clone(), method_info.clone());
    }
}
