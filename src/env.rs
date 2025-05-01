use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use rustc_hash::FxHashMap;
use strum::IntoEnumIterator;

use crate::eval::{load_toplevel_items, Bindings, EnclosingSymbol, ExpressionState};
use crate::garden_type::TypeVarEnv;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    BuiltinMethodKind, Expression, IdGenerator, MethodInfo, MethodKind, Symbol, SymbolName,
    SyntaxId, TestInfo, TypeHint, TypeName, TypeSymbol,
};
use crate::parser::parse_toplevel_items;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::types::{BuiltinType, TypeDef, TypeDefAndMethods};
use crate::values::{BuiltinFunctionKind, Value, Value_};
use crate::VfsPathBuf;

#[derive(Debug, Clone)]
pub(crate) struct Stack(pub(crate) Vec<StackFrame>);

impl Stack {
    pub(crate) fn new(namespace: Rc<RefCell<NamespaceInfo>>) -> Self {
        Self(vec![StackFrame {
            namespace,
            caller_pos: None,
            caller_expr_id: None,
            bindings: Bindings::default(),
            bindings_next_block: vec![],
            exprs_to_eval: vec![],
            evalled_values: vec![Value::unit()],
            return_hint: None,
            enclosing_name: EnclosingSymbol::Toplevel,
            type_bindings: FxHashMap::default(),
            caller_uses_value: true,
        }])
    }

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
            return FxHashMap::default();
        };

        stack_frame.type_bindings.clone()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Env {
    pub(crate) tests: FxHashMap<SymbolName, TestInfo>,
    pub(crate) types: FxHashMap<TypeName, TypeDefAndMethods>,

    pub(crate) prelude_namespace: Rc<RefCell<NamespaceInfo>>,
    pub(crate) namespaces: FxHashMap<PathBuf, Rc<RefCell<NamespaceInfo>>>,

    /// File paths relative to this directory will be shown as
    /// relative paths in e.g. errors.
    pub(crate) project_root: PathBuf,

    /// The arguments used the last time each function was
    /// called. Used for eval-up-to.
    pub(crate) prev_call_args: FxHashMap<SymbolName, Vec<Value>>,
    /// The receiver and arguments used the last time each method was
    /// called. Used for eval-up-to.
    pub(crate) prev_method_call_args:
        FxHashMap<TypeName, FxHashMap<SymbolName, (Value, Vec<Value>)>>,
    // TODO: should this be stored separately?
    pub(crate) stack: Stack,

    /// The number of execution steps we've evaluated so far.
    pub(crate) ticks: usize,
    /// Stop evaluation if we exceed this number of ticks.
    pub(crate) tick_limit: Option<usize>,

    /// Stop after evaluating the expression with this ID, if we reach
    /// it.
    ///
    /// Used for 'evaluate up to cursor'.
    pub(crate) stop_at_expr_id: Option<SyntaxId>,

    /// Refuse to run code might modify the system, such as filesystem
    /// access or shell commands. This should allow us to run
    /// arbitrary code safely.
    pub(crate) enforce_sandbox: bool,

    /// Used to track the IDs generated so far, so any additional
    /// parsing produces items with globally unique IDs.
    pub(crate) id_gen: IdGenerator,

    /// A copy of the source code of all files we've evaluated.
    pub(crate) vfs: Vfs,

    /// A copy of the environment before we started adding things to
    /// it. This is useful when running checks at runtime, where we
    /// don't want to do the work of initialising a fresh environment
    /// repeatedly.
    pub(crate) initial_state: Option<Box<Self>>,

    /// Command line arguments used to invoke this Garden program,
    /// e.g. `vec!["--stuff"]`.
    pub(crate) cli_args: Vec<String>,
}

impl Env {
    pub(crate) fn new(mut id_gen: IdGenerator, mut vfs: Vfs) -> Self {
        let mut namespaces = FxHashMap::default();

        let user_namespace = Rc::new(RefCell::new(NamespaceInfo {
            path: Rc::new(PathBuf::from("__user")),
            values: FxHashMap::default(),
            types: FxHashMap::default(),
        }));

        namespaces.insert(PathBuf::from("__user"), user_namespace.clone());

        let mut fs_values = FxHashMap::default();
        fs_values.insert(
            SymbolName {
                text: "write_file".to_owned(),
            },
            Value::new(Value_::BuiltinFunction(
                BuiltinFunctionKind::WriteFile,
                None,
                None,
            )),
        );

        let fs_namespace = Rc::new(RefCell::new(NamespaceInfo {
            path: Rc::new(PathBuf::from("__fs.gdn")),
            values: fs_values,
            types: FxHashMap::default(),
        }));

        namespaces.insert(PathBuf::from("__fs.gdn"), fs_namespace);

        let builtins_path = Rc::new(PathBuf::from("__builtins.gdn"));
        let builtins_src = include_str!("__builtins.gdn");
        let builtins_vfs_path = vfs.insert(builtins_path.clone(), builtins_src.to_owned());

        let mut path_methods = FxHashMap::default();
        path_methods.insert(
            SymbolName {
                text: "exists".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(&builtins_vfs_path),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(&builtins_vfs_path),
                        name: TypeName {
                            text: "Path".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(&builtins_vfs_path),
                },
                receiver_sym: Symbol::new(
                    Position::todo(&builtins_vfs_path),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(&builtins_vfs_path), "exists", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathExists, None),
            },
        );
        path_methods.insert(
            SymbolName {
                text: "read".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(&builtins_vfs_path),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(&builtins_vfs_path),
                        name: TypeName {
                            text: "Path".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(&builtins_vfs_path),
                },
                receiver_sym: Symbol::new(
                    Position::todo(&builtins_vfs_path),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(&builtins_vfs_path), "read", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathRead, None),
            },
        );

        let types = built_in_types(&builtins_vfs_path, &mut id_gen);

        let temp_prelude = Rc::new(RefCell::new(NamespaceInfo {
            path: Rc::new(PathBuf::from("__prelude")),
            values: FxHashMap::default(),
            types: FxHashMap::default(),
        }));

        let mut env = Self {
            tests: FxHashMap::default(),
            types,
            prelude_namespace: temp_prelude,
            namespaces,
            project_root: std::env::current_dir().unwrap_or(PathBuf::from("/")),
            prev_call_args: FxHashMap::default(),
            prev_method_call_args: FxHashMap::default(),
            stack: Stack::new(user_namespace.clone()),
            ticks: 0,
            tick_limit: None,
            enforce_sandbox: false,
            stop_at_expr_id: None,
            id_gen,
            vfs,
            initial_state: None,
            cli_args: vec![],
        };

        let prelude_namespace = fresh_prelude(&mut env, &builtins_vfs_path, builtins_src);
        insert_prelude(user_namespace.clone(), prelude_namespace.clone());

        env.prelude_namespace = prelude_namespace;

        env.initial_state = Some(Box::new(env.clone()));

        env
    }

    pub(crate) fn get_or_create_namespace(&mut self, path: &Path) -> Rc<RefCell<NamespaceInfo>> {
        if let Some(ns) = self.namespaces.get(path) {
            return ns.clone();
        }

        let ns = Rc::new(RefCell::new(NamespaceInfo {
            path: Rc::new(path.to_owned()),
            values: FxHashMap::default(),
            types: FxHashMap::default(),
        }));

        insert_prelude(ns.clone(), self.prelude_namespace.clone());

        self.namespaces.insert(path.to_owned(), ns.clone());
        ns
    }

    pub(crate) fn top_frame_name(&self) -> String {
        let top_stack = self.stack.0.last().unwrap();
        match &top_stack.enclosing_name {
            EnclosingSymbol::Fun(symbol) => format!("{}", symbol.name),
            EnclosingSymbol::Method(type_name, symbol) => format!("{}::{}", type_name, symbol.name),
            EnclosingSymbol::Test(symbol) => format!("test {}", symbol.name),
            EnclosingSymbol::Closure => "closure".to_owned(),
            EnclosingSymbol::Toplevel => "TOP".to_owned(),
        }
    }

    pub(crate) fn add_method(&mut self, method_info: &MethodInfo) {
        let type_name = method_info.receiver_hint.sym.name.clone();
        if let Some(type_and_methods) = self.types.get_mut(&type_name) {
            type_and_methods
                .methods
                .insert(method_info.name_sym.name.clone(), method_info.clone());
        }
    }

    /// Get the type definition associated with this `name`.
    ///
    /// This handles global type definitions, and type variables are
    /// not considered here.
    pub(crate) fn get_type_def<'a>(&'a self, name: &TypeName) -> Option<&'a TypeDef> {
        self.types.get(name).map(|td| &td.def)
    }

    pub(crate) fn all_types(&self) -> Vec<TypeName> {
        self.types.keys().cloned().collect()
    }

    pub(crate) fn add_type(&mut self, name: TypeName, type_: TypeDef) {
        self.types.insert(
            name.clone(),
            TypeDefAndMethods {
                def: type_.clone(),
                methods: FxHashMap::default(),
            },
        );
    }

    pub(crate) fn push_expr_to_eval(&mut self, state: ExpressionState, expr: Rc<Expression>) {
        let stack_frame = self.stack.0.last_mut().unwrap();
        stack_frame.exprs_to_eval.push((state, expr));
    }

    pub(crate) fn push_value(&mut self, value: Value) {
        let stack_frame = self.stack.0.last_mut().unwrap();
        stack_frame.evalled_values.push(value);
    }

    pub(crate) fn pop_value(&mut self) -> Option<Value> {
        let stack_frame = self.stack.0.last_mut().unwrap();
        stack_frame.evalled_values.pop()
    }

    pub(crate) fn current_namespace(&self) -> Rc<RefCell<NamespaceInfo>> {
        self.current_frame().namespace.clone()
    }

    pub(crate) fn current_frame(&self) -> &StackFrame {
        self.stack.0.last().unwrap()
    }

    pub(crate) fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.0.last_mut().unwrap()
    }
}

fn built_in_types(
    builtins_vfs_path: &VfsPathBuf,
    id_gen: &mut IdGenerator,
) -> FxHashMap<TypeName, TypeDefAndMethods> {
    let mut types = FxHashMap::default();
    // TODO: String literals are duplicated with type_representation.
    types.insert(
        TypeName { text: "Int".into() },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::Int, None),
            methods: FxHashMap::default(),
        },
    );

    let mut string_methods = FxHashMap::default();
    string_methods.insert(
        SymbolName {
            text: "index_of".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "String".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "index_of", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringIndexOf, None),
        },
    );
    string_methods.insert(
        SymbolName {
            text: "len".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "String".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "len", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLen, None),
        },
    );
    string_methods.insert(
        SymbolName {
            text: "lines".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "String".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "lines", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLines, None),
        },
    );
    string_methods.insert(
        SymbolName {
            text: "substring".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "String".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "substring", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringSubstring, None),
        },
    );

    types.insert(
        TypeName {
            text: "String".into(),
        },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::String, None),
            methods: string_methods,
        },
    );

    let mut list_methods = FxHashMap::default();
    list_methods.insert(
        SymbolName {
            text: "append".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "List".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "append", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListAppend, None),
        },
    );
    list_methods.insert(
        SymbolName {
            text: "contains".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "List".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "contains", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListContains, None),
        },
    );
    list_methods.insert(
        SymbolName {
            text: "len".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "List".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "len", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListLen, None),
        },
    );
    list_methods.insert(
        SymbolName {
            text: "get".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "List".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "get", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListGet, None),
        },
    );

    types.insert(
        TypeName {
            text: "List".into(),
        },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::List, None),
            methods: list_methods,
        },
    );
    types.insert(
        TypeName {
            text: "Tuple".into(),
        },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::Tuple, None),
            methods: FxHashMap::default(),
        },
    );
    types.insert(
        TypeName { text: "Fun".into() },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::Fun, None),
            methods: FxHashMap::default(),
        },
    );
    types.insert(
        TypeName {
            text: "Namespace".into(),
        },
        TypeDefAndMethods {
            def: TypeDef::Builtin(BuiltinType::Namespace, None),
            methods: FxHashMap::default(),
        },
    );

    types
}

fn insert_prelude(ns: Rc<RefCell<NamespaceInfo>>, prelude: Rc<RefCell<NamespaceInfo>>) {
    let mut ns = ns.borrow_mut();

    for (name, value) in prelude.borrow().values.iter() {
        ns.values.insert(name.clone(), value.clone());
    }
    for (name, type_) in prelude.borrow().types.iter() {
        ns.types.insert(name.clone(), type_.clone());
    }
}

// TODO: this shouldn't take an Env, we're in the process of constructing it.
fn fresh_prelude(
    env: &mut Env,
    builtins_vfs_path: &VfsPathBuf,
    builtins_src: &str,
) -> Rc<RefCell<NamespaceInfo>> {
    let id_gen = &mut env.id_gen;
    let vfs = &mut env.vfs;

    let mut path_methods = FxHashMap::default();

    path_methods.insert(
        SymbolName {
            text: "exists".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "Path".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "exists", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathExists, None),
        },
    );
    path_methods.insert(
        SymbolName {
            text: "read".to_owned(),
        },
        MethodInfo {
            pos: Position::todo(builtins_vfs_path),
            receiver_hint: TypeHint {
                args: vec![],
                sym: TypeSymbol {
                    position: Position::todo(builtins_vfs_path),
                    name: TypeName {
                        text: "Path".into(),
                    },
                    id: id_gen.next(),
                },
                position: Position::todo(builtins_vfs_path),
            },
            receiver_sym: Symbol::new(Position::todo(builtins_vfs_path), "__irrelevant", id_gen),
            name_sym: Symbol::new(Position::todo(builtins_vfs_path), "read", id_gen),
            kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathRead, None),
        },
    );

    let prelude_path = Rc::new(PathBuf::from("__prelude.gdn"));
    let prelude_src = include_str!("__prelude.gdn");
    let prelude_vfs_path = vfs.insert(prelude_path.clone(), prelude_src.to_owned());

    let mut values = FxHashMap::default();

    // Insert all the built-in functions.
    for fun_kind in BuiltinFunctionKind::iter() {
        let namespace_file = fun_kind.namespace_path();
        if namespace_file != *prelude_path {
            continue;
        }

        values.insert(
            SymbolName {
                text: format!("{}", fun_kind),
            },
            Value::new(Value_::BuiltinFunction(fun_kind, None, None)),
        );
    }

    // Insert built-in namespaces.
    let fs_namespace = env.namespaces.get(&PathBuf::from("__fs.gdn")).unwrap();
    values.insert(
        SymbolName {
            text: "fs".to_owned(),
        },
        Value::new(Value_::Namespace(fs_namespace.clone())),
    );

    let ns_info = NamespaceInfo {
        path: prelude_path,
        values,
        types: built_in_types(builtins_vfs_path, id_gen),
    };

    let (prelude_items, errors) = parse_toplevel_items(&prelude_vfs_path, prelude_src, id_gen);
    assert!(
        errors.is_empty(),
        "Prelude should be syntactically legal: {}",
        errors.first().unwrap().position().as_ide_string()
    );

    let (builtin_items, errors) = parse_toplevel_items(builtins_vfs_path, builtins_src, id_gen);
    assert!(
        errors.is_empty(),
        "Stubs for built-ins should be syntactically legal: {}",
        errors.first().unwrap().position().as_ide_string()
    );

    let ns = Rc::new(RefCell::new(ns_info));

    load_toplevel_items(&prelude_items, env, ns.clone());
    load_toplevel_items(&builtin_items, env, ns.clone());

    if let Some(path_def_and_methods) = env.types.get_mut(&TypeName {
        text: "Path".into(),
    }) {
        path_def_and_methods.methods.extend(path_methods);
    }

    ns
}

#[derive(Debug, Clone)]
pub(crate) struct StackFrame {
    /// The namespace we're currently in, where the callee is defined.
    pub(crate) namespace: Rc<RefCell<NamespaceInfo>>,

    /// The name of the function, method or test that we're evaluating.
    pub(crate) enclosing_name: EnclosingSymbol,
    /// Used to check the type of the returned value.
    pub(crate) return_hint: Option<TypeHint>,
    /// The position of the call site.
    pub(crate) caller_pos: Option<Position>,
    /// The ID of the call site expression.
    pub(crate) caller_expr_id: Option<SyntaxId>,
    /// Does the call site use the return value? If this is false,
    /// we're only called for side effects.
    pub(crate) caller_uses_value: bool,
    pub(crate) bindings: Bindings,
    /// Types bound in this stack frame, due to generic parameters.
    pub(crate) type_bindings: TypeVarEnv,
    /// If we are entering a block with extra bindings that are only
    /// defined for the duration of the block, pass them here.
    ///
    /// For example:
    /// ```garden
    /// match x { Some(y) => { y + 1 } _ => {}}
    /// ```
    ///
    /// We want `y` to be bound, but only in the block.
    pub(crate) bindings_next_block: Vec<(Symbol, Value)>,
    /// A stack of expressions to evaluate.
    pub(crate) exprs_to_eval: Vec<(ExpressionState, Rc<Expression>)>,
    /// The values of subexpressions that we've evaluated so far.
    pub(crate) evalled_values: Vec<Value>,
}
