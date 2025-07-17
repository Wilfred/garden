use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};
use strum::IntoEnumIterator;

use crate::eval::{load_toplevel_items, Bindings, EnclosingSymbol, ExpressionState};
use crate::garden_type::TypeVarEnv;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    BuiltinMethodKind, Expression, IdGenerator, MethodInfo, MethodKind, StructInfo, Symbol,
    SymbolName, SyntaxId, TestInfo, TypeHint, TypeName, TypeSymbol, Visibility,
};
use crate::parser::parse_toplevel_items;
use crate::parser::position::Position;
use crate::parser::vfs::{to_abs_path, Vfs};
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

    /// A copy of the prelude. This contains the values inserted in
    /// every new namespace.
    pub(crate) prelude_namespace: Rc<RefCell<NamespaceInfo>>,

    /// Namespaces in Garden are defined by the file that own
    /// them. For every file, the corresponding namespace information.
    ///
    /// Note that the namespace information may not match the file on
    /// disk, if the user has loaded additional definitions or if the
    /// file has been changed after loading.
    pub(crate) namespaces: FxHashMap<PathBuf, Rc<RefCell<NamespaceInfo>>>,

    /// File paths relative to this directory will be shown as
    /// relative paths in e.g. errors.
    pub(crate) project_root: PathBuf,

    /// The arguments used the last time each function was
    /// called. Used for eval-up-to.
    pub(crate) prev_call_args: FxHashMap<(SymbolName, PathBuf), Vec<Value>>,
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
            abs_path: Rc::new(PathBuf::from("__user.gdn")),
            values: FxHashMap::default(),
            external_syms: FxHashSet::default(),
            types: FxHashMap::default(),
        }));

        namespaces.insert(PathBuf::from("__user.gdn"), user_namespace.clone());

        let prelude_path = Rc::new(PathBuf::from("__prelude.gdn"));
        let prelude_src = include_str!("__prelude.gdn");
        let prelude_vfs_path = vfs.insert(prelude_path.clone(), prelude_src.to_owned());

        let types = built_in_types(&prelude_vfs_path, &mut id_gen);

        let temp_prelude = Rc::new(RefCell::new(NamespaceInfo {
            abs_path: Rc::new(PathBuf::from("__prelude.gdn")),
            values: FxHashMap::default(),
            external_syms: FxHashSet::default(),
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

        let prelude_namespace = fresh_prelude(&mut env, &prelude_vfs_path);
        insert_prelude(user_namespace.clone(), prelude_namespace.clone());

        env.prelude_namespace = prelude_namespace;

        env.initial_state = Some(Box::new(env.clone()));

        env
    }

    pub(crate) fn get_namespace(&self, abs_path: &Path) -> Option<Rc<RefCell<NamespaceInfo>>> {
        let abs_path = canonicalize_namespace_path(abs_path);
        self.namespaces.get(&abs_path).cloned()
    }

    pub(crate) fn get_or_create_namespace(
        &mut self,
        abs_path: &Path,
    ) -> Rc<RefCell<NamespaceInfo>> {
        let abs_path = canonicalize_namespace_path(abs_path);

        if let Some(ns) = self.namespaces.get(&abs_path) {
            return ns.clone();
        }

        let mut values = FxHashMap::default();
        for fun_kind in BuiltinFunctionKind::iter() {
            if fun_kind.namespace_path() != abs_path {
                continue;
            }

            values.insert(
                SymbolName {
                    text: format!("{}", fun_kind),
                },
                Value::new(Value_::BuiltinFunction(fun_kind, None, None)),
            );
        }

        let ns = Rc::new(RefCell::new(NamespaceInfo {
            abs_path: Rc::new(abs_path.clone()),
            external_syms: FxHashSet::default(),
            values,
            types: FxHashMap::default(),
        }));

        insert_prelude(ns.clone(), self.prelude_namespace.clone());

        self.namespaces.insert(abs_path, ns.clone());
        ns
    }

    pub(crate) fn top_frame_name(&self) -> String {
        let top_stack = self.stack.0.last().unwrap();
        match &top_stack.enclosing_name {
            EnclosingSymbol::Fun(symbol) => format!("fun {}", symbol.name),
            EnclosingSymbol::Method(type_name, symbol) => format!("{}::{}", type_name, symbol.name),
            EnclosingSymbol::Test(symbol) => format!("test {}", symbol.name),
            EnclosingSymbol::Closure => "closure".to_owned(),
            EnclosingSymbol::Toplevel => {
                let ns = top_stack.namespace.borrow();
                ns.abs_path
                    .file_name()
                    .map(|name| name.to_string_lossy().to_string())
                    .unwrap_or_else(|| ns.abs_path.display().to_string())
            }
        }
    }

    pub(crate) fn add_method(&mut self, method_info: &MethodInfo, load_stubs: bool) {
        let type_name = method_info.receiver_hint.sym.name.clone();
        match self.types.get_mut(&type_name) {
            Some(type_and_methods) => {
                type_and_methods
                    .methods
                    .insert(method_info.name_sym.name.clone(), method_info.clone());
            }
            None => {
                if !load_stubs {
                    return;
                }

                // The user is loading a method without a
                // corresponding type behind defined. Define a stub so
                // we have somewhere to attach the method.
                //
                // This ensures that the method is available when the
                // user (presumably) defines their type.
                let stub_struct = TypeDef::Struct(StructInfo {
                    pos: method_info.receiver_hint.position.clone(),
                    visibility: Visibility::CurrentFile,
                    doc_comment: None,
                    name_sym: method_info.receiver_hint.sym.clone(),
                    type_params: vec![],
                    fields: vec![],
                });

                let mut type_and_methods = TypeDefAndMethods {
                    def: stub_struct,
                    methods: FxHashMap::default(),
                };

                type_and_methods
                    .methods
                    .insert(method_info.name_sym.name.clone(), method_info.clone());

                self.types.insert(type_name, type_and_methods);
            }
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

    pub(crate) fn add_type(
        &mut self,
        name: TypeName,
        type_: TypeDef,
        namespace: Rc<RefCell<NamespaceInfo>>,
    ) {
        let methods = match self.types.remove(&name) {
            Some(type_def_and_meths) => type_def_and_meths.methods,
            None => FxHashMap::default(),
        };

        self.types.insert(
            name.clone(),
            TypeDefAndMethods {
                def: type_.clone(),
                methods: methods.clone(),
            },
        );

        namespace.borrow_mut().types.insert(
            name.clone(),
            TypeDefAndMethods {
                def: type_,
                methods,
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

    /// If `path` is a subdirectory of the current project, convert it
    /// to a relative path. This is intended to keep error messages
    /// more concise.
    pub(crate) fn relative_to_project<'a>(&self, path: &'a Path) -> &'a Path {
        match path.strip_prefix(&self.project_root) {
            Ok(rel_path) => rel_path,
            Err(_) => path,
        }
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
fn fresh_prelude(env: &mut Env, prelude_vfs_path: &VfsPathBuf) -> Rc<RefCell<NamespaceInfo>> {
    let id_gen = &mut env.id_gen;

    // Users shouldn't be able to see private methods in a
    // namespace. Where should this be enforced? When the namespace in
    // constructed during eval?
    //
    // Private methods still need to be stored somewhere.
    //
    // Store a separate list of public methods for access?
    // Fundamentally namespace access during evaluation is different
    // from foo::bar() access from another file.

    let prelude_path = Rc::new(PathBuf::from("__prelude.gdn"));
    let prelude_src = include_str!("__prelude.gdn");

    let mut values = FxHashMap::default();

    // Insert all the built-in prelude functions.
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

    let ns_info = NamespaceInfo {
        abs_path: prelude_path,
        external_syms: FxHashSet::default(),
        values,
        types: built_in_types(prelude_vfs_path, id_gen),
    };

    let (prelude_items, errors) = parse_toplevel_items(prelude_vfs_path, prelude_src, id_gen);
    assert!(
        errors.is_empty(),
        "Prelude should be syntactically legal: {}",
        errors
            .first()
            .unwrap()
            .position()
            .as_ide_string(&env.project_root)
    );

    let ns = Rc::new(RefCell::new(ns_info));
    let (diags, _) = load_toplevel_items(&prelude_items, env, ns.clone());
    assert_eq!(
        diags.len(),
        0,
        "Loading the prelude should produce zero warnings and errors."
    );

    let path_methods = vec![
        ("exists", BuiltinMethodKind::PathExists),
        ("read", BuiltinMethodKind::PathRead),
    ];

    if let Some(path_def_and_methods) = env.types.get_mut(&TypeName {
        text: "Path".into(),
    }) {
        // Merge the builtin method kinds for Path into the stub
        // methods defined in the prelude.
        for (name, builtin_kind) in path_methods {
            let name = SymbolName {
                text: name.to_owned(),
            };

            if let Some(existing_meth) = path_def_and_methods.methods.get_mut(&name) {
                existing_meth.kind =
                    MethodKind::BuiltinMethod(builtin_kind, existing_meth.fun_info().cloned());
            }
        }
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

fn canonicalize_namespace_path(abs_path: &Path) -> PathBuf {
    // Canonicalise stdlib paths, which always start with __.
    //
    // We treat "foo/bar/__fs.gdn" as "__fs.gdn". This ensures we can
    // use refer to stdlib files directly (e.g. `import "__fs.gdn"`)
    // as well as pass them to the CLI (e.g. `garden check
    // src/__fs.gdn`).
    if let Some(name_osstr) = abs_path.file_name() {
        let name = name_osstr.to_string_lossy();
        if name.starts_with("__") {
            return PathBuf::from(name_osstr);
        }
    }

    to_abs_path(abs_path)
}
