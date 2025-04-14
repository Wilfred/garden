use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;
use strum::IntoEnumIterator;

use crate::eval::{EnclosingSymbol, ExpressionState};
use crate::garden_type::TypeVarEnv;
use crate::namespaces::NamespaceInfo;
use crate::values::{BuiltinFunctionKind, Value, Value_};
use crate::{
    eval::{load_toplevel_items, Bindings},
    types::{BuiltinType, TypeDef},
};
use garden_lang_parser::ast::{
    BuiltinMethodKind, Expression, IdGenerator, MethodInfo, MethodKind, Symbol, SymbolName,
    SyntaxId, TestInfo, TypeHint, TypeName, TypeSymbol, Vfs,
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
            return_hint: None,
            enclosing_name: EnclosingSymbol::Toplevel,
            type_bindings: FxHashMap::default(),
            caller_uses_value: true,
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
            return FxHashMap::default();
        };

        stack_frame.type_bindings.clone()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Env {
    pub(crate) file_scope: FxHashMap<SymbolName, Value>,
    pub(crate) methods: FxHashMap<TypeName, FxHashMap<SymbolName, MethodInfo>>,
    pub(crate) tests: FxHashMap<SymbolName, TestInfo>,
    pub(crate) types: FxHashMap<TypeName, TypeDef>,
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
        let mut file_scope = FxHashMap::default();

        // Insert all the built-in functions.
        for fun_kind in BuiltinFunctionKind::iter() {
            file_scope.insert(
                SymbolName {
                    text: format!("{}", fun_kind),
                },
                Value::new(Value_::BuiltinFunction(fun_kind, None)),
            );
        }

        let mut prelude_values = FxHashMap::default();
        prelude_values.insert(
            SymbolName {
                text: "print".to_owned(),
            },
            Value::new(Value_::BuiltinFunction(BuiltinFunctionKind::Print, None)),
        );

        let prelude_namespace = Value::new(Value_::Namespace(NamespaceInfo {
            name: "prelude".to_owned(),
            values: prelude_values,
            types: FxHashMap::default(),
        }));
        file_scope.insert(
            SymbolName {
                text: "prelude".to_owned(),
            },
            prelude_namespace,
        );

        let mut fs_values = FxHashMap::default();
        fs_values.insert(
            SymbolName {
                text: "write_file".to_owned(),
            },
            Value::new(Value_::BuiltinFunction(
                BuiltinFunctionKind::WriteFile,
                None,
            )),
        );

        let fs_namespace = Value::new(Value_::Namespace(NamespaceInfo {
            name: "fs".to_owned(),
            values: fs_values,
            types: FxHashMap::default(),
        }));
        file_scope.insert(
            SymbolName {
                text: "fs".to_owned(),
            },
            fs_namespace,
        );

        let mut methods: FxHashMap<TypeName, FxHashMap<SymbolName, MethodInfo>> =
            FxHashMap::default();

        let builtins_path = Rc::new(PathBuf::from("builtins.gdn"));

        let mut path_methods = FxHashMap::default();
        path_methods.insert(
            SymbolName {
                text: "exists".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "Path".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "exists", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathExists, None),
            },
        );
        path_methods.insert(
            SymbolName {
                text: "read".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "Path".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "read", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::PathRead, None),
            },
        );

        methods.insert(
            TypeName {
                text: "Path".into(),
            },
            path_methods,
        );

        let mut string_methods = FxHashMap::default();
        string_methods.insert(
            SymbolName {
                text: "index_of".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "String".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "index_of",
                    &mut id_gen,
                ),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringIndexOf, None),
            },
        );
        string_methods.insert(
            SymbolName {
                text: "len".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "String".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "len", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLen, None),
            },
        );
        string_methods.insert(
            SymbolName {
                text: "lines".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "String".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "lines", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringLines, None),
            },
        );
        string_methods.insert(
            SymbolName {
                text: "substring".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "String".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "substring",
                    &mut id_gen,
                ),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::StringSubstring, None),
            },
        );

        methods.insert(
            TypeName {
                text: "String".into(),
            },
            string_methods,
        );

        let mut list_methods = FxHashMap::default();
        list_methods.insert(
            SymbolName {
                text: "append".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "List".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "append", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListAppend, None),
            },
        );
        list_methods.insert(
            SymbolName {
                text: "contains".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "List".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "contains",
                    &mut id_gen,
                ),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListContains, None),
            },
        );
        list_methods.insert(
            SymbolName {
                text: "len".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "List".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "len", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListLen, None),
            },
        );
        list_methods.insert(
            SymbolName {
                text: "get".to_owned(),
            },
            MethodInfo {
                pos: Position::todo(builtins_path.clone()),
                receiver_hint: TypeHint {
                    args: vec![],
                    sym: TypeSymbol {
                        position: Position::todo(builtins_path.clone()),
                        name: TypeName {
                            text: "List".into(),
                        },
                        id: id_gen.next(),
                    },
                    position: Position::todo(builtins_path.clone()),
                },
                receiver_sym: Symbol::new(
                    Position::todo(builtins_path.clone()),
                    "__irrelevant",
                    &mut id_gen,
                ),
                name_sym: Symbol::new(Position::todo(builtins_path.clone()), "get", &mut id_gen),
                kind: MethodKind::BuiltinMethod(BuiltinMethodKind::ListGet, None),
            },
        );

        methods.insert(
            TypeName {
                text: "List".into(),
            },
            list_methods,
        );

        // Insert all the built-in types.
        let mut types = FxHashMap::default();
        // TODO: String literals are duplicated with type_representation.
        types.insert(
            TypeName { text: "Int".into() },
            TypeDef::Builtin(BuiltinType::Int, None),
        );
        types.insert(
            TypeName {
                text: "String".into(),
            },
            TypeDef::Builtin(BuiltinType::String, None),
        );
        types.insert(
            TypeName {
                text: "List".into(),
            },
            TypeDef::Builtin(BuiltinType::List, None),
        );
        types.insert(
            TypeName {
                text: "Tuple".into(),
            },
            TypeDef::Builtin(BuiltinType::Tuple, None),
        );
        types.insert(
            TypeName { text: "Fun".into() },
            TypeDef::Builtin(BuiltinType::Fun, None),
        );
        types.insert(
            TypeName {
                text: "Namespace".into(),
            },
            TypeDef::Builtin(BuiltinType::Namespace, None),
        );

        let prelude_src = include_str!("prelude.gdn");
        let (prelude_items, errors) = parse_toplevel_items(
            &PathBuf::from("prelude.gdn"),
            prelude_src,
            &mut vfs,
            &mut id_gen,
        );
        assert!(
            errors.is_empty(),
            "Prelude should be syntactically legal: {}",
            errors.first().unwrap().position().as_ide_string()
        );

        let builtins_src = include_str!("builtins.gdn");
        let (builtin_items, errors) =
            parse_toplevel_items(&builtins_path, builtins_src, &mut vfs, &mut id_gen);
        assert!(
            errors.is_empty(),
            "Stubs for built-ins should be syntactically legal: {}",
            errors.first().unwrap().position().as_ide_string()
        );
        let mut env = Self {
            file_scope,
            methods,
            tests: FxHashMap::default(),
            types,
            prev_call_args: FxHashMap::default(),
            prev_method_call_args: FxHashMap::default(),
            stack: Stack::default(),
            ticks: 0,
            tick_limit: None,
            enforce_sandbox: false,
            stop_at_expr_id: None,
            id_gen,
            vfs,
            initial_state: None,
            cli_args: vec![],
        };

        load_toplevel_items(&prelude_items, &mut env);
        load_toplevel_items(&builtin_items, &mut env);

        env.initial_state = Some(Box::new(env.clone()));

        env
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

    pub(crate) fn current_frame(&self) -> &StackFrame {
        self.stack.0.last().unwrap()
    }

    pub(crate) fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.0.last_mut().unwrap()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct StackFrame {
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
