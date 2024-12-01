use std::{collections::HashMap, path::PathBuf};

use strum::IntoEnumIterator;

use crate::eval::{EnclosingSymbol, ExpressionState};
use crate::garden_type::TypeVarEnv;
use crate::values::{BuiltinFunctionKind, Value};
use crate::{
    eval::{load_toplevel_items, Bindings, StackFrame},
    types::{BuiltinType, TypeDef},
};
use garden_lang_parser::ast::{
    BuiltinMethodKind, Expression, MethodInfo, MethodKind, SourceString, Symbol, SymbolName,
    SyntaxId, SyntaxIdGenerator, TestInfo, TypeHint, TypeName, TypeSymbol,
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
    pub(crate) types: HashMap<TypeName, TypeDef>,
    /// The arguments used the last time each function was
    /// called. Used for eval-up-to.
    pub(crate) prev_call_args: HashMap<SymbolName, Vec<Value>>,
    /// The receiver and arguments used the last time each method was
    /// called. Used for eval-up-to.
    pub(crate) prev_method_call_args: HashMap<TypeName, HashMap<SymbolName, (Value, Vec<Value>)>>,
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
}

impl Env {
    pub(crate) fn new(id_gen: &mut SyntaxIdGenerator) -> Self {
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "len", id_gen.next()),
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "substring", id_gen.next()),
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "append", id_gen.next()),
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "append", id_gen.next()),
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "len", id_gen.next()),
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
                        id: id_gen.next(),
                    },
                    position: Position::todo(),
                },
                receiver_sym: Symbol::new(Position::todo(), "__irrelevant", id_gen.next()),
                name_sym: Symbol::new(Position::todo(), "get", id_gen.next()),
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
            TypeDef::Builtin(BuiltinType::Int, None),
        );
        types.insert(
            TypeName {
                name: "String".into(),
            },
            TypeDef::Builtin(BuiltinType::String, None),
        );
        types.insert(
            TypeName {
                name: "List".into(),
            },
            TypeDef::Builtin(BuiltinType::List, None),
        );
        types.insert(
            TypeName {
                name: "Tuple".into(),
            },
            TypeDef::Builtin(BuiltinType::Tuple, None),
        );
        types.insert(
            TypeName { name: "Fun".into() },
            TypeDef::Builtin(BuiltinType::Fun, None),
        );

        let prelude_src = include_str!("prelude.gdn");
        let (prelude_items, errors) =
            parse_toplevel_items(&PathBuf::from("prelude.gdn"), prelude_src, id_gen);
        assert!(
            errors.is_empty(),
            "Prelude should be syntactically legal: {}",
            errors.first().unwrap().position().as_ide_string()
        );

        let builtins_src = include_str!("builtins.gdn");
        let (builtin_items, errors) =
            parse_toplevel_items(&PathBuf::from("builtins.gdn"), builtins_src, id_gen);
        assert!(
            errors.is_empty(),
            "Stubs for built-ins should be syntactically legal: {}",
            errors.first().unwrap().position().as_ide_string()
        );
        let mut env = Self {
            file_scope,
            methods,
            tests: HashMap::new(),
            types,
            prev_call_args: HashMap::new(),
            prev_method_call_args: HashMap::new(),
            stack: Stack::default(),
            ticks: 0,
            tick_limit: None,
            enforce_sandbox: false,
            stop_at_expr_id: None,
        };

        load_toplevel_items(&prelude_items, &mut env);
        load_toplevel_items(&builtin_items, &mut env);

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

    pub(crate) fn push_expr_to_eval(&mut self, state: ExpressionState, expr: Expression) {
        let stack_frame = self.stack.0.last_mut().unwrap();
        stack_frame.exprs_to_eval.push((state, expr));
    }

    pub(crate) fn push_evalled(&mut self, value: Value) {
        let stack_frame = self.stack.0.last_mut().unwrap();
        stack_frame.evalled_values.push(value);
    }

    pub(crate) fn current_frame(&self) -> &StackFrame {
        self.stack.0.last().unwrap()
    }

    pub(crate) fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.0.last_mut().unwrap()
    }
}
