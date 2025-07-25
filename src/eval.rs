// Used in some TODO that eventually should handle Err properly.
#![allow(clippy::manual_flatten)]

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use normalize_path::NormalizePath as _;
use ordered_float::OrderedFloat;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::checks::{check_toplevel_items, check_toplevel_items_in_env};
use crate::diagnostics::{Diagnostic, Severity};
use crate::env::{Env, StackFrame};
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy};
use crate::json_session::{print_as_json, Response, ResponseKind};
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    AssignUpdateKind, AstId, BinaryOperatorKind, Block, BuiltinMethodKind, EnumInfo, Expression,
    ExpressionWithComma, Expression_, FunInfo, IdGenerator, ImportInfo, InternedSymbolId,
    LetDestination, MethodInfo, MethodKind, ParenthesizedArguments, ParenthesizedParameters,
    Pattern, StructInfo, Symbol, SymbolName, SymbolWithHint, SyntaxId, TestInfo, ToplevelItem,
    TypeHint, TypeName, TypeSymbol, Visibility,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::vfs::{Vfs, VfsPathBuf};
use crate::parser::{lex, parse_toplevel_items, placeholder_symbol};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::{TypeDef, TypeDefAndMethods};
use crate::values::{type_representation, BuiltinFunctionKind, Value, Value_};
use crate::{msgcode, msgtext};

/// Bindings in a single block. For example, `x` is only bound inside
/// the if block here.
///
/// ```garden
/// if y { let x = 1 }
/// ```
#[derive(Debug, Clone, Default)]
pub(crate) struct BlockBindings {
    /// Values bound in this block, such as local variables or
    /// function parameters.
    pub(crate) values: FxHashMap<InternedSymbolId, Value>,
}

/// Use reference equality for block bindings, so closures have
/// reference equality.
impl PartialEq for BlockBindings {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for BlockBindings {}

/// The bindings in the current scope. This is a vec of block
/// bindings, because exiting a block will remove some bindings.
#[derive(Debug, Clone)]
pub(crate) struct Bindings {
    pub(crate) block_bindings: Vec<BlockBindings>,
}

impl Bindings {
    pub(crate) fn push_block(&mut self) {
        self.block_bindings.push(BlockBindings::default());
    }

    pub(crate) fn pop_block(&mut self) {
        self.block_bindings.pop();
        assert!(!self.block_bindings.is_empty());
    }

    fn new_with(outer_scope: FxHashMap<InternedSymbolId, Value>) -> Self {
        Self {
            block_bindings: vec![BlockBindings {
                values: outer_scope,
            }],
        }
    }

    fn get(&self, interned_id: InternedSymbolId) -> Option<Value> {
        // TODO: this allows shadowing. Is that desirable -- does it
        // make REPL workflows less convenient when it's harder to inspect?
        //
        // (Probably not, as long as users can inspect everything.)
        for block_bindings in self.block_bindings.iter().rev() {
            if let Some(value) = block_bindings.values.get(&interned_id) {
                return Some(value.clone());
            }
        }
        None
    }

    pub(crate) fn has(&self, interned_id: InternedSymbolId) -> bool {
        self.get(interned_id).is_some()
    }

    /// Remove `sym` from bindings. If this variable is shadowed,
    /// remove the innermost binding.
    pub(crate) fn remove(&mut self, interned_id: InternedSymbolId) {
        for block_bindings in self.block_bindings.iter_mut().rev() {
            if block_bindings.values.contains_key(&interned_id) {
                block_bindings.values.remove(&interned_id);
            }
        }
    }

    fn add_new(&mut self, sym: &Symbol, value: Value) {
        if sym.name.is_underscore() {
            return;
        }

        let block_bindings = self
            .block_bindings
            .last_mut()
            .expect("Vec of bindings should always be non-empty");
        block_bindings.values.insert(sym.interned_id, value);
    }

    fn set_existing(&mut self, sym: &Symbol, value: Value) {
        for block_bindings in self.block_bindings.iter_mut().rev() {
            if let Entry::Occupied(mut e) = block_bindings.values.entry(sym.interned_id) {
                e.insert(value);
                return;
            }
        }
        unreachable!()
    }

    pub(crate) fn all(&self) -> Vec<(InternedSymbolId, Value)> {
        let mut res = vec![];
        for block_bindings in self.block_bindings.iter().rev() {
            for (k, v) in block_bindings.values.iter() {
                res.push((*k, v.clone()));
            }
        }

        res
    }
}

impl Default for Bindings {
    fn default() -> Self {
        Self {
            block_bindings: vec![BlockBindings::default()],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EnclosingSymbol {
    Fun(Symbol),
    Method(TypeName, Symbol),
    Test(Symbol),
    Closure,
    Toplevel,
}

impl std::fmt::Display for EnclosingSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnclosingSymbol::Fun(fun_sym) => write!(f, "fun {}()", fun_sym.name),
            EnclosingSymbol::Method(type_name, meth_sym) => {
                write!(f, "method {}(this: {})()", meth_sym.name, type_name.text)
            }
            EnclosingSymbol::Test(test_sym) => write!(f, "test {}", test_sym.name),
            EnclosingSymbol::Closure => write!(f, "closure"),
            EnclosingSymbol::Toplevel => write!(f, "__toplevel__"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExpressionState {
    /// This expression has not been evaluated at all.
    NotEvaluated,
    /// We have evaluated some of the subexpressions, but not
    /// all. This occurs in conditionally evaluated expressions,
    /// e.g. in `while foo() { bar() }` we have evaluated `foo()` but
    /// not yet `bar()`.
    PartiallyEvaluated,
    /// This expression has had its children evaluated, but hasn't
    /// been evaluated itself. For example, in `foo(bar())` we have
    /// evaluated `bar()` but not yet called `foo()` with the result.
    EvaluatedAllSubexpressions,
}

impl ExpressionState {
    pub(crate) fn done_children(&self) -> bool {
        matches!(self, ExpressionState::EvaluatedAllSubexpressions)
    }
}

pub(crate) fn most_similar(available: &[&SymbolName], name: &SymbolName) -> Option<SymbolName> {
    let mut res: Vec<_> = available.iter().collect();
    res.sort_by_key(|n| OrderedFloat(strsim::sorensen_dice(&n.text, &name.text)));

    if let Some(closest) = res.last() {
        if strsim::sorensen_dice(&closest.text, &name.text) > 0.4 {
            return Some((**closest).clone());
        }
    }

    None
}

fn most_similar_var(name: &SymbolName, env: &Env) -> Option<SymbolName> {
    let ns = env.current_namespace();
    let ns = ns.borrow();
    let file_level_names: Vec<_> = ns.values.keys().collect();

    // TODO: suggest local variables too.
    most_similar(&file_level_names, name)
}

fn get_var(sym: &Symbol, env: &Env) -> Option<Value> {
    if let Some(value) = env.current_frame().bindings.get(sym.interned_id) {
        return Some(value.clone());
    }

    let ns = env.current_namespace();
    if let Some(value) = ns.borrow().values.get(&sym.name) {
        return Some(value.clone());
    }

    None
}

#[derive(Debug)]
pub(crate) enum StdoutMode {
    WriteDirectly,
    WriteJson,
    DoNotWrite,
}

#[derive(Debug)]
pub(crate) struct Session {
    pub(crate) interrupted: Arc<AtomicBool>,
    /// Whether `print()` should write to stdout directly, or if we
    /// should write a JSON message to stdout summarising the print.
    pub(crate) stdout_mode: StdoutMode,
    pub(crate) start_time: Instant,
    pub(crate) trace_exprs: bool,
    pub(crate) pretty_print_json: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum EvalError {
    /// User pressed ctrl-c.
    Interrupted,
    /// Normal userland error, e.g. `throw()` was called. This cannot
    /// be handled in normal code (unlike Result), but may be resumed
    /// within the IDE (i.e the stack is not unwound).
    Exception(Position, ErrorMessage),
    /// `assert()` failed.
    AssertionFailed(Position, ErrorMessage),
    /// Ran out of ticks (i.e. program did not terminate in time).
    ReachedTickLimit(Position),
    /// Tried to execute a function that isn't permitted in the
    /// sandbox.
    ForbiddenInSandbox(Position),
}

#[derive(Debug)]
pub(crate) struct ToplevelEvalSummary {
    pub(crate) values: Vec<Value>,
    pub(crate) new_syms: Vec<SymbolName>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    /// Which tests were run, and the error if they failed.
    pub(crate) tests: Vec<(Symbol, Option<EvalError>)>,
}

/// Load, but do not evaluate, `items`.
///
/// Function definitions and method definitions are inserted into the
/// environment, but tests are not executed and toplevel expressions
/// are skipped.
pub(crate) fn load_toplevel_items(
    items: &[ToplevelItem],
    env: &mut Env,
    namespace: Rc<RefCell<NamespaceInfo>>,
) -> (Vec<Diagnostic>, Vec<SymbolName>) {
    let mut paths_seen = FxHashSet::default();
    load_toplevel_items_(items, env, &mut paths_seen, namespace, false)
}

pub(crate) fn load_toplevel_items_with_stubs(
    items: &[ToplevelItem],
    env: &mut Env,
    namespace: Rc<RefCell<NamespaceInfo>>,
) -> (Vec<Diagnostic>, Vec<SymbolName>) {
    let mut paths_seen = FxHashSet::default();
    load_toplevel_items_(items, env, &mut paths_seen, namespace, true)
}

fn load_toplevel_items_(
    items: &[ToplevelItem],
    env: &mut Env,
    paths_seen: &mut FxHashSet<PathBuf>,
    namespace: Rc<RefCell<NamespaceInfo>>,
    load_stubs: bool,
) -> (Vec<Diagnostic>, Vec<SymbolName>) {
    let mut diagnostics: Vec<Diagnostic> = vec![];
    let mut new_syms: Vec<SymbolName> = vec![];

    let mut enum_infos: Vec<&EnumInfo> = vec![];

    // Load types first, so we known which types are defined when we
    // start loading methods.
    let mut items = items.to_vec();
    items.sort_by_key(|item| match item {
        ToplevelItem::Enum(_) | ToplevelItem::Struct(_) => 0,
        _ => 1,
    });

    for item in &items {
        match &item {
            ToplevelItem::Fun(name_symbol, fun_info, visibility) => {
                if is_builtin_stub(fun_info) {
                    update_builtin_fun_info(fun_info, env, namespace.clone(), &mut diagnostics);
                } else {
                    let runtime_type =
                        Type::from_fun_info(fun_info, &env.types, &env.stack.type_bindings())
                            .unwrap_or_err_ty();
                    namespace.borrow_mut().values.insert(
                        name_symbol.name.clone(),
                        Value::new(Value_::Fun {
                            name_sym: name_symbol.clone(),
                            fun_info: fun_info.clone(),
                            runtime_type,
                        }),
                    );
                }

                match visibility {
                    Visibility::External(_) => {
                        namespace
                            .borrow_mut()
                            .external_syms
                            .insert(name_symbol.name.clone());
                    }
                    Visibility::CurrentFile => {
                        namespace
                            .borrow_mut()
                            .external_syms
                            .remove(&name_symbol.name);
                    }
                }

                new_syms.push(name_symbol.name.clone());
            }
            ToplevelItem::Method(meth_info, _) => {
                if let MethodKind::UserDefinedMethod(fun_info) = &meth_info.kind {
                    if is_builtin_stub(fun_info) {
                        update_builtin_meth_info(meth_info, fun_info, env, &mut diagnostics);
                    } else {
                        env.add_method(meth_info, load_stubs);
                    }
                }

                new_syms.push(SymbolName {
                    text: meth_info.full_name(),
                });
            }
            ToplevelItem::Test(test) => {
                env.tests.insert(test.name_sym.name.clone(), test.clone());
            }
            ToplevelItem::Enum(enum_info) => {
                // Add the enum definition to the type environment.
                env.add_type(
                    enum_info.name_sym.name.clone(),
                    TypeDef::Enum(enum_info.clone()),
                    namespace.clone(),
                );

                enum_infos.push(enum_info);

                let name_as_sym = SymbolName {
                    text: enum_info.name_sym.name.text.clone(),
                };
                new_syms.push(name_as_sym);
            }
            ToplevelItem::Struct(struct_info) => {
                if is_builtin_type(struct_info) {
                    update_builtin_type_info(struct_info, env, &mut diagnostics, namespace.clone());
                } else {
                    // Add the struct definition to the type environment.
                    env.add_type(
                        struct_info.name_sym.name.clone(),
                        TypeDef::Struct(struct_info.clone()),
                        namespace.clone(),
                    );
                }

                let name_as_sym = SymbolName {
                    text: struct_info.name_sym.name.text.clone(),
                };
                new_syms.push(name_as_sym);
            }
            ToplevelItem::Expr(_) => {}
            ToplevelItem::Block(_) => {}
            ToplevelItem::Import(import_info) => {
                let position = item.position();
                let Some(enclosing_dir) = position.path.parent() else {
                    diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "Could not find parent directory of `{}`.",
                            import_info.path.display()
                        ))]),
                        position,
                        notes: vec![],
                        severity: Severity::Error,
                    });

                    continue;
                };

                let abs_path = if import_info.path.display().to_string().starts_with("__") {
                    // TODO: warn on user trying to name files with
                    // two underscores.
                    import_info.path.to_owned()
                } else {
                    let norm_path = enclosing_dir.join(&import_info.path).normalize();
                    let Ok(abs_path) = std::path::absolute(&norm_path) else {
                        let current_dir_descr = match std::env::current_dir() {
                            Ok(d) => format!(" (currently {})", d.display()),
                            Err(_) => "".to_owned(),
                        };

                        diagnostics.push(Diagnostic {
                            message: ErrorMessage(vec![msgtext!(
                                "Could not convert `{}` to an absolute path. The working directory{} may not exist.",
                                import_info.path.display(),
                                current_dir_descr
                            )]),
                            position: import_info.path_pos.clone(),
                            notes: vec![],
                            severity: Severity::Error,
                        });

                        continue;
                    };

                    abs_path
                };

                if paths_seen.contains(&abs_path) {
                    // Already loaded this file, so we have a cyclic
                    // import. We don't need to load the namespace
                    // again, but we do need to still add the values
                    // to the current namespace.
                    let imported_ns = env.get_namespace(&abs_path).unwrap();
                    insert_imported_namespace(
                        import_info.namespace_sym.as_ref(),
                        namespace.clone(),
                        imported_ns,
                    );

                    continue;
                }
                paths_seen.insert(abs_path.clone());

                let src = match read_src(&abs_path, import_info) {
                    Ok(src) => src,
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        continue;
                    }
                };

                let vfs_path = env.vfs.insert(Rc::new(abs_path.clone()), src.clone());

                let (imported_items, parse_errors) =
                    parse_toplevel_items(&vfs_path, &src, &mut env.id_gen);
                if !parse_errors.is_empty() {
                    for error in parse_errors {
                        diagnostics.push(Diagnostic {
                            message: ErrorMessage(vec![Text(error.message().as_string())]),
                            position: error.position().clone(),
                            notes: vec![],
                            severity: Severity::Error,
                        });
                    }
                    continue;
                }

                // Load into its own namespace.
                let destination_ns = env.get_or_create_namespace(&abs_path);
                let (import_diagnostics, _) = load_toplevel_items_(
                    &imported_items,
                    env,
                    paths_seen,
                    destination_ns.clone(),
                    load_stubs,
                );

                let imported_syms = insert_imported_namespace(
                    import_info.namespace_sym.as_ref(),
                    namespace.clone(),
                    destination_ns,
                );

                diagnostics.extend(import_diagnostics);
                new_syms.extend(imported_syms);
            }
        }
    }

    // We've loaded all the toplevel items, so we can now create enum
    // constructor values knowing that all types have been loaded.
    for enum_info in enum_infos {
        // Add the values in the enum to the value environment.
        for (variant_idx, variant_sym) in enum_info.variants.iter().enumerate() {
            let enum_value = if variant_sym.payload_hint.is_some() {
                let runtime_type = enum_constructor_type(
                    env,
                    enum_info,
                    variant_sym.payload_hint.as_ref().unwrap(),
                );
                Value::new(Value_::EnumConstructor {
                    type_name: enum_info.name_sym.name.clone(),
                    variant_idx,
                    runtime_type,
                })
            } else {
                let runtime_type =
                    enum_value_runtime_type(env, &enum_info.name_sym.name, variant_idx, &Type::Top)
                        .unwrap_or(Type::no_value());

                Value::new(Value_::EnumVariant {
                    type_name: enum_info.name_sym.name.clone(),
                    runtime_type,
                    variant_idx,
                    payload: None,
                })
            };

            // TODO: warn if we're clobbering a name from a
            // different enum (i.e. not just redefining the
            // current enum).
            namespace
                .borrow_mut()
                .values
                .insert(variant_sym.name_sym.name.clone(), enum_value);
        }
    }

    (diagnostics, new_syms)
}

/// Insert `imported_ns` into `current_ns`, either as a single value
/// or as all the external symbols as values.
fn insert_imported_namespace(
    namespace_sym: Option<&Symbol>,
    current_ns: Rc<RefCell<NamespaceInfo>>,
    imported_ns: Rc<RefCell<NamespaceInfo>>,
) -> Vec<SymbolName> {
    match namespace_sym {
        Some(namespace_sym) => {
            let v = Value::new(Value_::Namespace(imported_ns));
            current_ns
                .borrow_mut()
                .values
                .insert(namespace_sym.name.clone(), v);

            vec![namespace_sym.name.clone()]
        }
        None => {
            let mut syms = vec![];

            // Load all the external items into the current namespace.
            let imported_ns = imported_ns.borrow();
            for (sym, value) in &imported_ns.values {
                if imported_ns.external_syms.contains(sym) {
                    current_ns
                        .borrow_mut()
                        .values
                        .insert(sym.clone(), value.clone());
                    syms.push(sym.clone());
                }
            }

            syms
        }
    }
}

fn read_src(abs_path: &Path, import_info: &ImportInfo) -> Result<String, Diagnostic> {
    if import_info.path == PathBuf::from("__prelude.gdn") {
        return Ok(include_str!("__prelude.gdn").to_owned());
    } else if import_info.path == PathBuf::from("__fs.gdn") {
        return Ok(include_str!("__fs.gdn").to_owned());
    } else if import_info.path == PathBuf::from("__garden.gdn") {
        return Ok(include_str!("__garden.gdn").to_owned());
    }

    let src_bytes = match std::fs::read(abs_path) {
        Ok(src_bytes) => src_bytes,
        Err(e) => {
            return Err(Diagnostic {
                // TODO: pass the absolute path, and find
                // a nice way to ensure that Garden tests
                // still work on any machine.
                message: describe_read_error(&import_info.path, &e),
                position: import_info.path_pos.clone(),
                notes: vec![],
                severity: Severity::Error,
            });
        }
    };

    let Ok(src) = String::from_utf8(src_bytes) else {
        return Err(Diagnostic {
            message: ErrorMessage(vec![
                msgtext!("The file "),
                msgcode!("{}", import_info.path.display()),
                msgtext!(" is not valid UTF-8."),
            ]),
            position: import_info.path_pos.clone(),
            notes: vec![],
            severity: Severity::Error,
        });
    };

    Ok(src)
}

fn describe_read_error(path: &Path, e: &std::io::Error) -> ErrorMessage {
    let parts = match e.kind() {
        std::io::ErrorKind::NotFound => {
            vec![
                msgtext!("No such file "),
                msgcode!("{}", path.display()),
                msgtext!("."),
            ]
        }
        std::io::ErrorKind::PermissionDenied => {
            vec![
                msgtext!("Permission denied when reading "),
                msgcode!("{}", path.display()),
                msgtext!("."),
            ]
        }
        _ => {
            // TODO: specific error on the file being a directory.
            vec![
                msgtext!("Could not read file "),
                msgcode!("{}", path.display()),
                msgtext!(", got error {}", e.kind()),
            ]
        }
    };

    ErrorMessage(parts)
}

/// Evaluate all toplevel items: definitions, then tests, then
/// expressions.
pub(crate) fn eval_toplevel_items(
    vfs_path: &VfsPathBuf,
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut defs = vec![];
    let mut exprs: Vec<Expression> = vec![];
    for item in items {
        match &item {
            ToplevelItem::Expr(toplevel_expression) => {
                exprs.push(toplevel_expression.0.clone());
            }
            ToplevelItem::Block(block) => {
                for expr in &block.exprs {
                    exprs.push(expr.as_ref().clone());
                }
            }
            _ => {
                defs.push(item.clone());
            }
        }
    }

    let ns = env.current_namespace();
    let (mut diagnostics, new_syms) = load_toplevel_items(&defs, env, ns.clone());
    for diagnostic in diagnostics.iter() {
        if matches!(diagnostic.severity, Severity::Error) {
            return Err(EvalError::Exception(
                diagnostic.position.clone(),
                diagnostic.message.clone(),
            ));
        }
    }

    diagnostics.extend(check_toplevel_items_in_env(vfs_path, items, env, ns));

    let mut summary = eval_tests(items, env, session);
    summary.diagnostics.extend(diagnostics);
    summary.new_syms.extend(new_syms);

    if !exprs.is_empty() {
        let value = eval_exprs(&exprs, env, session)?;
        summary.values = vec![value];
    }

    Ok(summary)
}

pub(crate) fn eval_tests_until_error(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut test_defs = vec![];
    for item in items {
        if let ToplevelItem::Test(test) = item {
            test_defs.push(test);
        }
    }

    // Update all the test definitions in the environment before
    // evaluating anything.
    for test in &test_defs {
        env.tests
            .insert(test.name_sym.name.clone(), (*test).clone());
    }

    let mut tests = vec![];
    for test in test_defs {
        push_test_stackframe(test, env);

        eval(env, session)?;
        tests.push((test.name_sym.clone(), None));

        env.stack.pop_to_toplevel();
    }

    Ok(ToplevelEvalSummary {
        values: vec![],
        new_syms: vec![],
        diagnostics: vec![],
        tests,
    })
}

/// Evaluate these tests.
pub(crate) fn eval_tests(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> ToplevelEvalSummary {
    let mut tests: Vec<(Symbol, Option<EvalError>)> = vec![];

    let mut test_defs = vec![];
    for item in items {
        if let ToplevelItem::Test(test) = item {
            test_defs.push(test);
        }
    }

    // Update all the test definitions in the environment before
    // evaluating anything.
    for test in &test_defs {
        env.tests
            .insert(test.name_sym.name.clone(), (*test).clone());
    }

    for test in test_defs {
        push_test_stackframe(test, env);

        match eval(env, session) {
            Ok(_) => {
                tests.push((test.name_sym.clone(), None));
            }
            Err(e) => {
                tests.push((test.name_sym.clone(), Some(e.clone())));
                if matches!(e, EvalError::Interrupted) {
                    break;
                }
            }
        }

        env.stack.pop_to_toplevel();
    }

    ToplevelEvalSummary {
        values: vec![],
        new_syms: vec![],
        diagnostics: vec![],
        tests,
    }
}

/// If we have a function that was previously called, and `id`
/// corresponds to one of its parameters, return the position of that
/// parameter along with the last saved value.
pub(crate) fn eval_up_to_param(
    env: &Env,
    items: &[ToplevelItem],
    id: SyntaxId,
) -> Option<(Value, Position)> {
    for item in items {
        match &item {
            ToplevelItem::Fun(name_sym, fun_info, _) => {
                let prev_args = match env
                    .prev_call_args
                    .get(&(name_sym.name.clone(), name_sym.position.path.to_path_buf()))
                {
                    _ if fun_info.params.params.is_empty() => vec![],
                    Some(prev_args) => prev_args.clone(),
                    None => {
                        continue;
                    }
                };

                for (i, param) in fun_info.params.params.iter().enumerate() {
                    if param.symbol.id != id {
                        continue;
                    }

                    if let Some(value) = prev_args.get(i) {
                        return Some((value.clone(), param.symbol.position.clone()));
                    }
                }
            }
            ToplevelItem::Method(meth_info, _) => {
                let Some(prev_calls_on_type) = env
                    .prev_method_call_args
                    .get(&meth_info.receiver_hint.sym.name)
                else {
                    continue;
                };

                let Some(fun_info) = meth_info.fun_info() else {
                    continue;
                };

                let (prev_receiver, prev_args) = match prev_calls_on_type
                    .get(&meth_info.name_sym.name)
                {
                    Some((prev_receiver, prev_args)) => (prev_receiver.clone(), prev_args.clone()),
                    None => continue,
                };

                if meth_info.receiver_sym.id == id {
                    return Some((
                        prev_receiver.clone(),
                        meth_info.receiver_sym.position.clone(),
                    ));
                }

                for (i, param) in fun_info.params.params.iter().enumerate() {
                    if param.symbol.id != id {
                        continue;
                    }

                    if let Some(value) = prev_args.get(i) {
                        return Some((value.clone(), param.symbol.position.clone()));
                    }
                }
            }
            _ => {}
        }
    }

    None
}

pub(crate) enum EvalUpToErr {
    EvalError(EvalError),
    NoExpressionFound,
    NoValueAvailable,
}

/// Try to evaluate items up to the syntax ID specified.
///
/// Returns None if we couldn't find anything to evaluate (not an error).
pub(crate) fn eval_up_to(
    vfs_path: &VfsPathBuf,
    env: &mut Env,
    session: &Session,
    items: &[ToplevelItem],
    offset: usize,
) -> Result<(Value, Position), EvalUpToErr> {
    let syn_ids = find_item_at(items, offset, offset);

    let mut expr_id: Option<SyntaxId> = None;
    let mut position = None;
    for syn_id in syn_ids.iter().rev() {
        // TODO: this is iterating items twice, which will be slower.
        if let Some(expr) = find_expr_of_id(items, syn_id.id()) {
            expr_id = Some(expr.id);
            position = Some(expr.position.clone());
            break;
        }
    }

    let Some(expr_id) = expr_id else {
        // We didn't find an expression with this ID, try to
        // evaluate a parameter with the ID.
        if let Some(AstId::Sym(syn_id)) = syn_ids.last() {
            if let Some((value, pos)) = eval_up_to_param(env, items, *syn_id) {
                return Ok((value, pos));
            }
        }

        return Err(EvalUpToErr::NoExpressionFound);
    };

    let Some(position) = position else {
        return Err(EvalUpToErr::NoExpressionFound);
    };

    let Some(item) = items
        .iter()
        .find(|&item| item.position().contains_offset(offset))
    else {
        return Err(EvalUpToErr::NoExpressionFound);
    };

    // TODO: this evaluates to the innermost enclosing expr. For
    // destructuring tuple, we want to return the tuple element and
    // position.

    let mut res: Result<_, EvalError> = match &item {
        ToplevelItem::Fun(name_sym, fun_info, _) => {
            let ns = env.current_namespace();
            load_toplevel_items(std::slice::from_ref(item), env, ns);

            let args = match env
                .prev_call_args
                .get(&(name_sym.name.clone(), name_sym.position.path.to_path_buf()))
            {
                _ if fun_info.params.params.is_empty() => vec![],
                Some(prev_args) => prev_args.clone(),
                None => {
                    // We don't have any known values that we can use, give up.
                    return Err(EvalUpToErr::NoValueAvailable);
                }
            };

            env.stop_at_expr_id = Some(expr_id);
            let res = eval_toplevel_call(&name_sym.name, &args, env, session, vfs_path);
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem::Method(method_info, _) => {
            let ns = env.current_namespace();
            load_toplevel_items(std::slice::from_ref(item), env, ns);

            let type_name = &method_info.receiver_hint.sym.name;

            let Some(prev_calls_for_type) = env.prev_method_call_args.get(type_name).cloned()
            else {
                return Err(EvalUpToErr::NoValueAvailable);
            };
            let Some((prev_recv, prev_args)) = prev_calls_for_type.get(&method_info.name_sym.name)
            else {
                return Err(EvalUpToErr::NoValueAvailable);
            };

            env.stop_at_expr_id = Some(expr_id);
            let res = eval_toplevel_method_call(
                prev_recv,
                &method_info.name_sym.name,
                prev_args,
                env,
                session,
                vfs_path,
            );
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem::Test(test) => {
            env.stop_at_expr_id = Some(expr_id);

            push_test_stackframe(test, env);
            let res = eval(env, session);
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem::Enum(_) | ToplevelItem::Struct(_) | ToplevelItem::Import(_) => {
            // nothing to do
            return Err(EvalUpToErr::NoExpressionFound);
        }
        ToplevelItem::Expr(_) | ToplevelItem::Block(_) => {
            env.stop_at_expr_id = Some(expr_id);

            let res = eval_toplevel_items(vfs_path, std::slice::from_ref(item), env, session);
            env.stop_at_expr_id = None;

            match res {
                Ok(mut eval_summary) => {
                    let Some(value) = eval_summary.values.pop() else {
                        env.stack.pop_to_toplevel();
                        return Err(EvalUpToErr::NoExpressionFound);
                    };
                    Ok((value, position))
                }
                Err(e) => Err(e),
            }
        }
    };

    if let Ok((value, pos)) = &mut res {
        // If the user asked for a single position in a let or a let with
        // tuple destructuring, handle it here.
        if let Some((var_value, var_pos)) = let_var_pos(value, items, &syn_ids) {
            *value = var_value;
            *pos = var_pos;
        }

        // For assignments and update assignments, return the newly
        // assigned value. E.g. for `foo += bar` we want the new value
        // for `foo`, not `Unit` (which is the value of the assignment
        // expression).
        if let Some((var_value, var_pos)) = assign_var_pos(env, items, &syn_ids) {
            *value = var_value;
            *pos = var_pos;
        }
    }

    env.stack.pop_to_toplevel();

    res.map_err(EvalUpToErr::EvalError)
}

/// If the AstId corresponded to a let variable or a destructuring let
/// variable, return the inner value and position.
///
/// ```garden
/// let (x, y) = (1, 2)
/// //      ^
/// ```
///
/// In this example, we want the position to be the position of `y`,
/// and the value to be `2`, not `(1, 2)`.
fn let_var_pos(
    value: &Value,
    items: &[ToplevelItem],
    ast_ids: &[AstId],
) -> Option<(Value, Position)> {
    let innermost_id = ast_ids.last()?;

    for syn_id in ast_ids.iter().rev() {
        let Some(expr) = find_expr_of_id(items, syn_id.id()) else {
            continue;
        };

        let dest = match &expr.expr_ {
            Expression_::ForIn(dest, _, _) => dest,
            Expression_::Let(dest, _, _) => dest,
            _ => continue,
        };

        match dest {
            LetDestination::Symbol(symbol) => {
                if &AstId::Sym(symbol.id) != innermost_id {
                    return None;
                }

                return Some((value.clone(), symbol.position.clone()));
            }
            LetDestination::Destructure(symbols) => {
                let mut matching_sym_i: Option<usize> = None;
                for (i, symbol) in symbols.iter().enumerate() {
                    if &AstId::Sym(symbol.id) == innermost_id {
                        matching_sym_i = Some(i);
                        break;
                    }
                }
                let matching_sym_i = matching_sym_i?;

                match value.as_ref() {
                    Value_::Tuple { items, .. } => {
                        let tuple_item = items.get(matching_sym_i)?;
                        return Some((
                            tuple_item.clone(),
                            symbols[matching_sym_i].position.clone(),
                        ));
                    }
                    _ => return None,
                }
            }
        };
    }

    None
}

/// If the AstId corresponded to a variable assignment, return the
/// inner value and position.
///
/// ```garden
/// foo_bar += baz
/// //  ^
/// ```
///
/// In this example, we want the value returned to be the value of
/// `foo_bar` after the assignment.
fn assign_var_pos(
    env: &Env,
    items: &[ToplevelItem],
    ast_ids: &[AstId],
) -> Option<(Value, Position)> {
    for syn_id in ast_ids.iter().rev() {
        let Some(expr) = find_expr_of_id(items, syn_id.id()) else {
            continue;
        };

        match &expr.expr_ {
            Expression_::Assign(symbol, _) | Expression_::AssignUpdate(symbol, _, _) => {
                let value = get_var(symbol, env)?;
                return Some((value, symbol.position.clone()));
            }
            Expression_::ForIn(dest, _, _) => match dest {
                LetDestination::Symbol(symbol) => {
                    let value = get_var(symbol, env)?;
                    return Some((value, symbol.position.clone()));
                }
                LetDestination::Destructure(_) => {
                    // TODO: handle eval-up-to on destructuring for
                    // loop variables.
                    break;
                }
            },
            _ => {
                break;
            }
        };
    }

    None
}

/// Helper for starting evaluation with a function call. Used when
/// running 'eval up to ID' in a function body.
pub(crate) fn eval_toplevel_call(
    name: &SymbolName,
    args: &[Value],
    env: &mut Env,
    session: &Session,
    vfs_path: &VfsPathBuf,
) -> Result<Value, EvalError> {
    let ns = env.current_namespace();

    // TODO: return an Err() rather than kludging a runtime string and letting
    // eval_env() return a type error.
    let recv_value = ns.borrow().values.get(name).cloned().unwrap_or_else(|| {
        Value::new(Value_::String(
            "ERROR: Tried to call a function that isn't defined".to_owned(),
        ))
    });
    env.push_value(recv_value);

    for value in args.iter().rev() {
        env.push_value(value.clone());
    }

    let recv_expr = Expression {
        position: Position::todo(vfs_path),
        expr_: Expression_::Variable(Symbol::new(
            Position::todo(vfs_path),
            &name.text,
            &mut env.id_gen,
        )),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let mut arguments = vec![];
    for _ in 0..args.len() {
        let pos = Position::todo(vfs_path);
        let expr = Rc::new(Expression::invalid(pos, env.id_gen.next()));
        arguments.push(ExpressionWithComma { expr, comma: None });
    }

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(vfs_path),
        arguments,
        close_paren: Position::todo(vfs_path),
    };

    let call_expr = Expression {
        position: Position::todo(vfs_path),
        expr_: Expression_::Call(Rc::new(recv_expr), paren_args),
        value_is_used: true,
        id: env.id_gen.next(),
    };
    env.push_expr_to_eval(
        ExpressionState::EvaluatedAllSubexpressions,
        call_expr.into(),
    );

    eval(env, session)
}

/// Helper for starting evaluation with a method call. Used for
/// eval-up-to when the cursor is inside a method body.
pub(crate) fn eval_toplevel_method_call(
    recv_value: &Value,
    meth_name: &SymbolName,
    args: &[Value],
    env: &mut Env,
    session: &Session,
    vfs_path: &VfsPathBuf,
) -> Result<Value, EvalError> {
    env.push_value(recv_value.clone());
    for value in args.iter().rev() {
        env.push_value(value.clone());
    }

    // Just create a placeholder symbol for the receiver. Since we
    // don't evaluate children, it doesn't matter.
    let recv_expr = Expression {
        position: Position::todo(vfs_path),
        expr_: Expression_::Variable(placeholder_symbol(
            Position::todo(vfs_path),
            &mut env.id_gen,
        )),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let meth_sym = Symbol {
        position: Position::todo(vfs_path),
        name: meth_name.clone(),
        id: env.id_gen.next(),
        interned_id: env.id_gen.intern_symbol(meth_name),
    };

    let mut arguments = vec![];
    for _ in 0..args.len() {
        let pos = Position::todo(vfs_path);
        let expr = Rc::new(Expression::invalid(pos, env.id_gen.next()));
        arguments.push(ExpressionWithComma { expr, comma: None });
    }

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(vfs_path),
        arguments,
        close_paren: Position::todo(vfs_path),
    };

    let call_expr = Expression {
        position: Position::todo(vfs_path),
        expr_: Expression_::MethodCall(Rc::new(recv_expr), meth_sym, paren_args),
        value_is_used: true,
        id: env.id_gen.next(),
    };
    env.push_expr_to_eval(
        ExpressionState::EvaluatedAllSubexpressions,
        call_expr.into(),
    );

    eval(env, session)
}

pub(crate) fn push_test_stackframe(test: &TestInfo, env: &mut Env) {
    let mut exprs_to_eval = vec![];
    for expr in test.body.exprs.iter().rev() {
        exprs_to_eval.push((ExpressionState::NotEvaluated, expr.clone()));
    }

    let namespace_path = test.name_sym.position.path.to_path_buf();
    let namespace = env.get_or_create_namespace(&namespace_path);

    let stack_frame = StackFrame {
        namespace,
        enclosing_name: EnclosingSymbol::Test(test.name_sym.clone()),
        return_hint: None,
        caller_pos: None,
        caller_expr_id: None,
        bindings: Bindings::default(),
        type_bindings: FxHashMap::default(),
        bindings_next_block: vec![],
        exprs_to_eval,
        evalled_values: vec![Value::unit()],
        caller_uses_value: true,
    };
    env.stack.0.push(stack_frame);
}

fn update_builtin_type_info(
    struct_info: &StructInfo,
    env: &mut Env,
    diagnostics: &mut Vec<Diagnostic>,
    namespace: Rc<RefCell<NamespaceInfo>>,
) {
    let symbol = &struct_info.name_sym;

    let Some(current_def) = env.types.get(&symbol.name) else {
        diagnostics.push(Diagnostic {
            notes: vec![],
            severity: Severity::Warning,
            message: ErrorMessage(vec![Text(format!(
                "Tried to update a built-in stub for a type `{}` that doesn't exist.",
                symbol.name
            ))]),
            position: symbol.position.clone(),
        });
        return;
    };

    let TypeDef::Builtin(kind, _) = &current_def.def else {
        diagnostics.push(Diagnostic {
            notes: vec![],
            severity: Severity::Warning,
            message: ErrorMessage(vec![Text(format!(
                "Tried to update a built-in stub but {} isn't a built-in type.",
                symbol.name,
            ))]),
            position: symbol.position.clone(),
        });
        return;
    };

    namespace.borrow_mut().types.insert(
        symbol.name.clone(),
        TypeDefAndMethods {
            def: TypeDef::Builtin(*kind, Some(struct_info.clone())),
            methods: current_def.methods.clone(),
        },
    );

    env.types.insert(
        symbol.name.clone(),
        TypeDefAndMethods {
            def: TypeDef::Builtin(*kind, Some(struct_info.clone())),
            methods: current_def.methods.clone(),
        },
    );
}

fn is_builtin_type(struct_info: &StructInfo) -> bool {
    let Some(field) = struct_info.fields.first() else {
        return false;
    };

    field.sym.name.text == "__BUILTIN_IMPLEMENTATION"
}

/// Update the built-in method described by `meth_info`, using
/// `fun_info` for position data.
fn update_builtin_meth_info(
    meth_info: &MethodInfo,
    fun_info: &FunInfo,
    env: &mut Env,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let type_name = &meth_info.receiver_hint.sym.name;

    let Some(type_def_and_methods) = env.types.get_mut(type_name) else {
        diagnostics.push(Diagnostic {
            notes: vec![],
            severity: Severity::Warning,
            message: ErrorMessage(vec![Text(format!(
                "Tried to update a built-in stub for a type {type_name} that doesn't exist."
            ))]),
            position: meth_info.receiver_hint.sym.position.clone(),
        });
        return;
    };

    match type_def_and_methods
        .methods
        .entry(meth_info.name_sym.name.clone())
    {
        Entry::Occupied(entry) => {
            let curr_meth_info = entry.into_mut();

            let MethodKind::BuiltinMethod(kind, _) = &curr_meth_info.kind else {
                diagnostics.push(Diagnostic {
                    notes: vec![],
                    severity: Severity::Warning,
                    message: ErrorMessage(vec![Text(format!(
                        // TODO: we need a better design principle around
                        // warning phrasing. It should probably always include
                        // an explanation of what will happen (in this case
                        // nothing).
                        "{}::{} is not a built-in method.",
                        type_name, meth_info.name_sym.name
                    ))]),
                    position: meth_info.name_sym.position.clone(),
                });
                return;
            };

            // Prefer hints and symbols from __prelude.gdn, as they have
            // better positions and full type parameters.
            curr_meth_info.receiver_hint = meth_info.receiver_hint.clone();
            curr_meth_info.receiver_sym = meth_info.receiver_sym.clone();
            curr_meth_info.name_sym = meth_info.name_sym.clone();

            curr_meth_info.kind = MethodKind::BuiltinMethod(*kind, Some(fun_info.clone()));
        }
        Entry::Vacant(entry) => {
            // Loading a stub for a built-in method that we haven't
            // seen before.
            entry.insert(meth_info.clone());
        }
    }
}

fn update_builtin_fun_info(
    fun_info: &FunInfo,
    env: &mut Env,
    namespace: Rc<RefCell<NamespaceInfo>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(symbol) = &fun_info.name_sym else {
        return;
    };

    let mut ns = namespace.borrow_mut();
    let Some(value) = ns.values.get(&symbol.name).cloned() else {
        diagnostics.push(Diagnostic {
            notes: vec![],
            severity: Severity::Warning,
            message: ErrorMessage(vec![Text(format!(
                "Tried to update a built-in stub for a function `{}` that doesn't exist.",
                symbol.name
            ))]),
            position: symbol.position.clone(),
        });

        let runtime_type = Type::from_fun_info(fun_info, &env.types, &env.stack.type_bindings())
            .unwrap_or_err_ty();

        // The built-in function doesn't exist, presumably the Garden
        // maintainer is still writing it. Add the function as-is to
        // the environment, so we can still type check call sites.
        ns.values.insert(
            symbol.name.clone(),
            Value::new(Value_::Fun {
                name_sym: symbol.clone(),
                fun_info: fun_info.clone(),
                runtime_type,
            }),
        );

        return;
    };

    let Value_::BuiltinFunction(kind, _, _) = value.as_ref() else {
        diagnostics.push(Diagnostic {
            notes: vec![],
            severity: Severity::Warning,
            message: ErrorMessage(vec![Text(format!(
                "Tried to update a built-in stub but `{}` isn't a built-in function (it's a {}).",
                symbol.name,
                Type::from_value(&value),
            ))]),
            position: symbol.position.clone(),
        });
        return;
    };

    let runtime_type =
        Type::from_fun_info(fun_info, &env.types, &env.stack.type_bindings()).unwrap_or_err_ty();

    ns.values.insert(
        symbol.name.clone(),
        Value::new(Value_::BuiltinFunction(
            *kind,
            Some(fun_info.clone()),
            Some(runtime_type),
        )),
    );
}

fn is_builtin_stub(fun_info: &FunInfo) -> bool {
    let exprs = &fun_info.body.exprs;
    if exprs.len() != 1 {
        return false;
    }

    let expr_ = &exprs[0].expr_;
    match expr_ {
        Expression_::Variable(variable) => variable.name.text == "__BUILTIN_IMPLEMENTATION",
        _ => false,
    }
}

// If value is a list of strings, return the strings as a vec. Return
// an error otherwise.
fn as_string_list(value: &Value) -> Result<Vec<String>, Value> {
    match value.as_ref() {
        Value_::List { items, .. } => {
            // TODO: check runtime_type instead.
            let mut res: Vec<String> = vec![];
            for item in items {
                match item.as_ref() {
                    Value_::String(s) => {
                        res.push(s.clone());
                    }
                    _ => {
                        return Err(item.clone());
                    }
                }
            }

            Ok(res)
        }
        _ => Err(value.clone()),
    }
}

/// Restore `stack_frame` by putting back evaluated values and
/// expressions to evaluate.
///
/// This enables evaluation to halt in a state where the user can
/// choose to try again if they wish.
fn restore_stack_frame(
    env: &mut Env,
    expr_to_eval: (ExpressionState, Rc<Expression>),
    evalled_values: &[Value],
) {
    for value in evalled_values {
        env.push_value(value.clone());
    }

    let (state, expr) = expr_to_eval;
    env.push_expr_to_eval(state, expr);
}

/// Values to push back to the evalled_values stack if we encounter an
/// error, so we can resume.
#[derive(Debug, Clone)]
struct RestoreValues(Vec<Value>);

fn eval_if(
    env: &mut Env,
    expr_value_is_used: bool,
    bool_position: &Position,
    then_body: &Block,
    else_body: Option<&Block>,
) -> Result<(), (RestoreValues, EvalError)> {
    let condition_value = env
        .pop_value()
        .expect("Popped an empty value stack for if condition");

    if let Some(b) = condition_value.as_rust_bool() {
        if b {
            eval_block(env, expr_value_is_used, then_body);
        } else {
            if let Some(else_body) = else_body {
                eval_block(env, expr_value_is_used, else_body);
            } else {
                // Ensure we always push a bindings block.
                env.current_frame_mut().bindings.push_block();

                if expr_value_is_used {
                    env.push_value(Value::unit());
                }
            }
        }
    } else {
        return Err((
            RestoreValues(vec![condition_value.clone()]),
            EvalError::Exception(
                bool_position.clone(),
                format_type_error(
                    &TypeName {
                        text: "Bool".into(),
                    },
                    &condition_value,
                    env,
                ),
            ),
        ));
    }

    Ok(())
}

fn eval_while_body(
    env: &mut Env,
    expr_value_is_used: bool,
    condition_pos: &Position,
    expr: Rc<Expression>,
    body: &Block,
) -> Result<(), (RestoreValues, EvalError)> {
    let condition_value = env
        .pop_value()
        .expect("Popped an empty value stack for while loop");

    let Some(b) = condition_value.as_rust_bool() else {
        return Err((
            RestoreValues(vec![condition_value.clone()]),
            EvalError::Exception(
                condition_pos.clone(),
                format_type_error(
                    &TypeName {
                        text: "Bool".into(),
                    },
                    &condition_value,
                    env,
                ),
            ),
        ));
    };

    let stack_frame = env.current_frame_mut();
    if b {
        stack_frame
            .exprs_to_eval
            .push((ExpressionState::EvaluatedAllSubexpressions, expr.clone()));

        // After the loop body, we will want to evaluate the expression again.
        stack_frame
            .exprs_to_eval
            .push((ExpressionState::NotEvaluated, expr.clone()));

        // Evaluate the body.
        eval_block(env, expr_value_is_used, body);
    } else {
        stack_frame.bindings.push_block();

        // We're done.
        stack_frame
            .exprs_to_eval
            .push((ExpressionState::EvaluatedAllSubexpressions, expr.clone()));
    }

    Ok(())
}

fn eval_for_in(
    env: &mut Env,
    iter_dest: &LetDestination,
    iteree_pos: &Position,
    outer_expr: Rc<Expression>,
    body: &Block,
) -> Result<(), (RestoreValues, EvalError)> {
    let iteree_value = env
        .pop_value()
        .expect("Popped an empty value stack for `for` loop iterated value");

    let iteree_idx = env
        .pop_value()
        .expect("Popped an empty value stack for `for` loop index");

    let iteree_idx = match iteree_idx.as_ref() {
        Value_::Integer(i) => *i,
        _ => {
            unreachable!(
                "`for` loop index should always be an `Int`, got `{}`: {}",
                iteree_idx.display(env),
                outer_expr.position.as_ide_string(&env.project_root)
            )
        }
    };

    let Value_::List { items, .. } = iteree_value.as_ref() else {
        return Err((
            RestoreValues(vec![iteree_value.clone()]),
            EvalError::Exception(
                iteree_pos.clone(),
                format_type_error(
                    &TypeName {
                        text: "List".into(),
                    },
                    &iteree_value,
                    env,
                ),
            ),
        ));
    };

    if iteree_idx as usize >= items.len() {
        return Ok(());
    }

    // After an iteration the loop body, evaluate again. We don't
    // re-evaluate the iteree expression though.
    env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());

    env.push_value(Value::new(Value_::Integer(iteree_idx + 1)));
    env.push_value(iteree_value.clone());

    let mut bindings: Vec<(Symbol, Value)> = vec![];
    let iteree_current_elem = items[iteree_idx as usize].clone();

    match iter_dest {
        LetDestination::Symbol(symbol) => {
            if !symbol.name.is_underscore() {
                bindings.push((symbol.clone(), iteree_current_elem));
            }
        }
        LetDestination::Destructure(symbols) => match iteree_current_elem.as_ref() {
            Value_::Tuple { items, .. } => {
                if items.len() != symbols.len() {
                    return Err((
                        RestoreValues(vec![iteree_current_elem.clone()]),
                        EvalError::Exception(
                            iteree_pos.clone(),
                            ErrorMessage(vec![Text(format!(
                                "Expected a tuple with {} items, got a tuple with {} items.",
                                symbols.len(),
                                items.len(),
                            ))]),
                        ),
                    ));
                }

                for (symbol, item) in symbols.iter().zip(items) {
                    if symbol.name.is_underscore() {
                        continue;
                    }

                    bindings.push((symbol.clone(), item.clone()));
                }
            }
            _ => {
                return Err((
                    RestoreValues(vec![iteree_current_elem.clone()]),
                    EvalError::Exception(
                        iteree_pos.clone(),
                        ErrorMessage(vec![Text(format!("Incorrect type for variable: {}", "x"))]),
                    ),
                ));
            }
        },
    }

    let stack_frame = env.current_frame_mut();
    stack_frame.bindings_next_block = bindings;
    eval_block(env, false, body);

    Ok(())
}

fn eval_assign_update(
    env: &mut Env,
    expr_value_is_used: bool,
    position: &Position,
    variable: &Symbol,
    op: AssignUpdateKind,
) -> Result<(), (RestoreValues, EvalError)> {
    let var_name = &variable.name;

    let Some(var_value) = get_var(variable, env) else {
        return Err((
            RestoreValues(vec![]),
            EvalError::Exception(
                variable.position.clone(),
                ErrorMessage(vec![Text(format!(
                    "{var_name} is not currently bound. Try `let {var_name} = something`."
                ))]),
            ),
        ));
    };
    let Value_::Integer(var_value_num) = var_value.as_ref() else {
        return Err((
            RestoreValues(vec![]),
            EvalError::Exception(
                position.clone(),
                format_type_error(&TypeName { text: "Int".into() }, &var_value, env),
            ),
        ));
    };

    let rhs_value = env.pop_value().expect(&format!(
        "Popped an empty value stack for `{}`",
        op.as_src()
    ));
    let Value_::Integer(rhs_num) = rhs_value.as_ref() else {
        return Err((
            RestoreValues(vec![rhs_value.clone()]),
            EvalError::Exception(
                position.clone(),
                format_type_error(&TypeName { text: "Int".into() }, &rhs_value, env),
            ),
        ));
    };

    let new_value_num = match op {
        AssignUpdateKind::Add => *var_value_num + *rhs_num,
        AssignUpdateKind::Subtract => *var_value_num - *rhs_num,
    };
    env.current_frame_mut()
        .bindings
        .set_existing(variable, Value::new(Value_::Integer(new_value_num)));

    if expr_value_is_used {
        env.push_value(Value::unit());
    }

    Ok(())
}

fn eval_assign(
    env: &mut Env,
    expr_value_is_used: bool,
    variable: &Symbol,
) -> Result<(), (RestoreValues, EvalError)> {
    let var_name = &variable.name;
    if !env.current_frame_mut().bindings.has(variable.interned_id) {
        return Err((
            RestoreValues(vec![]),
            EvalError::Exception(
                variable.position.clone(),
                ErrorMessage(vec![Text(format!(
                    "{var_name} is not currently bound. Try `let {var_name} = something`."
                ))]),
            ),
        ));
    }

    let expr_value = env
        .pop_value()
        .expect("Popped an empty value stack for let value");

    env.stack
        .0
        .last_mut()
        .unwrap()
        .bindings
        .set_existing(variable, expr_value);

    if expr_value_is_used {
        env.push_value(Value::unit());
    }

    Ok(())
}

/// Bind `variable` in the current local environment.
fn eval_let(
    env: &mut Env,
    expr_value_is_used: bool,
    destination: &LetDestination,
    init_value_pos: &Position,
    hint: &Option<TypeHint>,
) -> Result<(), (RestoreValues, EvalError)> {
    let expr_value = env
        .pop_value()
        .expect("Popped an empty value stack for let value");
    let stack_frame = env.current_frame_mut();
    let type_bindings = stack_frame.type_bindings.clone();

    if let Some(hint) = hint {
        let expected_ty = match Type::from_hint(hint, &env.types, &type_bindings) {
            Ok(ty) => ty,
            Err(e) => {
                return Err((
                    RestoreValues(vec![]),
                    EvalError::Exception(
                        hint.position.clone(),
                        ErrorMessage(vec![msgtext!("Unbound type in hint: "), Code(e)]),
                    ),
                ));
            }
        };

        if let Err(msg) = check_type(&expr_value, &expected_ty, env) {
            return Err((
                RestoreValues(vec![expr_value]),
                EvalError::Exception(init_value_pos.clone(), msg),
            ));
        };
    }

    let stack_frame = env.current_frame_mut();
    match destination {
        LetDestination::Symbol(symbol) => stack_frame.bindings.add_new(symbol, expr_value),
        LetDestination::Destructure(symbols) => match expr_value.as_ref() {
            Value_::Tuple { items, .. } => {
                if items.len() != symbols.len() {
                    return Err((
                        RestoreValues(vec![expr_value.clone()]),
                        EvalError::Exception(
                            init_value_pos.clone(),
                            ErrorMessage(vec![Text(format!(
                                "Expected a tuple with {} items, got a tuple with {} items.",
                                symbols.len(),
                                items.len(),
                            ))]),
                        ),
                    ));
                }

                for (symbol, item) in symbols.iter().zip(items) {
                    stack_frame.bindings.add_new(symbol, item.clone());
                }
            }
            _ => {
                return Err((
                    RestoreValues(vec![expr_value]),
                    EvalError::Exception(
                        init_value_pos.clone(),
                        ErrorMessage(vec![Text(format!("Incorrect type for variable: {}", "x"))]),
                    ),
                ));
            }
        },
    }

    // `let x = 1` should always evaluate to Unit. This is slightly
    // annoying when incrementally writing a block, but makes it
    // easier when incrementally writing a function.
    //
    // ```
    // fun foo(): Unit {
    //     let just_added_this_var = 1;
    // }
    // ```
    if expr_value_is_used {
        env.push_value(Value::unit());
    }

    Ok(())
}

fn format_type_error<T: ToString + ?Sized>(expected: &T, value: &Value, env: &Env) -> ErrorMessage {
    let actual_ty = Type::from_value(value);

    let parts = if actual_ty.is_unit() {
        vec![
            msgtext!("Expected "),
            Code(expected.to_string()),
            msgtext!(", but got "),
            Code("Unit".to_owned()),
        ]
    } else {
        vec![
            msgtext!("Expected "),
            Code(expected.to_string()),
            msgtext!(", but got "),
            Code(format!("{}", Type::from_value(value))),
            msgtext!(": "),
            Code(value.display(env)),
        ]
    };

    ErrorMessage(parts)
}

fn eval_boolean_binop(
    env: &mut Env,
    expr_value_is_used: bool,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), (RestoreValues, EvalError)> {
    {
        let rhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for LHS of binary operator");

        let Some(lhs_bool) = lhs_value.as_rust_bool() else {
            return Err((
                RestoreValues(vec![lhs_value.clone(), rhs_value]),
                EvalError::Exception(
                    lhs_position.clone(),
                    format_type_error(
                        &TypeName {
                            text: "Bool".into(),
                        },
                        &lhs_value,
                        env,
                    ),
                ),
            ));
        };

        let Some(rhs_bool) = rhs_value.as_rust_bool() else {
            return Err((
                RestoreValues(vec![lhs_value, rhs_value.clone()]),
                EvalError::Exception(
                    rhs_position.clone(),
                    format_type_error(
                        &TypeName {
                            text: "Bool".into(),
                        },
                        &rhs_value,
                        env,
                    ),
                ),
            ));
        };

        if expr_value_is_used {
            match op {
                BinaryOperatorKind::And => {
                    env.push_value(Value::bool(lhs_bool && rhs_bool));
                }
                BinaryOperatorKind::Or => {
                    env.push_value(Value::bool(lhs_bool || rhs_bool));
                }
                _ => unreachable!(),
            }
        }
    }
    Ok(())
}

fn eval_equality_binop(env: &mut Env, expr_value_is_used: bool, op: BinaryOperatorKind) {
    let rhs_value = env
        .pop_value()
        .expect("Popped an empty value stack for RHS of binary operator");
    let lhs_value = env
        .pop_value()
        .expect("Popped an empty value stack for LHS of binary operator");

    if expr_value_is_used {
        match op {
            BinaryOperatorKind::Equal => {
                env.push_value(Value::bool(lhs_value == rhs_value));
            }
            BinaryOperatorKind::NotEqual => {
                env.push_value(Value::bool(lhs_value != rhs_value));
            }
            _ => unreachable!(),
        }
    }
}

fn eval_integer_binop(
    env: &mut Env,
    expr_value_is_used: bool,
    position: &Position,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), (RestoreValues, EvalError)> {
    {
        let rhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for LHS of binary operator");

        let lhs_num = match lhs_value.as_ref() {
            Value_::Integer(i) => *i,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value.clone(), rhs_value]),
                    EvalError::Exception(
                        lhs_position.clone(),
                        format_type_error(&TypeName { text: "Int".into() }, &lhs_value, env),
                    ),
                ));
            }
        };
        let rhs_num = match rhs_value.as_ref() {
            Value_::Integer(i) => *i,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value, rhs_value.clone()]),
                    EvalError::Exception(
                        rhs_position.clone(),
                        format_type_error(&TypeName { text: "Int".into() }, &rhs_value, env),
                    ),
                ));
            }
        };

        let value = match op {
            BinaryOperatorKind::Add => Value::new(Value_::Integer(lhs_num.wrapping_add(rhs_num))),
            BinaryOperatorKind::Subtract => {
                Value::new(Value_::Integer(lhs_num.wrapping_sub(rhs_num)))
            }
            BinaryOperatorKind::Multiply => {
                Value::new(Value_::Integer(lhs_num.wrapping_mul(rhs_num)))
            }
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::Exception(
                            position.clone(),
                            ErrorMessage(vec![Text(format!(
                                "Tried to divide {} by zero.",
                                lhs_value.display(env)
                            ))]),
                        ),
                    ));
                }

                Value::new(Value_::Integer(lhs_num / rhs_num))
            }
            BinaryOperatorKind::Modulo => Value::new(Value_::Integer(lhs_num % rhs_num)),
            BinaryOperatorKind::Exponent => {
                if rhs_num < 0 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::Exception(
                            position.clone(),
                            ErrorMessage(vec![Text(format!(
                                "Cannot raise an integer to a negative power, got {}.^ {}",
                                lhs_value.display(env),
                                rhs_value.display(env),
                            ))]),
                        ),
                    ));
                }

                if rhs_num > u32::MAX as i64 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::Exception(
                            position.clone(),
                            ErrorMessage(vec![Text(format!(
                                "Exponent is too large, got {}.^ {}",
                                lhs_value.display(env),
                                rhs_value.display(env),
                            ))]),
                        ),
                    ));
                }

                match lhs_num.checked_pow(rhs_num as u32) {
                    Some(num) => Value::new(Value_::Integer(num)),
                    None => {
                        return Err((
                            RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                            EvalError::Exception(
                                position.clone(),
                                ErrorMessage(vec![Text(format!(
                                    "Integer overflow on raising to the power, got {}.^ {}",
                                    lhs_value.display(env),
                                    rhs_value.display(env),
                                ))]),
                            ),
                        ));
                    }
                }
            }
            BinaryOperatorKind::LessThan => Value::bool(lhs_num < rhs_num),
            BinaryOperatorKind::GreaterThan => Value::bool(lhs_num > rhs_num),
            BinaryOperatorKind::LessThanOrEqual => Value::bool(lhs_num <= rhs_num),
            BinaryOperatorKind::GreaterThanOrEqual => Value::bool(lhs_num >= rhs_num),
            _ => {
                unreachable!()
            }
        };

        if expr_value_is_used {
            env.push_value(value);
        }
    }
    Ok(())
}

fn eval_string_concat(
    env: &mut Env,
    expr_value_is_used: bool,
    lhs_position: &Position,
    rhs_position: &Position,
) -> Result<(), (RestoreValues, EvalError)> {
    {
        let rhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for RHS of `^`.");
        let lhs_value = env
            .pop_value()
            .expect("Popped an empty value stack for LHS of `^`.");

        let lhs_str = match lhs_value.as_ref() {
            Value_::String(s) => s,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value.clone(), rhs_value]),
                    EvalError::Exception(
                        lhs_position.clone(),
                        format_type_error(
                            &TypeName {
                                text: "String".into(),
                            },
                            &lhs_value,
                            env,
                        ),
                    ),
                ));
            }
        };
        let rhs_str = match rhs_value.as_ref() {
            Value_::String(s) => s,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value, rhs_value.clone()]),
                    EvalError::Exception(
                        rhs_position.clone(),
                        format_type_error(
                            &TypeName {
                                text: "String".into(),
                            },
                            &rhs_value,
                            env,
                        ),
                    ),
                ));
            }
        };

        let mut out_str = String::with_capacity(lhs_str.len() + rhs_str.len());
        out_str.push_str(lhs_str);
        out_str.push_str(rhs_str);

        let value = Value::new(Value_::String(out_str));

        if expr_value_is_used {
            env.push_value(value);
        }
    }
    Ok(())
}

fn check_arity(
    fun_name: &SymbolName,
    receiver_value: &Value,
    receiver_pos: &Position,
    expected: usize,
    arg_positions: &[Position],
    arg_values: &[Value],
) -> Result<(), (RestoreValues, EvalError)> {
    if arg_values.len() != expected {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        let error_position = if arg_values.len() > expected {
            arg_positions[expected].clone()
        } else {
            // TODO: for methods it would be better to highlight the
            // position of the method name at the call site, not the
            // receiver.
            receiver_pos.clone()
        };

        return Err((
            RestoreValues(saved_values),
            EvalError::Exception(
                error_position,
                ErrorMessage(vec![Text(format!(
                    "Function {} requires {} argument{}, but got {}",
                    fun_name,
                    expected,
                    if expected == 1 { "" } else { "s" },
                    arg_values.len()
                ))]),
            ),
        ));
    }

    Ok(())
}

/// Check that `value` has `expected` type.
fn check_type(value: &Value, expected: &Type, env: &Env) -> Result<(), ErrorMessage> {
    let value_type = Type::from_value(value);

    if is_subtype(&value_type, expected) {
        Ok(())
    } else {
        Err(format_type_error(expected, value, env))
    }
}

fn as_int_str_tuple(i: i64, s: &str) -> Value {
    let items = vec![
        Value::new(Value_::Integer(i)),
        Value::new(Value_::String(s.to_owned())),
    ];

    Value::new(Value_::Tuple {
        items,
        item_types: vec![Type::int(), Type::string()],
    })
}

fn eval_builtin_call(
    env: &mut Env,
    kind: BuiltinFunctionKind,
    receiver_value: &Value,
    receiver_pos: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    expr_value_is_used: bool,
    position: &Position,
    session: &Session,
) -> Result<(), (RestoreValues, EvalError)> {
    match kind {
        BuiltinFunctionKind::Throw => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            match arg_values[0].as_ref() {
                Value_::String(msg) => {
                    let message = ErrorMessage(vec![Text(msg.clone())]);
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(position.clone(), message),
                    ));
                }
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            }
        }
        BuiltinFunctionKind::Print => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match arg_values[0].as_ref() {
                Value_::String(s) => match session.stdout_mode {
                    StdoutMode::WriteDirectly => {
                        print!("{s}");
                    }
                    StdoutMode::WriteJson => {
                        let response = Response {
                            kind: ResponseKind::Printed { s: s.clone() },
                            position: None,
                            id: None,
                        };
                        print_as_json(&response, session.pretty_print_json);
                    }
                    StdoutMode::DoNotWrite => {}
                },
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            }

            if expr_value_is_used {
                env.push_value(Value::unit());
            }
        }
        BuiltinFunctionKind::Println => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match arg_values[0].as_ref() {
                Value_::String(s) => match session.stdout_mode {
                    StdoutMode::WriteDirectly => {
                        println!("{s}");
                    }
                    StdoutMode::WriteJson => {
                        let response = Response {
                            kind: ResponseKind::Printed {
                                s: format!("{s}\n"),
                            },
                            position: None,
                            id: None,
                        };
                        print_as_json(&response, session.pretty_print_json);
                    }
                    StdoutMode::DoNotWrite => {}
                },
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            }

            if expr_value_is_used {
                env.push_value(Value::unit());
            }
        }
        BuiltinFunctionKind::Shell => {
            if env.enforce_sandbox {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ForbiddenInSandbox(receiver_pos.clone()),
                ));
            }

            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            match arg_values[0].as_ref() {
                Value_::String(s) => {
                    match as_string_list(&arg_values[1]) {
                        Ok(items) => {
                            let mut command = std::process::Command::new(s);
                            for item in items {
                                command.arg(item);
                            }

                            let v = match command.output() {
                                Ok(output) => {
                                    let mut s = String::new();

                                    // TODO: complain if output is not UTF-8.
                                    s.write_str(&String::from_utf8_lossy(&output.stdout))
                                        .unwrap();
                                    s.write_str(&String::from_utf8_lossy(&output.stderr))
                                        .unwrap();

                                    if output.status.success() {
                                        Value::ok(Value::new(Value_::String(s)))
                                    } else {
                                        Value::err(Value::new(Value_::String(s)))
                                    }
                                }
                                Err(e) => {
                                    let s = Value::new(Value_::String(format!("{e}")));
                                    Value::err(s)
                                }
                            };

                            if expr_value_is_used {
                                env.push_value(v);
                            }
                        }
                        Err(v) => {
                            let mut saved_values = vec![];
                            for value in arg_values.iter().rev() {
                                saved_values.push(value.clone());
                            }
                            saved_values.push(receiver_value.clone());

                            let message = format_type_error(
                                &TypeName {
                                    text: "List".into(),
                                },
                                &v,
                                env,
                            );
                            return Err((
                                RestoreValues(saved_values),
                                EvalError::Exception(arg_positions[0].clone(), message),
                            ));
                        }
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            }
        }
        BuiltinFunctionKind::StringRepr => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            if expr_value_is_used {
                env.push_value(Value::new(Value_::String(arg_values[0].display(env))));
            }
        }
        BuiltinFunctionKind::NamespaceFunctions => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let Value_::Namespace(namespace_info) = arg_values[0].as_ref() else {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                let message = format_type_error(
                    &TypeName {
                        text: "Namespace".into(),
                    },
                    &arg_values[0],
                    env,
                );
                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(arg_positions[0].clone(), message),
                ));
            };

            let mut fun_names = vec![];

            let ns = namespace_info.borrow();
            for sym_name in ns.values.keys() {
                if ns.external_syms.contains(sym_name) {
                    fun_names.push(sym_name.text.clone());
                }
            }

            fun_names.sort();
            let items = fun_names
                .into_iter()
                .map(|n| Value::new(Value_::String(n)))
                .collect::<Vec<_>>();

            if expr_value_is_used {
                env.push_value(Value::new(Value_::List {
                    items,
                    elem_type: Type::string(),
                }));
            }
        }
        BuiltinFunctionKind::ListDirectory => {
            if env.enforce_sandbox {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ForbiddenInSandbox(receiver_pos.clone()),
                ));
            }

            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let path_s = match unwrap_path(&arg_values[0], env) {
                Ok(s) => s,
                Err(msg) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(receiver_pos.clone(), msg),
                    ));
                }
            };

            let path = PathBuf::from(path_s.clone());

            let value = match path.read_dir() {
                Ok(dir_iter) => {
                    let mut items = vec![];
                    for entry in dir_iter {
                        // TODO: don't silently discard errors.
                        if let Ok(entry) = entry {
                            items.push(Value::path(entry.path().display().to_string()));
                        }
                    }

                    Value::ok(Value::new(Value_::List {
                        items,
                        elem_type: Type::path(),
                    }))
                }
                Err(e) => {
                    let s = Value::new(Value_::String(format!("{e} {path_s}")));
                    Value::err(s)
                }
            };

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        BuiltinFunctionKind::SourceDirectory => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            let v = match std::fs::canonicalize(position.path.as_ref()) {
                Ok(abspath) => Value::some(Value::path(abspath.display().to_string())),
                Err(_) => Value::none(),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::ShellArguments => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            if expr_value_is_used {
                let items = env
                    .cli_args
                    .iter()
                    .map(|arg| Value::new(Value_::String(arg.clone())))
                    .collect::<Vec<_>>();

                env.push_value(Value::new(Value_::List {
                    items,
                    elem_type: Type::string(),
                }));
            }
        }
        BuiltinFunctionKind::SetWorkingDirectory => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let path_s = match unwrap_path(&arg_values[0], env) {
                Ok(s) => s,
                Err(msg) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(receiver_pos.clone(), msg),
                    ));
                }
            };

            let path = PathBuf::from(path_s);
            let v = match std::env::set_current_dir(path) {
                Ok(()) => Value::ok(Value::unit()),
                Err(e) => Value::err(Value::new(Value_::String(format!("{e}")))),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::WorkingDirectory => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            // TODO: when we have a userland result type, use that.
            let path = std::env::current_dir().unwrap_or_default();

            if expr_value_is_used {
                env.push_value(Value::path(path.display().to_string()));
            }
        }
        BuiltinFunctionKind::WriteFile => {
            if env.enforce_sandbox {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ForbiddenInSandbox(receiver_pos.clone()),
                ));
            }

            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            let content_s = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error("String", &arg_values[0], env);
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let path_s = match unwrap_path(&arg_values[1], env) {
                Ok(s) => s,
                Err(msg) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(receiver_pos.clone(), msg),
                    ));
                }
            };

            let path = PathBuf::from(path_s);

            let v = match std::fs::write(path, content_s) {
                Ok(()) => Value::ok(Value::unit()),
                Err(e) => Value::err(Value::new(Value_::String(format!("{e}")))),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::CheckSnippet => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            let snippet = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let v = check_snippet(snippet, env);
            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::Lex => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            let src = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let path = Rc::new(PathBuf::from("__snippet"));
            let vfs_path = env.vfs.insert(path.clone(), src.clone());

            let (mut token_stream, _lex_errors) = lex::lex(&vfs_path, src);

            let mut items = vec![];
            while let Some(token) = token_stream.pop() {
                for (pos, comment_str) in &token.preceding_comments {
                    items.push(as_int_str_tuple(pos.start_offset as i64, comment_str));
                }

                items.push(as_int_str_tuple(
                    token.position.start_offset as i64,
                    token.text,
                ));
            }

            for (pos, comment_str) in &token_stream.trailing_comments {
                items.push(as_int_str_tuple(pos.start_offset as i64, comment_str));
            }

            let v = Value_::List {
                items,
                elem_type: Type::Tuple(vec![Type::int(), Type::string()]),
            };
            if expr_value_is_used {
                env.push_value(Value::new(v));
            }
        }
        BuiltinFunctionKind::DocComment => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            let namespace = match arg_values[0].as_ref() {
                Value_::Namespace(ns) => ns,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "Namespace".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let target_name = match arg_values[1].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let ns = namespace.borrow();

            let mut ret_value = Value::none();
            for (name, value) in &ns.values {
                if name.text == *target_name {
                    if let Some(doc_comment_text) = value.doc_comment() {
                        ret_value = Value::some(Value::new(Value_::String(doc_comment_text)));
                    }
                    break;
                }
            }

            if expr_value_is_used {
                env.push_value(ret_value);
            }
        }
        BuiltinFunctionKind::DocCommentForType => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            let name = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let v = match env.types.get(&TypeName::from(name)) {
                Some(ty) => {
                    let doc_comment: Option<String> = match &ty.def {
                        TypeDef::Builtin(_, struct_info) => {
                            struct_info.as_ref().and_then(|si| si.doc_comment.clone())
                        }
                        TypeDef::Enum(enum_info) => enum_info.doc_comment.clone(),
                        TypeDef::Struct(struct_info) => struct_info.doc_comment.clone(),
                    };

                    match doc_comment {
                        Some(s) => Value::some(Value::new(Value_::String(s))),
                        None => Value::none(),
                    }
                }
                None => Value::none(),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::SourceForType => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            let name = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let message = format_type_error(
                        &TypeName {
                            text: "String".into(),
                        },
                        &arg_values[0],
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(arg_positions[0].clone(), message),
                    ));
                }
            };

            let v = match env.types.get(&TypeName::from(name)) {
                Some(ty) => {
                    let src: Option<String> = match &ty.def {
                        TypeDef::Builtin(_, Some(struct_info)) => {
                            env.vfs.pos_src(&struct_info.pos).map(|s| s.to_owned())
                        }
                        TypeDef::Builtin(_, None) => None,
                        TypeDef::Enum(enum_info) => {
                            env.vfs.pos_src(&enum_info.pos).map(|s| s.to_owned())
                        }
                        TypeDef::Struct(struct_info) => {
                            env.vfs.pos_src(&struct_info.pos).map(|s| s.to_owned())
                        }
                    };

                    match src {
                        Some(s) => Value::some(Value::new(Value_::String(s))),
                        None => Value::none(),
                    }
                }
                None => Value::none(),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::PreludeTypes => {
            check_arity(
                &SymbolName {
                    text: format!("{kind}"),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            let mut names = env
                .prelude_namespace
                .borrow()
                .types
                .keys()
                .map(|k| (k.text.clone()))
                .collect::<Vec<_>>();
            names.sort();

            let items = names
                .into_iter()
                .map(|n| Value::new(Value_::String(n)))
                .collect::<Vec<_>>();

            let v = Value::new(Value_::List {
                items,
                elem_type: Type::string(),
            });

            if expr_value_is_used {
                env.push_value(v);
            }
        }
    }

    Ok(())
}

fn check_snippet(src: &str, env: &Env) -> Value {
    let path = PathBuf::from("__snippet.gdn");

    let mut check_env = env
        .initial_state
        .as_ref()
        .map(|e| e.as_ref().clone())
        .unwrap_or_else(|| Env::new(IdGenerator::default(), Vfs::default()));

    let vfs_path = check_env.vfs.insert(Rc::new(path.clone()), src.to_owned());
    let (items, syntax_errors) = parse_toplevel_items(&vfs_path, src, &mut check_env.id_gen);

    let mut error_messages = vec![];
    for err in syntax_errors {
        error_messages.push(Value::new(Value_::String(err.message().as_string())));
    }

    for Diagnostic {
        message, severity, ..
    } in check_toplevel_items(&vfs_path, &items, &check_env)
    {
        match severity {
            Severity::Warning => {}
            Severity::Error => {
                error_messages.push(Value::new(Value_::String(message.as_string())));
            }
        }
    }

    if error_messages.is_empty() {
        Value::ok(Value::unit())
    } else {
        Value::err(Value::new(Value_::List {
            items: error_messages,
            elem_type: Type::string(),
        }))
    }
}

/// Evaluate a function call.
///
/// If we're calling a userland function, return the new stackframe to
/// evaluate next.
fn eval_call(
    env: &mut Env,
    expr_value_is_used: bool,
    caller_expr: Rc<Expression>,
    paren_args: &ParenthesizedArguments,
    session: &Session,
) -> Result<Option<StackFrame>, (RestoreValues, EvalError)> {
    let mut arg_values = vec![];
    let mut arg_positions = vec![];

    for arg in &paren_args.arguments {
        arg_values.push(
            env.pop_value()
                .expect("Popped an empty value for stack for call arguments"),
        );
        arg_positions.push(arg.expr.position.clone());
    }
    let receiver_value = env
        .pop_value()
        .expect("Popped an empty value stack for call receiver");

    let stack_frame = env.current_frame_mut();

    match receiver_value.as_ref() {
        Value_::Closure(bindings, fun_info, _) => {
            let mut bindings = bindings.clone();

            if fun_info.params.params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                let message = ErrorMessage(vec![Text(format!(
                    "Closure expects {} argument{}, but got {}",
                    fun_info.params.params.len(),
                    if fun_info.params.params.len() == 1 {
                        ""
                    } else {
                        "s"
                    },
                    arg_values.len()
                ))]);
                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(caller_expr.position.clone(), message),
                ));
            }

            let mut fun_subexprs = vec![];
            for expr in fun_info.body.exprs.iter().rev() {
                fun_subexprs.push((ExpressionState::NotEvaluated, expr.clone()));
            }

            let mut fun_bindings = FxHashMap::default();
            for (param, value) in fun_info.params.params.iter().zip(arg_values.iter()) {
                if !param.symbol.name.is_underscore() {
                    fun_bindings.insert(param.symbol.interned_id, value.clone());
                }
            }

            let mut type_bindings = TypeVarEnv::default();
            for type_param in &fun_info.type_params {
                // TODO: compute the value of these type params properly.
                type_bindings.insert(type_param.name.clone(), Some(Type::Top));
            }

            bindings.push(BlockBindings {
                values: fun_bindings,
            });

            let namespace_path = fun_info.pos.path.to_path_buf();
            let namespace = env.get_or_create_namespace(&namespace_path);

            return Ok(Some(StackFrame {
                namespace,
                caller_pos: Some(caller_expr.position.clone()),
                caller_expr_id: Some(caller_expr.id),
                bindings: Bindings {
                    block_bindings: bindings,
                },
                type_bindings,
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![Value::unit()],
                return_hint: fun_info.return_hint.clone(),
                enclosing_name: EnclosingSymbol::Closure,
                caller_uses_value: expr_value_is_used,
            }));
        }
        Value_::Fun {
            name_sym,
            fun_info:
                fi @ FunInfo {
                    params: ParenthesizedParameters { params, .. },
                    body,
                    ..
                },
            ..
        } => {
            // Calling a user-defined function.

            check_arity(
                &name_sym.name,
                &receiver_value,
                &caller_expr.position,
                params.len(),
                &arg_positions,
                &arg_values,
            )?;

            let enclosing_name = EnclosingSymbol::Fun(name_sym.clone());
            let is_self_call = enclosing_name == stack_frame.enclosing_name;

            // Always update prev_call_args, unless we're in a
            // self-recursive call, as the initial arguments are
            // generally more interesting.
            if !is_self_call {
                env.prev_call_args.insert(
                    (name_sym.name.clone(), name_sym.position.path.to_path_buf()),
                    arg_values.to_vec(),
                );
            }

            let mut type_bindings = TypeVarEnv::default();
            for param_sym in &fi.type_params {
                // TODO: calculate the value of type parameters properly.
                type_bindings.insert(param_sym.name.clone(), Some(Type::Top));
            }

            check_param_types(
                env,
                &receiver_value,
                params,
                &arg_positions,
                &arg_values,
                &type_bindings,
            )?;

            let mut fun_subexprs = vec![];
            for expr in body.exprs.iter().rev() {
                fun_subexprs.push((ExpressionState::NotEvaluated, expr.clone()));
            }

            let mut fun_bindings = FxHashMap::default();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                if !param.symbol.name.is_underscore() {
                    fun_bindings.insert(param.symbol.interned_id, value.clone());
                }
            }

            let namespace_path = fi.pos.path.to_path_buf();
            let namespace = env.get_or_create_namespace(&namespace_path);

            return Ok(Some(StackFrame {
                namespace,
                return_hint: fi.return_hint.clone(),
                caller_pos: Some(caller_expr.position.clone()),
                caller_expr_id: Some(caller_expr.id),
                enclosing_name,
                bindings: Bindings::new_with(fun_bindings),
                type_bindings,
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![Value::unit()],
                caller_uses_value: expr_value_is_used,
            }));
        }
        Value_::BuiltinFunction(kind, _, _) => eval_builtin_call(
            env,
            *kind,
            &receiver_value,
            &caller_expr.position,
            &arg_positions,
            &arg_values,
            expr_value_is_used,
            &caller_expr.position,
            session,
        )?,
        Value_::EnumConstructor {
            type_name,
            variant_idx,
            ..
        } => {
            check_arity(
                &SymbolName {
                    text: type_name.text.clone(),
                },
                &receiver_value,
                &caller_expr.position,
                1,
                &arg_positions,
                &arg_values,
            )?;

            let runtime_type = enum_value_runtime_type(
                env,
                type_name,
                *variant_idx,
                &Type::from_value(&arg_values[0]),
            )
            .unwrap_or(Type::no_value());

            let value = Value::new(Value_::EnumVariant {
                type_name: type_name.clone(),
                variant_idx: *variant_idx,
                // TODO: check type of arg_values[0] is compatible
                // with the declared type of the variant.
                payload: Some(Box::new(arg_values[0].clone())),
                runtime_type,
            });

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        _ => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            let message = format_type_error(
                &TypeName {
                    text: "Function".into(),
                },
                &receiver_value,
                env,
            );
            return Err((
                RestoreValues(saved_values),
                EvalError::Exception(caller_expr.position.clone(), message),
            ));
        }
    }

    Ok(None)
}

fn eval_assert(
    env: &mut Env,
    expr_value_is_used: bool,
    recv_expr: &Rc<Expression>,
) -> Result<(), (RestoreValues, EvalError)> {
    let receiver_value = env
        .pop_value()
        .expect("Popped an empty value stack for call receiver");

    // Unconditionally pop the LHS and RHS values, even if the assert
    // succeeds, so we maintain stack discipline.
    let subexpr_values = match binop_for_assert(recv_expr) {
        Some((_, kind, _)) => {
            let rhs_value = env
                .pop_value()
                .expect("Popped an empty value stack in assert");
            let lhs_value = env
                .pop_value()
                .expect("Popped an empty value stack in assert");

            Some((lhs_value, kind, rhs_value))
        }
        None => None,
    };

    if let Some(b) = receiver_value.as_rust_bool() {
        if !b {
            let message = match subexpr_values {
                Some((lhs_value, BinaryOperatorKind::Equal, rhs_value)) => {
                    // Convention: we the expected value is the RHS,
                    // so `assert(value == expected_value)`.
                    vec![
                        msgtext!("Expected "),
                        msgcode!("{}", rhs_value.display(env),),
                        msgtext!(" but got "),
                        msgcode!("{}", lhs_value.display(env),),
                        msgtext!("."),
                    ]
                }
                Some((lhs_value, kind, rhs_value)) => {
                    vec![
                        msgtext!("Assertion failed: "),
                        msgcode!(
                            "{} {} {}",
                            lhs_value.display(env),
                            kind,
                            rhs_value.display(env),
                        ),
                        msgtext!("."),
                    ]
                }
                None => vec![msgtext!("Assertion failed.")],
            };

            return Err((
                RestoreValues(vec![receiver_value]),
                EvalError::AssertionFailed(recv_expr.position.clone(), ErrorMessage(message)),
            ));
        }
    } else {
        let message = format_type_error(
            &TypeName {
                text: "Bool".into(),
            },
            &receiver_value,
            env,
        );
        return Err((
            RestoreValues(vec![receiver_value]),
            EvalError::Exception(recv_expr.position.clone(), message),
        ));
    }

    if expr_value_is_used {
        env.push_value(Value::unit());
    }

    Ok(())
}

/// Given an enum constructor, e.g. `Some`, return the function type
/// it represents (e.g. `T -> Option<T>` in this case).
fn enum_constructor_type(env: &Env, enum_info: &EnumInfo, payload_hint: &TypeHint) -> Type {
    let type_params = enum_info
        .type_params
        .iter()
        .map(|sym| sym.name.clone())
        .collect();

    let enum_type_param_names: HashSet<&TypeName> =
        enum_info.type_params.iter().map(|tp| &tp.name).collect();

    let arg_ty = if enum_type_param_names.contains(&payload_hint.sym.name) {
        // Enum variant payload is a generic type.
        Type::TypeParameter(payload_hint.sym.name.clone())
    } else {
        // Enum variant payload is a concrete type.
        Type::from_hint(payload_hint, &env.types, &env.stack.type_bindings()).unwrap_or_err_ty()
    };

    let type_args: Vec<Type> = enum_info
        .type_params
        .iter()
        .map(|tp| Type::TypeParameter(tp.name.clone()))
        .collect();

    let return_ = Type::UserDefined {
        kind: TypeDefKind::Enum,
        name: enum_info.name_sym.name.clone(),
        args: type_args,
    };

    Type::Fun {
        name_sym: None,
        type_params,
        params: vec![arg_ty],
        return_: Box::new(return_),
    }
}

fn enum_value_runtime_type(
    env: &Env,
    type_name: &TypeName,
    variant_idx: usize,
    payload_value_type: &Type,
) -> Option<Type> {
    let TypeDef::Enum(enum_info) = env.get_type_def(type_name)? else {
        return None;
    };

    let variant_info = enum_info.variants.get(variant_idx)?;

    if let Some(hint) = &variant_info.payload_hint {
        // If this variant has a payload whose type is a type
        // parameter, fill in that type argument accordingly.
        let mut args = vec![];
        for type_param in &enum_info.type_params {
            if type_param.name == hint.sym.name {
                args.push(payload_value_type.clone());
            } else {
                args.push(Type::no_value());
            }
        }

        Some(Type::UserDefined {
            kind: TypeDefKind::Enum,
            name: enum_info.name_sym.name.clone(),
            args,
        })
    } else {
        let args = vec![Type::no_value(); enum_info.type_params.len()];

        // This variant does not have a payload. Resolve all the
        // type parameters in this enum definition to NoValue.
        Some(Type::UserDefined {
            kind: TypeDefKind::Enum,
            name: enum_info.name_sym.name.clone(),
            args,
        })
    }
}

fn check_param_types(
    env: &Env,
    receiver_value: &Value,
    params: &[SymbolWithHint],
    arg_positions: &[Position],
    arg_values: &[Value],
    type_bindings: &TypeVarEnv,
) -> Result<(), (RestoreValues, EvalError)> {
    for (i, (param, arg_value)) in params.iter().zip(arg_values).enumerate() {
        if let Some(param_hint) = &param.hint {
            let param_ty = match Type::from_hint(param_hint, &env.types, type_bindings) {
                Ok(ty) => ty,
                Err(e) => {
                    return Err((
                        RestoreValues(vec![]),
                        EvalError::Exception(
                            arg_positions[i].clone(),
                            ErrorMessage(vec![msgtext!("Unbound type in hint: "), Code(e)]),
                        ),
                    ));
                }
            };

            if let Err(msg) = check_type(arg_value, &param_ty, env) {
                let mut saved_values = vec![];
                saved_values.push(receiver_value.clone());
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(arg_positions[i].clone(), msg),
                ));
            }
        }
    }

    Ok(())
}

/// Evaluate a method call.
///
/// If we're calling a userland method, return the new stackframe to
/// evaluate next.
fn eval_method_call(
    env: &mut Env,
    expr_value_is_used: bool,
    caller_expr: Rc<Expression>,
    meth_name: &Symbol,
    paren_args: &ParenthesizedArguments,
) -> Result<Option<StackFrame>, (RestoreValues, EvalError)> {
    let mut arg_values: Vec<Value> = Vec::with_capacity(paren_args.arguments.len());
    let mut arg_positions: Vec<Position> = Vec::with_capacity(paren_args.arguments.len());
    for arg in &paren_args.arguments {
        arg_values.push(
            env.pop_value()
                .expect("Popped an empty value for stack for method call arguments."),
        );
        arg_positions.push(arg.expr.position.clone());
    }
    let receiver_value = env
        .pop_value()
        .expect("Popped an empty value stack for method call receiver.");

    let receiver_type_name = type_representation(&receiver_value);

    let prev_meth_calls_for_ty = env
        .prev_method_call_args
        .entry(receiver_type_name.clone())
        .or_default();

    if !prev_meth_calls_for_ty.contains_key(&meth_name.name) {
        prev_meth_calls_for_ty.insert(
            meth_name.name.clone(),
            (receiver_value.clone(), arg_values.clone()),
        );
    }

    let Some(receiver_type_and_methods) = env.types.get(&receiver_type_name) else {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        return Err((
            RestoreValues(saved_values),
            EvalError::Exception(
                meth_name.position.clone(),
                ErrorMessage(vec![Text(format!(
                    "No methods defined on `{receiver_type_name}`."
                ))]),
            ),
        ));
    };

    let Some(receiver_method) = receiver_type_and_methods.methods.get(&meth_name.name) else {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        return Err((
            RestoreValues(saved_values),
            EvalError::Exception(
                meth_name.position.clone(),
                ErrorMessage(vec![
                    msgtext!("There is no method named "),
                    msgcode!("{}", meth_name.name),
                    msgtext!(" on values of type "),
                    msgcode!("{}", receiver_type_name),
                    msgtext!(". This value is "),
                    msgcode!("{}", receiver_value.display(env)),
                    msgtext!("."),
                ]),
            ),
        ));
    };

    let fun_info = match &receiver_method.kind {
        MethodKind::BuiltinMethod(kind, _) => {
            eval_builtin_method_call(
                env,
                *kind,
                &receiver_value,
                &caller_expr.position,
                &arg_positions,
                &arg_values,
                expr_value_is_used,
            )?;
            return Ok(None);
        }
        MethodKind::UserDefinedMethod(fun_info) => fun_info,
    };

    let mut method_subexprs = vec![];
    for expr in fun_info.body.exprs.iter().rev() {
        method_subexprs.push((ExpressionState::NotEvaluated, expr.clone()));
    }

    // TODO: use a fully qualified method name here?
    check_arity(
        &meth_name.name,
        &receiver_value,
        &caller_expr.position,
        fun_info.params.params.len(),
        &arg_positions,
        &arg_values,
    )?;

    // TODO: check for duplicate parameter names.
    // TODO: parameter names must not clash with the receiver name.
    let mut fun_bindings: FxHashMap<InternedSymbolId, Value> = FxHashMap::default();
    for (param, value) in fun_info.params.params.iter().zip(arg_values.iter()) {
        fun_bindings.insert(param.symbol.interned_id, value.clone());
    }
    fun_bindings.insert(receiver_method.receiver_sym.interned_id, receiver_value);

    let mut type_bindings = TypeVarEnv::default();
    for type_param in &fun_info.type_params {
        // TODO: compute the value of these type params properly.
        type_bindings.insert(type_param.name.clone(), Some(Type::Top));
    }

    let return_hint = fun_info.return_hint.clone();

    let namespace_path = fun_info.pos.path.to_path_buf();
    let namespace = env.get_or_create_namespace(&namespace_path);

    Ok(Some(StackFrame {
        namespace,
        return_hint,
        enclosing_name: EnclosingSymbol::Method(receiver_type_name, meth_name.clone()),
        caller_pos: Some(caller_expr.position.clone()),
        caller_expr_id: Some(caller_expr.id),
        bindings: Bindings::new_with(fun_bindings),
        type_bindings,
        bindings_next_block: vec![],
        exprs_to_eval: method_subexprs,
        evalled_values: vec![Value::unit()],
        caller_uses_value: expr_value_is_used,
    }))
}

fn unwrap_path(value: &Value, env: &Env) -> Result<String, ErrorMessage> {
    let Value_::Struct {
        type_name, fields, ..
    } = value.as_ref()
    else {
        return Err(format_type_error(
            &TypeName {
                text: "Path".into(),
            },
            value,
            env,
        ));
    };

    if type_name.text != "Path" {
        return Err(format_type_error(
            &TypeName {
                text: "Path".into(),
            },
            value,
            env,
        ));
    }

    let Some((field_name, field_value)) = fields.first() else {
        return Err(ErrorMessage(vec![Text(
            "Malformed `Path` struct (expected some fields).".to_owned(),
        )]));
    };
    if field_name.text != "p" {
        return Err(ErrorMessage(vec![Text(format!(
            "Malformed `Path` struct (expected a field named `p`, got `{field_name}`).",
        ))]));
    }

    match field_value.as_ref() {
        Value_::String(s) => Ok(s.clone()),
        _ => Err(ErrorMessage(vec![Text(format!(
            "Malformed `Path` struct (expected `p` to contain a string, got `{}`).",
            field_value.display(env),
        ))])),
    }
}

fn eval_builtin_method_call(
    env: &mut Env,
    kind: BuiltinMethodKind,
    receiver_value: &Value,
    receiver_pos: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    expr_value_is_used: bool,
) -> Result<(), (RestoreValues, EvalError)> {
    match kind {
        BuiltinMethodKind::ListAppend => {
            check_arity(
                &SymbolName {
                    text: "List::append".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match receiver_value.as_ref() {
                Value_::List { items, .. } => {
                    let mut new_items = items.clone();
                    new_items.push(arg_values[0].clone());

                    if expr_value_is_used {
                        let elem_type = Type::from_value(&arg_values[0]);

                        // TODO: check that the new value has the same
                        // type as the existing list items.
                        env.push_value(Value::new(Value_::List {
                            items: new_items,
                            elem_type,
                        }));
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            receiver_pos.clone(),
                            format_type_error(
                                &TypeName {
                                    text: "List".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::ListContains => {
            check_arity(
                &SymbolName {
                    text: "List::contains".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let needle = &arg_values[0];
            match receiver_value.as_ref() {
                Value_::List { items, .. } => {
                    let mut present = false;
                    for item in items {
                        if item == needle {
                            present = true;
                            break;
                        }
                    }

                    if expr_value_is_used {
                        env.push_value(Value::bool(present));
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "List".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::ListGet => {
            check_arity(
                &SymbolName {
                    text: "List::get".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match (receiver_value.as_ref(), arg_values[0].as_ref()) {
                (Value_::List { items, .. }, Value_::Integer(i)) => {
                    let v = if *i >= items.len() as i64 || *i < 0 {
                        Value::none()
                    } else {
                        Value::some(items[*i as usize].clone())
                    };

                    if expr_value_is_used {
                        env.push_value(v);
                    }
                }
                (_, Value_::Integer(_)) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "List".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
                (_, _) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[1].clone(),
                            format_type_error(
                                &TypeName { text: "Int".into() },
                                &arg_values[0],
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::ListLen => {
            check_arity(
                &SymbolName {
                    text: "List::len".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match receiver_value.as_ref() {
                Value_::List { items, .. } => {
                    if expr_value_is_used {
                        env.push_value(Value::new(Value_::Integer(items.len() as i64)));
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "List".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::PathExists => {
            if env.enforce_sandbox {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ForbiddenInSandbox(receiver_pos.clone()),
                ));
            }

            check_arity(
                &SymbolName {
                    text: "Path::exists".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            let path_s = match unwrap_path(receiver_value, env) {
                Ok(s) => s,
                Err(msg) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(receiver_pos.clone(), msg),
                    ));
                }
            };

            if expr_value_is_used {
                let path = PathBuf::from(path_s);
                env.push_value(Value::bool(path.exists()));
            }
        }
        BuiltinMethodKind::PathRead => {
            if env.enforce_sandbox {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ForbiddenInSandbox(receiver_pos.clone()),
                ));
            }

            check_arity(
                &SymbolName {
                    text: "Path::read".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            let path_s = match unwrap_path(receiver_value, env) {
                Ok(s) => s,
                Err(msg) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(receiver_pos.clone(), msg),
                    ));
                }
            };

            let path = PathBuf::from(path_s.clone());

            let v = match std::fs::read_to_string(path) {
                Ok(s) => Value::ok(Value::new(Value_::String(s))),
                Err(e) => Value::err(Value::new(Value_::String(format!("{e} {path_s}")))),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinMethodKind::StringChars => {
            check_arity(
                &SymbolName {
                    text: "String::chars".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match receiver_value.as_ref() {
                Value_::String(s) => {
                    let mut items = vec![];
                    for (_, c) in s.char_indices() {
                        items.push(Value::new(Value_::String(format!("{c}"))));
                    }
                    let chars_list = Value::new(Value_::List {
                        items,
                        elem_type: Type::string(),
                    });

                    if expr_value_is_used {
                        env.push_value(chars_list);
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::StringIndexOf => {
            check_arity(
                &SymbolName {
                    text: "String::index_of".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let receiver_s = match receiver_value.as_ref() {
                Value_::String(s) => s.clone(),
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            };
            let arg_s = match arg_values[0].as_ref() {
                Value_::String(s) => s,
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                &arg_values[0],
                                env,
                            ),
                        ),
                    ));
                }
            };

            let mut value = Value::none();
            if let Some(needle_byte_offset) = receiver_s.find(arg_s) {
                for (i, (byte_offset, _)) in receiver_s.char_indices().enumerate() {
                    if byte_offset == needle_byte_offset {
                        value = Value::some(Value::new(Value_::Integer(i as i64)));
                        break;
                    }
                }
            }

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        BuiltinMethodKind::StringJoin => {
            check_arity(
                &SymbolName {
                    text: "String::join".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let Value_::String(receiver_s) = receiver_value.as_ref() else {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(
                        arg_positions[0].clone(),
                        format_type_error(
                            &TypeName {
                                text: "String".into(),
                            },
                            receiver_value,
                            env,
                        ),
                    ),
                ));
            };

            let items = &arg_values[0];
            let Value_::List { items, elem_type } = items.as_ref() else {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(
                        arg_positions[0].clone(),
                        format_type_error(
                            &TypeName {
                                text: "List".into(),
                            },
                            receiver_value,
                            env,
                        ),
                    ),
                ));
            };

            if !is_subtype(elem_type, &Type::string()) {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(
                        arg_positions[0].clone(),
                        format_type_error("List<String>", &arg_values[0], env),
                    ),
                ));
            }

            let mut joined_str = String::new();
            for (i, item) in items.iter().enumerate() {
                let Value_::String(item_s) = item.as_ref() else {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                item,
                                env,
                            ),
                        ),
                    ));
                };

                if i != 0 {
                    joined_str.push_str(receiver_s);
                }
                joined_str.push_str(item_s);
            }

            if expr_value_is_used {
                env.push_value(Value::new(Value_::String(joined_str)));
            }
        }
        BuiltinMethodKind::StringLen => {
            check_arity(
                &SymbolName {
                    text: "String::len".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match receiver_value.as_ref() {
                Value_::String(s) => {
                    if expr_value_is_used {
                        env.push_value(Value::new(Value_::Integer(s.chars().count() as i64)));
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::StringLines => {
            check_arity(
                &SymbolName {
                    text: "String::lines".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match receiver_value.as_ref() {
                Value_::String(s) => {
                    let lines = s
                        .lines()
                        .map(|line| Value::new(Value_::String(line.to_owned())))
                        .collect::<Vec<_>>();

                    let elem_type = if lines.is_empty() {
                        Type::no_value()
                    } else {
                        Type::string()
                    };

                    if expr_value_is_used {
                        env.push_value(Value::new(Value_::List {
                            items: lines,
                            elem_type,
                        }));
                    }
                }
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::StringSubstring => {
            check_arity(
                &SymbolName {
                    text: "String::substring".to_owned(),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            let s_arg = match receiver_value.as_ref() {
                Value_::String(s) => s,
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    text: "String".into(),
                                },
                                receiver_value,
                                env,
                            ),
                        ),
                    ));
                }
            };
            let from_arg = match arg_values[0].as_ref() {
                Value_::Integer(i) => i,
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[1].clone(),
                            format_type_error(
                                &TypeName { text: "Int".into() },
                                &arg_values[0],
                                env,
                            ),
                        ),
                    ));
                }
            };
            let to_arg = match arg_values[1].as_ref() {
                Value_::Integer(i) => i,
                _ => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::Exception(
                            arg_positions[2].clone(),
                            format_type_error(
                                &TypeName { text: "Int".into() },
                                &arg_values[1],
                                env,
                            ),
                        ),
                    ));
                }
            };

            if *from_arg < 0 {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(
                        arg_positions[0].clone(),
                        ErrorMessage(vec![
                            msgtext!("The first argument to "),
                            msgcode!("String::substring"),
                            msgtext!(" must be greater than 0, but got {}.", from_arg),
                        ]),
                    ),
                ));
            }

            if from_arg > to_arg {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                let s_len = s_arg.chars().count();
                return Err((
                    RestoreValues(saved_values),
                    EvalError::Exception(
                        arg_positions[1].clone(),
                        ErrorMessage(vec![
                            msgtext!("The first argument ({}) to ", from_arg),
                            msgcode!("String::substring"),
                            msgtext!(" cannot be greater than than the second ({}). ", to_arg),
                            msgtext!(
                                "The string itself is {} character{} long.",
                                s_len,
                                if s_len == 1 { "" } else { "s" }
                            ),
                        ]),
                    ),
                ));
            }

            if expr_value_is_used {
                env.push_value(Value::new(Value_::String(
                    s_arg
                        .chars()
                        .skip(*from_arg as usize)
                        .take((to_arg - from_arg) as usize)
                        .collect(),
                )));
            }
        }
    }

    Ok(())
}

/// Evaluate `outer_expr`. If it's a function call, return the new
/// stack frame.
fn eval_expr(
    env: &mut Env,
    session: &Session,
    outer_expr: Rc<Expression>,
    expr_state: &mut ExpressionState,
) -> Result<Option<StackFrame>, (RestoreValues, EvalError)> {
    let expr_position = outer_expr.position.clone();
    let expr_value_is_used =
        outer_expr.value_is_used || env.stop_at_expr_id.as_ref() == Some(&outer_expr.id);

    match &outer_expr.expr_ {
        Expression_::Match(scrutinee, cases) => match expr_state {
            ExpressionState::NotEvaluated => {
                env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, scrutinee.clone());
            }
            ExpressionState::PartiallyEvaluated => {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                eval_match_cases(env, expr_value_is_used, &scrutinee.position, cases)
                    .map_err(|e| (RestoreValues(vec![]), e))?;
            }
            ExpressionState::EvaluatedAllSubexpressions => {
                env.current_frame_mut().bindings.pop_block();
            }
        },
        Expression_::If(condition, ref then_body, ref else_body) => match expr_state {
            ExpressionState::NotEvaluated => {
                env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, condition.clone());
            }
            ExpressionState::PartiallyEvaluated => {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                eval_if(
                    env,
                    expr_value_is_used,
                    &condition.position,
                    then_body,
                    else_body.as_ref(),
                )?;
            }
            ExpressionState::EvaluatedAllSubexpressions => {
                env.current_frame_mut().bindings.pop_block();
            }
        },
        Expression_::While(condition, ref body) => {
            match expr_state {
                ExpressionState::NotEvaluated => {
                    // Once we've evaluated the condition, we can consider evaluating the body.
                    env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());
                    // Evaluate the loop condition first.
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, condition.clone());
                }
                ExpressionState::PartiallyEvaluated => {
                    // Evaluated condition, can possibly evaluate body.
                    eval_while_body(
                        env,
                        expr_value_is_used,
                        &condition.position,
                        outer_expr.clone(),
                        body,
                    )?;
                }
                ExpressionState::EvaluatedAllSubexpressions => {
                    env.current_frame_mut().bindings.pop_block();

                    // Done condition and body, nothing left to do.
                    if expr_value_is_used {
                        env.push_value(Value::unit());
                    }
                }
            }
        }
        Expression_::ForIn(sym, expr, body) => {
            match expr_state {
                ExpressionState::NotEvaluated => {
                    // The initial value of the loop index.
                    env.push_value(Value::new(Value_::Integer(0)));

                    env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());

                    // Next, we're going to evaluate the value
                    // that we want to iterate over.
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
                }
                ExpressionState::PartiallyEvaluated => {
                    eval_for_in(env, sym, &expr.position, outer_expr.clone(), body)?;
                }
                ExpressionState::EvaluatedAllSubexpressions => {
                    let stack_frame = env.current_frame_mut();
                    stack_frame.bindings.pop_block();

                    // We've finished this `for` loop.
                    if expr_value_is_used {
                        env.push_value(Value::unit());
                    }
                }
            }
        }
        Expression_::Return(expr) => {
            if expr_state.done_children() {
                // No more expressions to evaluate in this function, we're returning.
                let stack_frame = env.current_frame_mut();
                stack_frame.exprs_to_eval.clear();
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                if let Some(expr) = expr {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
                } else {
                    // `return` is the same as `return Unit`.
                    env.push_value(Value::unit());
                }
            }
        }
        Expression_::Assign(variable, expr) => {
            if expr_state.done_children() {
                eval_assign(env, expr_value_is_used, variable)?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
            }
        }
        Expression_::AssignUpdate(variable, op, expr) => {
            if expr_state.done_children() {
                eval_assign_update(env, expr_value_is_used, &expr_position, variable, *op)?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
            }
        }
        Expression_::Let(destination, hint, expr) => {
            if expr_state.done_children() {
                eval_let(env, expr_value_is_used, destination, &expr.position, hint)?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
            }
        }
        Expression_::IntLiteral(i) => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            if expr_value_is_used {
                env.push_value(Value::new(Value_::Integer(*i)));
            }
        }
        Expression_::StringLiteral(s) => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            if expr_value_is_used {
                env.push_value(Value::new(Value_::String(s.clone())));
            }
        }
        Expression_::ListLiteral(items) => {
            if expr_state.done_children() {
                let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                let mut element_type = Type::no_value();

                for _ in 0..items.len() {
                    let element = env
                        .pop_value()
                        .expect("Value stack should have sufficient items for the list literal");
                    // TODO: check that all elements are of a compatible type.
                    // [1, None] should be an error.
                    element_type = Type::from_value(&element);
                    list_values.push(element);
                }

                if expr_value_is_used {
                    env.push_value(Value::new(Value_::List {
                        items: list_values,
                        elem_type: element_type,
                    }));
                }
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                for item in items.iter() {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, item.expr.clone());
                }
            }
        }
        Expression_::TupleLiteral(items) => {
            if expr_state.done_children() {
                let mut items_values: Vec<Value> = Vec::with_capacity(items.len());
                let mut item_types: Vec<Type> = Vec::with_capacity(items.len());

                for _ in 0..items.len() {
                    let element = env
                        .pop_value()
                        .expect("Value stack should have sufficient items for the tuple literal");

                    item_types.push(Type::from_value(&element));
                    items_values.push(element);
                }

                if expr_value_is_used {
                    env.push_value(Value::new(Value_::Tuple {
                        items: items_values,
                        item_types,
                    }));
                }
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                for item in items.iter() {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, item.clone());
                }
            }
        }
        Expression_::StructLiteral(type_sym, field_exprs) => {
            if expr_state.done_children() {
                eval_struct_value(
                    env,
                    &outer_expr.position,
                    expr_value_is_used,
                    type_sym.clone(),
                    field_exprs,
                )?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                for (_, field_expr) in field_exprs.iter() {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, field_expr.clone());
                }
            }
        }
        Expression_::Variable(name_sym) => {
            if let Some(value) = get_var(name_sym, env) {
                *expr_state = ExpressionState::EvaluatedAllSubexpressions;

                if expr_value_is_used {
                    env.push_value(value);
                }
            } else {
                let mut message = vec![
                    msgtext!("No such variable "),
                    msgcode!("{}", name_sym.name),
                    msgtext!("."),
                ];

                if let Some(closest_name) = most_similar_var(&name_sym.name, env) {
                    message.push(msgtext!(" Did you mean "));
                    message.push(msgcode!("{}", closest_name));
                    message.push(msgtext!(" instead?"));
                }

                return Err((
                    RestoreValues(vec![]),
                    EvalError::Exception(name_sym.position.clone(), ErrorMessage(message)),
                ));
            }
        }
        Expression_::BinaryOperator(
            lhs,
            op @ (BinaryOperatorKind::Add
            | BinaryOperatorKind::Subtract
            | BinaryOperatorKind::Multiply
            | BinaryOperatorKind::Divide
            | BinaryOperatorKind::Modulo
            | BinaryOperatorKind::Exponent
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEqual
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEqual),
            rhs,
        ) => {
            if expr_state.done_children() {
                eval_integer_binop(
                    env,
                    expr_value_is_used,
                    &expr_position,
                    &lhs.position,
                    &rhs.position,
                    *op,
                )?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, rhs.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, lhs.clone());
            }
        }
        Expression_::BinaryOperator(
            lhs,
            op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
            rhs,
        ) => {
            if expr_state.done_children() {
                eval_equality_binop(env, expr_value_is_used, *op)
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, rhs.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, lhs.clone());
            }
        }
        Expression_::BinaryOperator(
            lhs,
            op @ (BinaryOperatorKind::And | BinaryOperatorKind::Or),
            rhs,
        ) => {
            if expr_state.done_children() {
                eval_boolean_binop(env, expr_value_is_used, &lhs.position, &rhs.position, *op)?;
            } else {
                // TODO: do short-circuit evaluation of && and ||.
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, rhs.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, lhs.clone());
            }
        }
        Expression_::BinaryOperator(lhs, BinaryOperatorKind::StringConcat, rhs) => {
            if expr_state.done_children() {
                eval_string_concat(env, expr_value_is_used, &lhs.position, &rhs.position)?;
            } else {
                // TODO: do short-circuit evaluation of && and ||.
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, rhs.clone());
                env.push_expr_to_eval(ExpressionState::NotEvaluated, lhs.clone());
            }
        }
        Expression_::FunLiteral(fun_info) => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;

            if expr_value_is_used {
                let stack_frame = env.current_frame_mut();
                let bindings = stack_frame.bindings.block_bindings.clone();
                let runtime_type =
                    Type::from_fun_info(fun_info, &env.types, &env.stack.type_bindings())
                        .unwrap_or_err_ty();

                env.push_value(Value::new(Value_::Closure(
                    bindings,
                    fun_info.clone(),
                    runtime_type,
                )));
            }
        }
        Expression_::Call(receiver, paren_args) => match expr_state {
            ExpressionState::NotEvaluated => {
                env.push_expr_to_eval(ExpressionState::PartiallyEvaluated, outer_expr.clone());

                env.push_expr_to_eval(ExpressionState::NotEvaluated, receiver.clone());
            }
            ExpressionState::PartiallyEvaluated => {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                for arg in &paren_args.arguments {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, arg.expr.clone());
                }
            }
            ExpressionState::EvaluatedAllSubexpressions => {
                match eval_call(
                    env,
                    expr_value_is_used,
                    outer_expr.clone(),
                    paren_args,
                    session,
                )? {
                    Some(new_stack_frame) => {
                        return Ok(Some(new_stack_frame));
                    }
                    None => {}
                }
            }
        },
        Expression_::MethodCall(receiver_expr, meth_name, paren_args) => {
            if expr_state.done_children() {
                match eval_method_call(
                    env,
                    expr_value_is_used,
                    outer_expr.clone(),
                    meth_name,
                    paren_args,
                )? {
                    Some(new_stack_frame) => {
                        return Ok(Some(new_stack_frame));
                    }
                    None => {}
                }
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );

                for arg in &paren_args.arguments {
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, arg.expr.clone());
                }
                // Push the receiver after arguments, so
                // we evaluate it before arguments.
                env.push_expr_to_eval(ExpressionState::NotEvaluated, receiver_expr.clone());
            }
        }
        Expression_::DotAccess(recv, sym) => {
            if expr_state.done_children() {
                eval_dot_access(env, expr_value_is_used, sym, &recv.position)?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, recv.clone());
            }
        }
        Expression_::NamespaceAccess(recv, sym) => {
            if expr_state.done_children() {
                eval_namespace_access(env, expr_value_is_used, sym, &recv.position)?;
            } else {
                env.push_expr_to_eval(
                    ExpressionState::EvaluatedAllSubexpressions,
                    outer_expr.clone(),
                );
                env.push_expr_to_eval(ExpressionState::NotEvaluated, recv.clone());
            }
        }
        Expression_::Break => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            eval_break(env, expr_value_is_used);
        }
        Expression_::Continue => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            eval_continue(env);
        }
        Expression_::Parentheses(_, expr, _) => {
            env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
        }
        Expression_::Invalid => {
            return Err((RestoreValues(vec![]),
                        (EvalError::Exception(expr_position,
                                                   ErrorMessage(vec![msgtext!("Tried to evaluate a syntactically invalid expression. Check your code parses correctly.")])))));
        }
        Expression_::Assert(expr) => {
            match expr_state {
                ExpressionState::NotEvaluated => match binop_for_assert(expr) {
                    Some((lhs, _, rhs)) => {
                        // Intercept evaluation of LHS and RHS, so we
                        // can show their values if we show an
                        // assertion failure message.

                        env.push_expr_to_eval(
                            ExpressionState::PartiallyEvaluated,
                            outer_expr.clone(),
                        );

                        env.push_expr_to_eval(ExpressionState::NotEvaluated, rhs.clone());
                        env.push_expr_to_eval(ExpressionState::NotEvaluated, lhs.clone());
                    }
                    None => {
                        env.push_expr_to_eval(
                            ExpressionState::EvaluatedAllSubexpressions,
                            outer_expr.clone(),
                        );

                        env.push_expr_to_eval(ExpressionState::NotEvaluated, expr.clone());
                    }
                },
                ExpressionState::PartiallyEvaluated => {
                    // Duplicate the LHS and RHS values.

                    let rhs_value = env
                        .pop_value()
                        .expect("Popped an empty value stack for RHS of binary operator");
                    let lhs_value = env
                        .pop_value()
                        .expect("Popped an empty value stack for LHS of binary operator");

                    env.push_value(lhs_value.clone());
                    env.push_value(rhs_value.clone());
                    env.push_value(lhs_value);
                    env.push_value(rhs_value);

                    env.push_expr_to_eval(
                        ExpressionState::EvaluatedAllSubexpressions,
                        outer_expr.clone(),
                    );

                    env.push_expr_to_eval(
                        ExpressionState::EvaluatedAllSubexpressions,
                        expr.clone(),
                    );
                }
                ExpressionState::EvaluatedAllSubexpressions => {
                    eval_assert(env, expr_value_is_used, expr)?;
                }
            }
        }
    }

    Ok(None)
}

fn binop_for_assert(
    expr: &Rc<Expression>,
) -> Option<(Rc<Expression>, BinaryOperatorKind, Rc<Expression>)> {
    match &expr.expr_ {
        Expression_::BinaryOperator(lhs, op_kind, rhs) => {
            Some((lhs.clone(), *op_kind, rhs.clone()))
        }
        _ => None,
    }
}

pub(crate) fn eval(env: &mut Env, session: &Session) -> Result<Value, EvalError> {
    if env.stack.0.len() == 1 && env.current_frame().exprs_to_eval.is_empty() {
        // We expect to evaluate a non-zero number of expressions, so
        // we have values pushed to the value stack. This isn't true
        // when running :resume at the toplevel, so return early.
        return Ok(Value::unit());
    }

    loop {
        if let Some((mut expr_state, outer_expr)) = env.current_frame_mut().exprs_to_eval.pop() {
            env.ticks += 1;

            if session.interrupted.load(Ordering::SeqCst) {
                session.interrupted.store(false, Ordering::SeqCst);
                restore_stack_frame(env, (expr_state, outer_expr), &[]);
                return Err(EvalError::Interrupted);
            }

            if let Some(tick_limit) = env.tick_limit {
                if env.ticks >= tick_limit {
                    let position = outer_expr.position.clone();
                    restore_stack_frame(env, (expr_state, outer_expr), &[]);
                    return Err(EvalError::ReachedTickLimit(position));
                }
            }

            if session.trace_exprs {
                println!("{:?} {:?}\n", &outer_expr.expr_, expr_state);
            }

            // Print the whole call stack every 10,000 ticks if the
            // environment variable GDN_PROFILE is set, to enable
            // basic profiling.
            if env.ticks % 10_000 == 0 && std::env::var("GDN_PROFILE").is_ok() {
                for (i, frame) in env.stack.0.iter().enumerate() {
                    print!(
                        "{}{}",
                        if i == 0 { "" } else { " > " },
                        frame.enclosing_name
                    );
                }
                println!();
            }

            match eval_expr(env, session, outer_expr.clone(), &mut expr_state) {
                Err((RestoreValues(restore_values), eval_err)) => {
                    restore_stack_frame(env, (expr_state, outer_expr.clone()), &restore_values);
                    return Err(eval_err);
                }
                Ok(Some(new_stack_frame)) => {
                    env.stack.0.push(new_stack_frame);
                    continue;
                }
                Ok(None) => {}
            }

            // If we've just finished evaluating the expression that
            // we were requested to stop at, return that value
            // immediately.
            //
            if env.stop_at_expr_id.is_some() && env.stop_at_expr_id.as_ref() == Some(&outer_expr.id)
            {
                if expr_state.done_children() {
                    let v = if let Some(value) = env.current_frame().evalled_values.last().cloned()
                    {
                        value
                    } else {
                        // TODO: this should probably be an Err() case.
                        Value::new(Value_::String(
                            "__ERROR: no expressions evaluated. This is a bug.".to_owned(),
                        ))
                    };

                    return Ok(v);
                }

                // `for x in y { z }` loops are a special case. We
                // want to evaluate `y`, enter the block, then stop
                // evaluation, so we know the first value of `x`.
                if matches!(outer_expr.expr_, Expression_::ForIn(_, _, _))
                    && matches!(expr_state, ExpressionState::PartiallyEvaluated)
                {
                    return Ok(Value::unit());
                }
            }
        } else {
            // No more expressions in this stack frame.
            if env.stack.0.len() == 1 {
                // Don't pop the outer scope: that's for the top level environment. We're done.
                break;
            }

            // Check that the value matches the return type.
            let return_value = env.pop_value().expect("Should have a value");

            let type_bindings = env.current_frame().type_bindings.clone();
            if let Some(return_hint) = &env.current_frame().return_hint {
                let err_pos = return_hint.position.clone();

                let return_ty = match Type::from_hint(return_hint, &env.types, &type_bindings) {
                    Ok(ty) => ty,
                    Err(e) => {
                        return Err(EvalError::Exception(err_pos, ErrorMessage(vec![Text(e)])));
                    }
                };

                if let Err(msg) = check_type(&return_value, &return_ty, env) {
                    env.push_value(return_value.clone());
                    return Err(EvalError::Exception(err_pos, msg));
                }
            }

            // We've just finished evaluating a call and we were
            // requested to stop at this call expression and return
            // that result.
            if env.current_frame().caller_expr_id.is_some()
                && env.stop_at_expr_id == env.current_frame().caller_expr_id
            {
                env.stack.0.pop();
                return Ok(return_value);
            }

            let caller_uses_value = env.current_frame().caller_uses_value;

            // Stack frame is now done.
            env.stack.0.pop();

            // The final evaluation result of the function
            // call should be used in the previous stack
            // frame.
            if caller_uses_value {
                env.push_value(return_value);
            }
        }
    }

    Ok(env
        .pop_value()
        .expect("Should have a value from the last expression"))
}

fn eval_block(env: &mut Env, expr_value_is_used: bool, block: &Block) {
    let stack_frame = env.current_frame_mut();
    stack_frame.bindings.push_block();

    let bindings_next_block = std::mem::take(&mut stack_frame.bindings_next_block);
    for (sym, expr) in bindings_next_block {
        stack_frame.bindings.add_new(&sym, expr);
    }

    for expr in block.exprs.iter().rev() {
        let mut expr = expr.as_ref().clone();

        if !expr_value_is_used {
            expr.value_is_used = false;
        }

        env.push_expr_to_eval(ExpressionState::NotEvaluated, Rc::new(expr));
    }
}

fn eval_break(env: &mut Env, expr_value_is_used: bool) {
    // Pop all the currently evaluating expressions until we are no
    // longer inside the innermost loop.
    while let Some((expr_state, expr)) = env.current_frame_mut().exprs_to_eval.pop() {
        if matches!(
            expr.expr_,
            Expression_::While(_, _) | Expression_::ForIn(_, _, _)
        ) && matches!(
            expr_state,
            ExpressionState::NotEvaluated | ExpressionState::PartiallyEvaluated
        ) {
            break;
        }
    }

    // Loops always evaluate to unit.
    if expr_value_is_used {
        env.push_value(Value::unit());
    }
}

fn eval_continue(env: &mut Env) {
    // Pop all the currently evaluating expressions until we are back
    // at the loop.
    while let Some((expr_state, expr)) = env.current_frame_mut().exprs_to_eval.pop() {
        if matches!(
            expr.expr_,
            Expression_::While(_, _) | Expression_::ForIn(_, _, _)
        ) {
            env.push_expr_to_eval(expr_state, expr);
            break;
        }
    }
}

fn eval_namespace_access(
    env: &mut Env,
    expr_value_is_used: bool,
    symbol: &Symbol,
    recv_pos: &Position,
) -> Result<(), (RestoreValues, EvalError)> {
    let recv_value = env
        .pop_value()
        .expect("Popped an empty value when evaluating namespace access");

    match recv_value.as_ref() {
        Value_::Namespace(ns) => {
            let ns = ns.borrow();

            match ns.values.get(&symbol.name) {
                Some(v) => {
                    if !ns.external_syms.contains(&symbol.name) {
                        return Err((
                            RestoreValues(vec![recv_value.clone()]),
                            EvalError::Exception(
                                symbol.position.clone(),
                                ErrorMessage(vec![
                                    msgcode!("{}", symbol.name),
                                    msgtext!(" is not marked as "),
                                    msgcode!("external"),
                                    msgtext!(" in "),
                                    msgcode!("{}", env.relative_to_project(&ns.abs_path).display()),
                                    msgtext!(". "),
                                ]),
                            ),
                        ));
                    }

                    if expr_value_is_used {
                        env.push_value(v.clone());
                    }
                }
                None => {
                    return Err((
                        RestoreValues(vec![recv_value.clone()]),
                        EvalError::Exception(
                            symbol.position.clone(),
                            ErrorMessage(vec![
                                msgtext!("Namespace "),
                                msgcode!("{}", env.relative_to_project(&ns.abs_path).display()),
                                msgtext!(" does not contain a function named "),
                                msgcode!("{}", symbol.name),
                                msgtext!(". "),
                            ]),
                        ),
                    ));
                }
            }
        }
        _ => {
            return Err((
                RestoreValues(vec![recv_value.clone()]),
                EvalError::Exception(
                    recv_pos.clone(),
                    format_type_error("namespace", &recv_value, env),
                ),
            ));
        }
    }

    Ok(())
}

fn eval_dot_access(
    env: &mut Env,
    expr_value_is_used: bool,
    symbol: &Symbol,
    recv_pos: &Position,
) -> Result<(), (RestoreValues, EvalError)> {
    let recv_value = env
        .pop_value()
        .expect("Popped an empty value when evaluating dot access");

    match recv_value.as_ref() {
        Value_::Struct { ref fields, .. } => {
            let mut found = false;

            for (field_name, field_value) in fields {
                if *field_name == symbol.name {
                    if expr_value_is_used {
                        env.push_value(field_value.clone());
                    }

                    found = true;
                    break;
                }
            }

            if !found {
                return Err((
                    RestoreValues(vec![recv_value]),
                    EvalError::Exception(
                        symbol.position.clone(),
                        ErrorMessage(vec![Text(format!(
                            "This struct has no field named `{}`.",
                            symbol.name
                        ))]),
                    ),
                ));
            }
        }
        _ => {
            return Err((
                RestoreValues(vec![recv_value.clone()]),
                EvalError::Exception(
                    recv_pos.clone(),
                    format_type_error("struct", &recv_value, env),
                ),
            ));
        }
    }

    Ok(())
}

fn eval_struct_value(
    env: &mut Env,
    outer_expr_pos: &Position,
    expr_value_is_used: bool,
    type_symbol: TypeSymbol,
    field_exprs: &[(Symbol, Rc<Expression>)],
) -> Result<(), (RestoreValues, EvalError)> {
    let Some(type_info) = env.get_type_def(&type_symbol.name) else {
        return Err((
            RestoreValues(vec![]),
            EvalError::Exception(
                type_symbol.position.clone(),
                ErrorMessage(vec![Text(format!(
                    "No type exists named `{}`.",
                    type_symbol.name
                ))]),
            ),
        ));
    };
    let TypeDef::Struct(struct_info) = type_info.clone() else {
        let message = ErrorMessage(vec![Text(format!(
            "`{}` is not a struct, so it cannot be initialized with struct syntax.",
            type_symbol.name,
        ))]);

        return Err((
            RestoreValues(vec![]),
            EvalError::Exception(type_symbol.position.clone(), message),
        ));
    };

    let type_params: HashSet<_> = struct_info.type_params.iter().map(|p| &p.name).collect();
    let mut type_arg_bindings = FxHashMap::default();

    let mut expected_fields_by_name = FxHashMap::default();
    for field_info in &struct_info.fields {
        expected_fields_by_name.insert(&field_info.sym.name, field_info.clone());
    }

    let mut fields = vec![];

    let type_bindings = env.current_frame().type_bindings.clone();
    for (field_sym, field_expr) in field_exprs {
        let field_value = env
            .pop_value()
            .expect("Value stack should have sufficient items for the struct literal");

        let Some(field_info) = expected_fields_by_name.remove(&field_sym.name) else {
            // TODO: this would be a good candidate for additional
            // positions, in this case the definition site of the
            // struct.
            let message = ErrorMessage(vec![Text(format!(
                "`{}` does not have a field named `{}`.",
                type_symbol.name, field_sym.name
            ))]);

            return Err((
                RestoreValues(vec![]), // TODO
                EvalError::Exception(field_sym.position.clone(), message),
            ));
        };

        if type_params.contains(&field_info.hint.sym.name) {
            type_arg_bindings.insert(
                field_info.hint.sym.name.clone(),
                Type::from_value(&field_value),
            );
        }

        let expected_ty =
            Type::from_hint(&field_info.hint, &env.types, &type_bindings).unwrap_or_err_ty();
        if let Err(msg) = check_type(&field_value, &expected_ty, env) {
            return Err((
                RestoreValues(vec![]), // TODO
                EvalError::Exception(
                    field_expr.position.clone(),
                    ErrorMessage(vec![Text(format!(
                        "Incorrect type for field: {}",
                        msg.as_string()
                    ))]),
                ),
            ));
        }

        fields.push((field_sym.name.clone(), field_value));
    }

    if !expected_fields_by_name.is_empty() {
        // TODO: print the missing field names too.
        let missing: Vec<_> = expected_fields_by_name
            .into_keys()
            .map(|sn| format!("`{}`", sn.text))
            .collect();
        let message = ErrorMessage(vec![Text(format!(
            "Missing fields from `{}`: {}.",
            type_symbol.name,
            missing.join(", ")
        ))]);

        return Err((
            RestoreValues(vec![]), // TODO
            EvalError::Exception(outer_expr_pos.clone(), message),
        ));
    }

    let mut type_args = vec![];
    for type_param in &struct_info.type_params {
        let param_value = type_arg_bindings
            .get(&type_param.name)
            .cloned()
            .unwrap_or(Type::no_value());
        type_args.push(param_value);
    }

    let runtime_type = Type::UserDefined {
        kind: TypeDefKind::Struct,
        name: type_symbol.name.clone(),
        args: type_args,
    };

    if expr_value_is_used {
        env.push_value(Value::new(Value_::Struct {
            type_name: type_symbol.name,
            fields,
            runtime_type,
        }));
    }

    Ok(())
}

fn eval_match_cases(
    env: &mut Env,
    expr_value_is_used: bool,
    scrutinee_pos: &Position,
    cases: &[(Pattern, Block)],
) -> Result<(), EvalError> {
    let scrutinee_value = env
        .pop_value()
        .expect("Popped an empty value stack for match");

    let Value_::EnumVariant {
        type_name: value_type_name,
        variant_idx: value_variant_idx,
        payload: value_payload,
        ..
    } = scrutinee_value.as_ref()
    else {
        let msg = ErrorMessage(vec![Text(format!(
            "Expected an enum value, but got {}: {}",
            Type::from_value(&scrutinee_value),
            scrutinee_value.display(env)
        ))]);
        return Err(EvalError::Exception(scrutinee_pos.clone(), msg));
    };

    let Some(_type) = env.get_type_def(value_type_name) else {
        let msg = ErrorMessage(vec![Text(format!(
            "Could not find an enum type named {value_type_name}"
        ))]);
        return Err(EvalError::Exception(scrutinee_pos.clone(), msg));
    };

    for (pattern, case_expr) in cases {
        if pattern.variant_sym.name.is_underscore() {
            eval_block(env, expr_value_is_used, case_expr);
            return Ok(());
        }

        let Some(value) = get_var(&pattern.variant_sym, env) else {
            let msg = ErrorMessage(vec![
                msgtext!("Expected an enum variant named "),
                msgcode!("{}", pattern.variant_sym.name),
                msgtext!(" but nothing is defined with that name."),
            ]);
            return Err(EvalError::Exception(
                pattern.variant_sym.position.clone(),
                msg,
            ));
        };

        let (pattern_type_name, pattern_variant_idx) = match value.as_ref() {
            Value_::EnumVariant {
                type_name,
                variant_idx,
                ..
            } => (type_name, *variant_idx),
            Value_::EnumConstructor {
                type_name,
                variant_idx,
                ..
            } => (type_name, *variant_idx),
            _ => {
                // TODO: error messages should include examples of valid code.
                let msg = ErrorMessage(vec![Text(format!(
                    "Patterns must be enum variants, got `{}`",
                    value.display(env)
                ))]);
                return Err(EvalError::Exception(
                    pattern.variant_sym.position.clone(),
                    msg,
                ));
            }
        };

        if value_type_name == pattern_type_name && *value_variant_idx == pattern_variant_idx {
            let mut bindings: Vec<(Symbol, Value)> = vec![];
            match (&value_payload, &pattern.payload) {
                (Some(payload), Some(payload_dest)) => match payload_dest {
                    LetDestination::Symbol(symbol) => {
                        if !symbol.name.is_underscore() {
                            bindings.push((symbol.clone(), (**payload).clone()));
                        }
                    }
                    LetDestination::Destructure(symbols) => {
                        let items = match payload.as_ref().as_ref() {
                            Value_::Tuple { items, .. } => items,
                            _ => {
                                let msg = ErrorMessage(vec![
                                    msgtext!(
                                        "Expected a tuple of {} items, but got ",
                                        symbols.len(),
                                    ),
                                    msgcode!("{}", type_representation(payload.as_ref())),
                                    msgtext!("."),
                                ]);
                                return Err(EvalError::Exception(
                                    pattern.variant_sym.position.clone(),
                                    msg,
                                ));
                            }
                        };

                        if items.len() != symbols.len() {
                            let msg = ErrorMessage(vec![msgtext!(
                                "Expected a tuple of {} items, but got a tuple of {} items.",
                                symbols.len(),
                                items.len(),
                            )]);
                            return Err(EvalError::Exception(
                                pattern.variant_sym.position.clone(),
                                msg,
                            ));
                        }

                        for (symbol, value) in symbols.iter().zip(items) {
                            if symbol.name.is_underscore() {
                                continue;
                            }

                            bindings.push((symbol.clone(), value.clone()));
                        }
                    }
                },
                (None, None) => {}
                _ => {
                    // This variant has been redefined and previously
                    // had/didn't have a payload. Ignore it.
                    continue;
                }
            }

            let stack_frame = env.current_frame_mut();
            stack_frame.bindings_next_block = bindings;
            eval_block(env, expr_value_is_used, case_expr);
            return Ok(());
        }
    }

    let msg = ErrorMessage(vec![Text(
        "No cases in this `match` statement were reached.".to_owned(),
    )]);
    Err(EvalError::Exception(scrutinee_pos.clone(), msg))
}

/// Evaluate the toplevel expressions provided, and then stop. If we
/// had previously interrupted execution, we should still be
/// interrupted at the same place.
pub(crate) fn eval_toplevel_exprs_then_stop(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
    namespace: Rc<RefCell<NamespaceInfo>>,
) -> Result<Option<Value>, EvalError> {
    let mut exprs = vec![];
    for item in items {
        match &item {
            ToplevelItem::Expr(expr) => {
                exprs.push(expr.0.clone());
            }
            ToplevelItem::Block(block) => {
                for expr in &block.exprs {
                    exprs.push(expr.as_ref().clone());
                }
            }
            _ => {}
        }
    }

    let Some(last_expr) = exprs.last() else {
        return Ok(None);
    };

    // Switch the namespace of the toplevel stack frame to the
    // namespace given. This ensures that we can access whatever
    // toplevel items we've just loaded in that namespace.
    if env.stack.0.len() == 1 {
        let top_stack = env.current_frame_mut();
        top_stack.namespace = namespace;
    }

    let old_stop_at_expr_id = env.stop_at_expr_id;
    env.stop_at_expr_id = Some(last_expr.id);

    let eval_result = eval_exprs(&exprs, env, session);
    env.stop_at_expr_id = old_stop_at_expr_id;

    eval_result.map(Some)
}

pub(crate) fn eval_exprs(
    exprs: &[Expression],
    env: &mut Env,
    session: &Session,
) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::unit());
    }

    let mut exprs_to_eval: Vec<(ExpressionState, Rc<Expression>)> = vec![];
    for expr in exprs.iter().rev() {
        exprs_to_eval.push((ExpressionState::NotEvaluated, expr.clone().into()));
    }

    let top_stack = env
        .stack
        .0
        .last_mut()
        .expect("Stack should always be non-empty.");
    // TODO: do this setup outside of this function.
    top_stack.exprs_to_eval = exprs_to_eval;

    eval(env, session)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::ast::IdGenerator;
    use crate::ParseError;

    fn load_toplevel_items(items: &[ToplevelItem], env: &mut Env) {
        let ns = env.current_namespace();
        super::load_toplevel_items(items, env, ns);
    }

    fn parse_toplevel_items(
        src: &str,
        vfs: &mut Vfs,
        id_gen: &mut IdGenerator,
    ) -> (Vec<ToplevelItem>, Vec<ParseError>) {
        let path = Rc::new(PathBuf::from("__test.gdn"));
        let vfs_path = vfs.insert(path, src.to_owned());
        super::parse_toplevel_items(&vfs_path, src, id_gen)
    }

    fn parse_items_from_str(
        src: &str,
        vfs: &mut Vfs,
        id_gen: &mut IdGenerator,
    ) -> Vec<ToplevelItem> {
        let (items, errors) = parse_toplevel_items(src, vfs, id_gen);
        assert!(errors.is_empty());

        items
    }

    fn parse_exprs_from_str(src: &str, vfs: &mut Vfs, id_gen: &mut IdGenerator) -> Vec<Expression> {
        let (items, errors) = parse_toplevel_items(src, vfs, id_gen);
        assert!(errors.is_empty());

        let mut exprs = vec![];
        for item in items {
            match item {
                ToplevelItem::Expr(e) => exprs.push(e.0),
                ToplevelItem::Block(b) => {
                    for e in &b.exprs {
                        exprs.push(e.as_ref().clone());
                    }
                }
                _ => unreachable!(),
            }
        }

        exprs
    }

    use super::*;

    fn eval_exprs(exprs: &[Expression], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let session = Session {
            interrupted,
            stdout_mode: StdoutMode::WriteDirectly,
            start_time: Instant::now(),
            trace_exprs: false,
            pretty_print_json: true,
        };

        super::eval_exprs(exprs, env, &session)
    }

    #[test]
    fn test_eval_equality() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("\"a\" == \"b\"", &mut vfs, &mut id_gen);

        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_persist_env() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let exprs = parse_exprs_from_str("let foo = 123", &mut env.vfs, &mut env.id_gen);
        eval_exprs(&exprs, &mut env).unwrap();

        let exprs = parse_exprs_from_str("foo", &mut env.vfs, &mut env.id_gen);
        eval_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_exprs() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("True False", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_add() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("1 + 2", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(3)));
    }

    #[test]
    fn test_eval_less_than() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("1 < 2", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_less_than_or_equal() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("3 <= 2", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_list_literal() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4]", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::new(Value_::List {
                items: vec![
                    Value::new(Value_::Integer(3)),
                    Value::new(Value_::Integer(12))
                ],
                elem_type: Type::int()
            })
        );
    }

    #[test]
    fn test_eval_block() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("{ let x = 1 x + 1 }", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(2)));
    }

    #[test]
    fn test_eval_let() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("let foo = True foo", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_if() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str(
            "let foo = if True { 1 } else { 2 } foo",
            &mut vfs,
            &mut id_gen,
        );

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(1)));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("if True { let x = 1 } x", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_list_append() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("[1, 2].append(3)", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::new(Value_::List {
                items: vec![
                    Value::new(Value_::Integer(1)),
                    Value::new(Value_::Integer(2)),
                    Value::new(Value_::Integer(3))
                ],
                elem_type: Type::int()
            })
        );
    }

    #[test]
    fn test_eval_list_length() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("[0, 1].len()", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(2)));
    }

    #[test]
    fn test_eval_string_length() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("\"abc\".len()", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(3)));
    }

    #[test]
    fn test_eval_string_substring() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str("\"abcdef\".substring(1, 3)", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::String("bc".into())));
    }

    #[test]
    fn test_eval_call() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f() { True }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(x) { x }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(123)", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(123)));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(x, y) { y }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(1, 2)", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(2)));
    }

    #[test]
    fn test_eval_call_closure_immediately() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "fun f() { let x = 1 let f = fun() { x } f() }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(1)));
    }

    #[test]
    fn test_eval_call_bad_arity() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(x) { }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_return_closure_and_call() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "fun f() { let x = 1 fun() { x } }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("let y = f() y()", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(1)));
    }

    #[test]
    fn test_eval_method_call() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "method f(this: String) { True }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("\"\".f()", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_method_call_bad_arity() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "method f(this: String) { True }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("\"\".f(123)", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_while() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs =
            parse_exprs_from_str("let i = 0 while i < 5 { i = i + 1 }", &mut vfs, &mut id_gen);

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_while_block_scope_does_not_leak() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str(
            "let i = 0 while i < 5 { i = i + 1 let x = 1 }",
            &mut vfs,
            &mut id_gen,
        );

        let mut env = Env::new(id_gen, vfs);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_env_after_call() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun id(x) { x }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("let i = 0 id(i) i", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(0)));
    }

    #[test]
    fn test_eval_return() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f() { return 1 2 }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(1)));
    }

    #[test]
    fn test_eval_correct_return_type() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(): Int { 1 }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_wrong_argument_type() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(x: Int) { }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(True)", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(): String { 1 }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type_early_return() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "fun f(): String { return 1 }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_underscore_param_not_bound() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f(_) { _ }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(1)", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_not_bound() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str("fun f() { let _ = 1 xy }", &mut env.vfs, &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_repeated() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let mut env = Env::new(id_gen, vfs);

        let items = parse_items_from_str(
            "fun f() { let _ = 1 let _ = 2 }",
            &mut env.vfs,
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.vfs, &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_match() {
        let mut id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();
        let exprs = parse_exprs_from_str(
            "let x = Some(1) match x { Some(i) => i + 1 _ => {} }",
            &mut vfs,
            &mut id_gen,
        );

        let mut env = Env::new(id_gen, vfs);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::new(Value_::Integer(2)));
    }

    #[test]
    fn test_eval_empty_test() {
        let interrupted = Arc::new(AtomicBool::new(false));
        let session = Session {
            interrupted,
            stdout_mode: StdoutMode::WriteDirectly,
            start_time: Instant::now(),
            trace_exprs: false,
            pretty_print_json: true,
        };

        let id_gen = IdGenerator::default();
        let mut vfs = Vfs::default();

        let path = Rc::new(PathBuf::from("__test.gdn"));
        let src = "test f {}";
        let vfs_path = vfs.insert(path, src.to_owned());

        let mut env = Env::new(id_gen, vfs);

        let (defs, errors) = super::parse_toplevel_items(&vfs_path, src, &mut env.id_gen);
        assert!(errors.is_empty());
        let eval_result = eval_toplevel_items(&vfs_path, &defs, &mut env, &session);
        assert!(eval_result.is_ok());
    }
}
