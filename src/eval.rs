// Used in some TODO that eventually should handle Err properly.
#![allow(clippy::manual_flatten)]

use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_items, placeholder_symbol};
use ordered_float::OrderedFloat;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::{Diagnostic, Level};
use crate::env::{Env, StackFrame};
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy};
use crate::json_session::{print_as_json, Response, ResponseKind};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;
use crate::values::{escape_string_literal, type_representation, BuiltinFunctionKind, Value};
use garden_lang_parser::ast::{
    AssignUpdateKind, AstId, BinaryOperatorKind, Block, BuiltinMethodKind, EnumInfo, FunInfo,
    IdGenerator, InternedSymbolId, LetDestination, MethodInfo, MethodKind, ParenthesizedArguments,
    ParenthesizedParameters, Pattern, StructInfo, Symbol, SymbolWithHint, SyntaxId, TestInfo,
    TypeHint, TypeName, TypeSymbol,
};
use garden_lang_parser::ast::{Expression, Expression_, SymbolName, ToplevelItem, ToplevelItem_};
use garden_lang_parser::position::Position;

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

#[derive(Debug, Clone)]
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
                write!(f, "fun (this: {}) {}()", type_name.name, meth_sym.name)
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
    res.sort_by_key(|n| OrderedFloat(strsim::sorensen_dice(&n.name, &name.name)));

    if let Some(closest) = res.last() {
        if strsim::sorensen_dice(&closest.name, &name.name) > 0.3 {
            return Some((**closest).clone());
        }
    }

    None
}

fn most_similar_var(name: &SymbolName, env: &Env) -> Option<SymbolName> {
    let file_level_names: Vec<_> = env.file_scope.keys().collect();

    // TODO: suggest local variables too.
    most_similar(&file_level_names, name)
}

fn get_var(sym: &Symbol, env: &Env) -> Option<Value> {
    if let Some(value) = env.current_frame().bindings.get(sym.interned_id) {
        return Some(value.clone());
    }

    if let Some(value) = env.file_scope.get(&sym.name) {
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
    /// Normal userland error, e.g. `error()` was called.
    ResumableError(Position, ErrorMessage),
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
) -> (Vec<Diagnostic>, Vec<SymbolName>) {
    let mut paths_seen = FxHashSet::default();
    load_toplevel_items_(items, env, &mut paths_seen)
}

fn load_toplevel_items_(
    items: &[ToplevelItem],
    env: &mut Env,
    paths_seen: &mut FxHashSet<PathBuf>,
) -> (Vec<Diagnostic>, Vec<SymbolName>) {
    let mut diagnostics: Vec<Diagnostic> = vec![];
    let mut new_syms: Vec<SymbolName> = vec![];

    let mut enum_infos: Vec<&EnumInfo> = vec![];

    for item in items {
        match &item.2 {
            ToplevelItem_::Fun(name_symbol, fun_info, _) => {
                if is_builtin_stub(fun_info) {
                    update_builtin_fun_info(fun_info, env, &mut diagnostics);
                } else {
                    env.set_with_file_scope(
                        &name_symbol.name,
                        Value::Fun {
                            name_sym: name_symbol.clone(),
                            fun_info: fun_info.clone(),
                        },
                    );
                }

                new_syms.push(name_symbol.name.clone());
            }
            ToplevelItem_::Method(meth_info, _) => {
                if let MethodKind::UserDefinedMethod(fun_info) = &meth_info.kind {
                    if is_builtin_stub(fun_info) {
                        update_builtin_meth_info(meth_info, fun_info, env, &mut diagnostics);
                    } else {
                        // TODO: check that types in definitions are defined, and emit
                        // warnings otherwise.
                        //
                        // ```
                        // fun (this: NoSuchType) foo(x: NoSuchType): NoSuchType {}
                        // ```
                        env.add_method(meth_info);
                    }
                }

                new_syms.push(SymbolName {
                    name: meth_info.full_name(),
                });
            }
            ToplevelItem_::Test(test) => {
                env.tests.insert(test.name_sym.name.clone(), test.clone());
            }
            ToplevelItem_::Enum(enum_info) => {
                // Add the enum definition to the type environment.
                env.add_type(
                    enum_info.name_sym.name.clone(),
                    TypeDef::Enum(enum_info.clone()),
                );

                enum_infos.push(enum_info);

                let name_as_sym = SymbolName {
                    name: enum_info.name_sym.name.name.clone(),
                };
                new_syms.push(name_as_sym);
            }
            ToplevelItem_::Struct(struct_info) => {
                if is_builtin_type(struct_info) {
                    update_builtin_type_info(struct_info, env, &mut diagnostics);
                } else {
                    // Add the struct definition to the type environment.
                    env.add_type(
                        struct_info.name_sym.name.clone(),
                        TypeDef::Struct(struct_info.clone()),
                    );
                }

                let name_as_sym = SymbolName {
                    name: struct_info.name_sym.name.name.clone(),
                };
                new_syms.push(name_as_sym);
            }
            ToplevelItem_::Expr(_) => {}
            ToplevelItem_::Block(_) => {}
            ToplevelItem_::Import(import_info) => {
                let Some(enclosing_dir) = item.1.path.parent() else {
                    diagnostics.push(Diagnostic {
                        message: format!(
                            "Could not find parent directory of `{}`.",
                            import_info.path.display()
                        ),
                        position: item.1.clone(),
                        level: Level::Error,
                    });

                    continue;
                };

                let path = enclosing_dir.join(&import_info.path);
                let Ok(path) = path.canonicalize() else {
                    diagnostics.push(Diagnostic {
                        message: format!(
                            "Invalid path `{}`. Double-check that this file exists.",
                            import_info.path.display()
                        ),
                        position: import_info.path_pos.clone(),
                        level: Level::Error,
                    });

                    continue;
                };

                if paths_seen.contains(&path) {
                    // Already imported this file, so we have a cyclic import.
                    continue;
                }
                paths_seen.insert(path.clone());

                let Ok(src) = std::fs::read_to_string(&path) else {
                    diagnostics.push(Diagnostic {
                        message: format!("Could not read {} or not valid UTF-8.", path.display()),
                        position: import_info.path_pos.clone(),
                        level: Level::Error,
                    });

                    continue;
                };

                let (imported_items, parse_errors) =
                    parse_toplevel_items(&path, &src, &mut env.id_gen);
                if !parse_errors.is_empty() {
                    for error in parse_errors {
                        diagnostics.push(Diagnostic {
                            message: error.message().0.clone(),
                            position: error.position().clone(),
                            level: Level::Error,
                        });
                    }
                    continue;
                }

                let (import_diagnostics, imported_syms) =
                    load_toplevel_items_(&imported_items, env, paths_seen);
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
                Value::EnumConstructor {
                    type_name: enum_info.name_sym.name.clone(),
                    variant_idx,
                    runtime_type,
                }
            } else {
                let runtime_type =
                    enum_value_runtime_type(env, &enum_info.name_sym.name, variant_idx, &Type::Top)
                        .unwrap_or(Type::no_value());

                Value::EnumVariant {
                    type_name: enum_info.name_sym.name.clone(),
                    runtime_type,
                    variant_idx,
                    payload: None,
                }
            };

            // TODO: warn if we're clobbering a name from a
            // different enum (i.e. not just redefining the
            // current enum).
            env.set_with_file_scope(&variant_sym.name_sym.name, enum_value);
        }
    }

    (diagnostics, new_syms)
}

/// Evaluate all toplevel items: definitions, then tests, then
/// expressions.
pub(crate) fn eval_toplevel_items(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut defs = vec![];
    let mut exprs: Vec<Expression> = vec![];
    for item in items {
        match &item.2 {
            ToplevelItem_::Expr(toplevel_expression) => {
                exprs.push(toplevel_expression.0.clone());
            }
            ToplevelItem_::Block(block) => {
                for expr in &block.exprs {
                    exprs.push(expr.as_ref().clone());
                }
            }
            _ => {
                defs.push(item.clone());
            }
        }
    }

    let (mut diagnostics, new_syms) = load_toplevel_items(&defs, env);
    for diagnostic in diagnostics.iter() {
        if matches!(diagnostic.level, Level::Error) {
            return Err(EvalError::ResumableError(
                diagnostic.position.clone(),
                ErrorMessage(diagnostic.message.clone()),
            ));
        }
    }

    diagnostics.extend(check_toplevel_items_in_env(items, env));

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
        if let ToplevelItem(_, _, ToplevelItem_::Test(test)) = item {
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
        if let ToplevelItem(_, _, ToplevelItem_::Test(test)) = item {
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

fn call_to_main_src(cli_args: &[String]) -> String {
    let arg_literals: Vec<_> = cli_args.iter().map(|s| escape_string_literal(s)).collect();
    format!("main([{}])", arg_literals.join(", "))
}

/// Evaluate a call to the user's main() function.
pub(crate) fn eval_call_main(
    cli_args: &[String],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let call_src = call_to_main_src(cli_args);
    let (call_expr_items, parse_errors) =
        parse_toplevel_items(&PathBuf::from("__main_fun__"), &call_src, &mut env.id_gen);
    assert!(
        parse_errors.is_empty(),
        "Internally constructed main() invocation should always be valid syntax."
    );
    eval_toplevel_items(&call_expr_items, env, session)
}

pub(crate) fn eval_up_to_param(
    env: &Env,
    items: &[ToplevelItem],
    id: SyntaxId,
) -> Option<(Value, Position)> {
    for item in items {
        match &item.2 {
            ToplevelItem_::Fun(name_sym, fun_info, _) => {
                let prev_args = match env.prev_call_args.get(&name_sym.name) {
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
            ToplevelItem_::Method(meth_info, _) => {
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
    env: &mut Env,
    session: &Session,
    items: &[ToplevelItem],
    offset: usize,
) -> Result<(Value, Position), EvalUpToErr> {
    let syn_ids = find_item_at(items, offset, offset);

    let path = PathBuf::from("__eval_up_to__");

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

    let Some(item) = items.iter().find(|&item| item.1.contains_offset(offset)) else {
        return Err(EvalUpToErr::NoExpressionFound);
    };

    // TODO: this evaluates to the innermost enclosing expr. For
    // destructuring tuple, we want to return the tuple element and
    // position.

    let mut res: Result<_, EvalError> = match &item.2 {
        ToplevelItem_::Fun(name_sym, fun_info, _) => {
            load_toplevel_items(&[item.clone()], env);
            let args = match env.prev_call_args.get(&name_sym.name) {
                _ if fun_info.params.params.is_empty() => vec![],
                Some(prev_args) => prev_args.clone(),
                None => {
                    // We don't have any known values that we can use, give up.
                    return Err(EvalUpToErr::NoValueAvailable);
                }
            };

            env.stop_at_expr_id = Some(expr_id);
            let res = eval_toplevel_call(&name_sym.name, &args, env, session, &path);
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem_::Method(method_info, _) => {
            load_toplevel_items(&[item.clone()], env);
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
                &path,
            );
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem_::Test(test) => {
            env.stop_at_expr_id = Some(expr_id);

            push_test_stackframe(test, env);
            let res = eval(env, session);
            env.stop_at_expr_id = None;

            res.map(|v| (v, position))
        }
        ToplevelItem_::Enum(_) | ToplevelItem_::Struct(_) | ToplevelItem_::Import(_) => {
            // nothing to do
            return Err(EvalUpToErr::NoExpressionFound);
        }
        ToplevelItem_::Expr(_) | ToplevelItem_::Block(_) => {
            env.stop_at_expr_id = Some(expr_id);

            let res = eval_toplevel_items(&[item.clone()], env, session);
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

        let Expression_::Let(let_dest, _, _) = &expr.expr_ else {
            continue;
        };

        match let_dest {
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

                match value {
                    Value::Tuple { items, .. } => {
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

fn assign_var_pos(
    env: &Env,
    items: &[ToplevelItem],
    ast_ids: &[AstId],
) -> Option<(Value, Position)> {
    for syn_id in ast_ids.iter().rev() {
        let Some(expr) = find_expr_of_id(items, syn_id.id()) else {
            continue;
        };

        let symbol = match &expr.expr_ {
            Expression_::Assign(symbol, _) => symbol,
            Expression_::AssignUpdate(symbol, _, _) => symbol,
            Expression_::ForIn(symbol, _, _) => symbol,
            _ => {
                break;
            }
        };

        let value = get_var(symbol, env)?;
        return Some((value, symbol.position.clone()));
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
    path: &Path,
) -> Result<Value, EvalError> {
    // TODO: return an Err() rather than kludging a string and letting
    // eval_env() return a type error.
    let recv_value = env.file_scope.get(name).cloned().unwrap_or_else(|| {
        Value::String("ERROR: Tried to call a function that isn't defined".to_owned())
    });
    env.push_value(recv_value);

    for value in args.iter().rev() {
        env.push_value(value.clone());
    }

    let recv_expr = Expression {
        position: Position::todo(path.to_owned()),
        expr_: Expression_::Variable(Symbol::new(
            Position::todo(path.to_owned()),
            &name.name,
            &mut env.id_gen,
        )),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let mut arguments = vec![];
    for _ in 0..args.len() {
        let pos = Position::todo(path.to_owned());
        arguments.push(Rc::new(Expression::invalid(pos, env.id_gen.next())));
    }

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(path.to_owned()),
        arguments,
        close_paren: Position::todo(path.to_owned()),
    };

    let call_expr = Expression {
        position: Position::todo(path.to_owned()),
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
    path: &Path,
) -> Result<Value, EvalError> {
    env.push_value(recv_value.clone());
    for value in args.iter().rev() {
        env.push_value(value.clone());
    }

    // Just create a placeholder symbol for the receiver. Since we
    // don't evaluate children, it doesn't matter.
    let recv_expr = Expression {
        position: Position::todo(path.to_owned()),
        expr_: Expression_::Variable(placeholder_symbol(
            Position::todo(path.to_owned()),
            &mut env.id_gen,
        )),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let meth_sym = Symbol {
        position: Position::todo(path.to_owned()),
        name: meth_name.clone(),
        id: env.id_gen.next(),
        interned_id: env.id_gen.intern_symbol(meth_name),
    };

    let mut arguments = vec![];
    for _ in 0..args.len() {
        let pos = Position::todo(path.to_owned());
        arguments.push(Rc::new(Expression::invalid(pos, env.id_gen.next())));
    }

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(path.to_owned()),
        arguments,
        close_paren: Position::todo(path.to_owned()),
    };

    let call_expr = Expression {
        position: Position::todo(path.to_owned()),
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

    let stack_frame = StackFrame {
        src: test.src_string.clone(),
        enclosing_name: EnclosingSymbol::Test(test.name_sym.clone()),
        enclosing_fun: None,
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
) {
    let symbol = &struct_info.name_sym;

    let Some(current_def) = env.types.get(&symbol.name) else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub for a type `{}` that doesn't exist.",
                symbol.name
            ),
            position: symbol.position.clone(),
        });
        return;
    };

    let TypeDef::Builtin(kind, _) = current_def else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub but {} isn't a built-in type.",
                symbol.name,
            ),
            position: symbol.position.clone(),
        });
        return;
    };

    env.types.insert(
        symbol.name.clone(),
        TypeDef::Builtin(*kind, Some(struct_info.clone())),
    );
}

fn is_builtin_type(struct_info: &StructInfo) -> bool {
    let Some(field) = struct_info.fields.first() else {
        return false;
    };

    field.sym.name.name == "__BUILTIN_IMPLEMENTATION"
}

fn update_builtin_meth_info(
    meth_info: &MethodInfo,
    fun_info: &FunInfo,
    env: &mut Env,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let type_name = &meth_info.receiver_hint.sym.name;
    let Some(type_methods) = env.methods.get_mut(type_name) else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub for a type {} that doesn't exist.",
                type_name
            ),
            position: meth_info.receiver_hint.sym.position.clone(),
        });
        return;
    };

    let Some(curr_meth_info) = type_methods.get_mut(&meth_info.name_sym.name) else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub for a method {} that doesn't exist on {}.",
                meth_info.name_sym.name, type_name
            ),
            position: meth_info.name_sym.position.clone(),
        });
        return;
    };

    let MethodKind::BuiltinMethod(kind, _) = &curr_meth_info.kind else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                // TODO: we need a better design principle around
                // warning phrasing. It should probably always include
                // an explanation of what will happen (in this case
                // nothing).
                "{}::{} is not a built-in method.",
                type_name, meth_info.name_sym.name
            ),
            position: meth_info.name_sym.position.clone(),
        });
        return;
    };

    // Prefer hints and symbols from builtins.gdn, as they have better
    // positions and full type parameters.
    curr_meth_info.receiver_hint = meth_info.receiver_hint.clone();
    curr_meth_info.receiver_sym = meth_info.receiver_sym.clone();
    curr_meth_info.name_sym = meth_info.name_sym.clone();

    curr_meth_info.kind = MethodKind::BuiltinMethod(*kind, Some(fun_info.clone()));
}

fn update_builtin_fun_info(fun_info: &FunInfo, env: &mut Env, diagnostics: &mut Vec<Diagnostic>) {
    let Some(symbol) = &fun_info.name_sym else {
        return;
    };

    let Some(value) = env.file_scope.get(&symbol.name) else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub for a function `{}` that doesn't exist.",
                symbol.name
            ),
            position: symbol.position.clone(),
        });
        return;
    };

    let Value::BuiltinFunction(kind, _) = value else {
        diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: format!(
                "Tried to update a built-in stub but `{}` isn't a built-in function (it's a {}).",
                symbol.name,
                Type::from_value(value, &env.types, &env.stack.type_bindings()),
            ),
            position: symbol.position.clone(),
        });
        return;
    };

    env.set_with_file_scope(
        &symbol.name,
        Value::BuiltinFunction(*kind, Some(fun_info.clone())),
    );
}

fn is_builtin_stub(fun_info: &FunInfo) -> bool {
    let exprs = &fun_info.body.exprs;
    if exprs.len() != 1 {
        return false;
    }

    let expr_ = &exprs[0].expr_;
    match expr_ {
        Expression_::Variable(variable) => variable.name.name == "__BUILTIN_IMPLEMENTATION",
        _ => false,
    }
}

// If value is a list of strings, return the strings as a vec. Return
// an error otherwise.
fn as_string_list(value: &Value) -> Result<Vec<String>, Value> {
    match value {
        Value::List { items, .. } => {
            // TODO: check runtime_type instead.
            let mut res: Vec<String> = vec![];
            for item in items {
                match item {
                    Value::String(s) => {
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
            EvalError::ResumableError(
                bool_position.clone(),
                format_type_error(
                    &TypeName {
                        name: "Bool".into(),
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
            EvalError::ResumableError(
                condition_pos.clone(),
                format_type_error(
                    &TypeName {
                        name: "Bool".into(),
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
    iter_symbol: &Symbol,
    iteree_pos: &Position,
    outer_expr: Rc<Expression>,
    body: &Block,
) -> Result<(), (RestoreValues, EvalError)> {
    let iteree_value = env
        .pop_value()
        .expect("Popped an empty value stack for `for` loop iterated value");

    let iteree_idx = match env
        .pop_value()
        .expect("Popped an empty value stack for `for` loop index")
    {
        Value::Integer(i) => i,
        v => {
            unreachable!(
                "`for` loop index should always be an `Int`, got `{}`: {}",
                v.display(env),
                outer_expr.position.as_ide_string()
            )
        }
    };

    let Value::List { items, .. } = &iteree_value else {
        return Err((
            RestoreValues(vec![iteree_value.clone()]),
            EvalError::ResumableError(
                iteree_pos.clone(),
                format_type_error(
                    &TypeName {
                        name: "List".into(),
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

    env.push_value(Value::Integer(iteree_idx + 1));
    env.push_value(iteree_value.clone());

    let mut bindings: Vec<(Symbol, Value)> = vec![];
    if !iter_symbol.name.is_underscore() {
        bindings.push((iter_symbol.clone(), items[iteree_idx as usize].clone()));
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
            EvalError::ResumableError(
                variable.position.clone(),
                ErrorMessage(format!(
                    "{} is not currently bound. Try `let {} = something`.",
                    var_name, var_name
                )),
            ),
        ));
    };
    let Value::Integer(var_value_num) = var_value else {
        return Err((
            RestoreValues(vec![]),
            EvalError::ResumableError(
                position.clone(),
                format_type_error(&TypeName { name: "Int".into() }, &var_value, env),
            ),
        ));
    };

    let rhs_value = env.pop_value().expect(&format!(
        "Popped an empty value stack for `{}`",
        op.as_src()
    ));
    let Value::Integer(rhs_num) = rhs_value else {
        return Err((
            RestoreValues(vec![rhs_value.clone()]),
            EvalError::ResumableError(
                position.clone(),
                format_type_error(&TypeName { name: "Int".into() }, &rhs_value, env),
            ),
        ));
    };

    let new_value_num = match op {
        AssignUpdateKind::Add => var_value_num + rhs_num,
        AssignUpdateKind::Subtract => var_value_num - rhs_num,
    };
    env.current_frame_mut()
        .bindings
        .set_existing(variable, Value::Integer(new_value_num));

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
            EvalError::ResumableError(
                variable.position.clone(),
                ErrorMessage(format!(
                    "{} is not currently bound. Try `let {} = something`.",
                    var_name, var_name
                )),
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
    position: &Position,
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
                    EvalError::ResumableError(
                        hint.position.clone(),
                        ErrorMessage(format!("Unbound type in hint: {}", e)),
                    ),
                ));
            }
        };

        if let Err(msg) = check_type(&expr_value, &expected_ty, env) {
            return Err((
                RestoreValues(vec![expr_value]),
                EvalError::ResumableError(
                    hint.position.clone(),
                    ErrorMessage(format!("Incorrect type for variable: {}", msg.0)),
                ),
            ));
        };
    }

    let stack_frame = env.current_frame_mut();
    match destination {
        LetDestination::Symbol(symbol) => stack_frame.bindings.add_new(symbol, expr_value),
        LetDestination::Destructure(symbols) => match &expr_value {
            Value::Tuple { items, .. } => {
                if items.len() != symbols.len() {
                    return Err((
                        RestoreValues(vec![expr_value.clone()]),
                        EvalError::ResumableError(
                            position.clone(),
                            ErrorMessage(format!(
                                "Expected a tuple with {} items, got a tuple with {} items.",
                                symbols.len(),
                                items.len(),
                            )),
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
                    EvalError::ResumableError(
                        position.clone(),
                        ErrorMessage(format!("Incorrect type for variable: {}", "x")),
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
    let actual_ty = Type::from_value(value, &env.types, &env.stack.type_bindings());

    let msg = if actual_ty.is_unit() {
        format!("Expected `{}`, but got `Unit`", expected.to_string(),)
    } else {
        format!(
            "Expected `{}`, but got `{}`: {}",
            expected.to_string(),
            Type::from_value(value, &env.types, &env.stack.type_bindings()),
            value.display(env)
        )
    };

    ErrorMessage(msg)
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
                EvalError::ResumableError(
                    lhs_position.clone(),
                    format_type_error(
                        &TypeName {
                            name: "Bool".into(),
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
                EvalError::ResumableError(
                    rhs_position.clone(),
                    format_type_error(
                        &TypeName {
                            name: "Bool".into(),
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

        let lhs_num = match lhs_value {
            Value::Integer(i) => i,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value.clone(), rhs_value]),
                    EvalError::ResumableError(
                        lhs_position.clone(),
                        format_type_error(&TypeName { name: "Int".into() }, &lhs_value, env),
                    ),
                ));
            }
        };
        let rhs_num = match rhs_value {
            Value::Integer(i) => i,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value, rhs_value.clone()]),
                    EvalError::ResumableError(
                        rhs_position.clone(),
                        format_type_error(&TypeName { name: "Int".into() }, &rhs_value, env),
                    ),
                ));
            }
        };

        let value = match op {
            BinaryOperatorKind::Add => Value::Integer(lhs_num.wrapping_add(rhs_num)),
            BinaryOperatorKind::Subtract => Value::Integer(lhs_num.wrapping_sub(rhs_num)),
            BinaryOperatorKind::Multiply => Value::Integer(lhs_num.wrapping_mul(rhs_num)),
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::ResumableError(
                            position.clone(),
                            ErrorMessage(format!(
                                "Tried to divide {} by zero.",
                                lhs_value.display(env)
                            )),
                        ),
                    ));
                }

                Value::Integer(lhs_num / rhs_num)
            }
            BinaryOperatorKind::Modulo => Value::Integer(lhs_num % rhs_num),
            BinaryOperatorKind::Exponent => {
                if rhs_num < 0 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::ResumableError(
                            position.clone(),
                            ErrorMessage(format!(
                                "Cannot raise an integer to a negative power, got {}.^ {}",
                                lhs_value.display(env),
                                rhs_value.display(env),
                            )),
                        ),
                    ));
                }

                if rhs_num > u32::MAX as i64 {
                    return Err((
                        RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                        EvalError::ResumableError(
                            position.clone(),
                            ErrorMessage(format!(
                                "Exponent is too large, got {}.^ {}",
                                lhs_value.display(env),
                                rhs_value.display(env),
                            )),
                        ),
                    ));
                }

                match lhs_num.checked_pow(rhs_num as u32) {
                    Some(num) => Value::Integer(num),
                    None => {
                        return Err((
                            RestoreValues(vec![lhs_value.clone(), rhs_value.clone()]),
                            EvalError::ResumableError(
                                position.clone(),
                                ErrorMessage(format!(
                                    "Integer overflow on raising to the power, got {}.^ {}",
                                    lhs_value.display(env),
                                    rhs_value.display(env),
                                )),
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

        let lhs_str = match &lhs_value {
            Value::String(s) => s,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value.clone(), rhs_value]),
                    EvalError::ResumableError(
                        lhs_position.clone(),
                        format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            &lhs_value,
                            env,
                        ),
                    ),
                ));
            }
        };
        let rhs_str = match &rhs_value {
            Value::String(s) => s,
            _ => {
                return Err((
                    RestoreValues(vec![lhs_value, rhs_value.clone()]),
                    EvalError::ResumableError(
                        rhs_position.clone(),
                        format_type_error(
                            &TypeName {
                                name: "String".into(),
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

        let value = Value::String(out_str);

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
            EvalError::ResumableError(
                error_position,
                ErrorMessage(format!(
                    "Function {} requires {} argument{}, but got {}",
                    fun_name,
                    expected,
                    if expected == 1 { "" } else { "s" },
                    arg_values.len()
                )),
            ),
        ));
    }

    Ok(())
}

/// Check that `value` has `expected` type.
fn check_type(value: &Value, expected: &Type, env: &Env) -> Result<(), ErrorMessage> {
    let value_type = Type::from_value(value, &env.types, &env.stack.type_bindings());

    if is_subtype(&value_type, expected) {
        Ok(())
    } else {
        Err(format_type_error(expected, value, env))
    }
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
        BuiltinFunctionKind::Error => {
            check_arity(
                &SymbolName {
                    name: "error".to_owned(),
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

            match &arg_values[0] {
                Value::String(msg) => {
                    let message = ErrorMessage(msg.clone());
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(position.clone(), message),
                    ));
                }
                v => {
                    let message = format_type_error(
                        &TypeName {
                            name: "String".into(),
                        },
                        v,
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
                    ));
                }
            }
        }
        BuiltinFunctionKind::Print => {
            check_arity(
                &SymbolName {
                    name: "print".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match &arg_values[0] {
                Value::String(s) => match session.stdout_mode {
                    StdoutMode::WriteDirectly => {
                        print!("{}", s);
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
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            name: "String".into(),
                        },
                        v,
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
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
                    name: "println".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match &arg_values[0] {
                Value::String(s) => match session.stdout_mode {
                    StdoutMode::WriteDirectly => {
                        println!("{}", s);
                    }
                    StdoutMode::WriteJson => {
                        let response = Response {
                            kind: ResponseKind::Printed {
                                s: format!("{}\n", s),
                            },
                            position: None,
                            id: None,
                        };
                        print_as_json(&response, session.pretty_print_json);
                    }
                    StdoutMode::DoNotWrite => {}
                },
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            name: "String".into(),
                        },
                        v,
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
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
                    name: "shell".to_owned(),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            match &arg_values[0] {
                Value::String(s) => {
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
                                        Value::ok(Value::String(s), env)
                                    } else {
                                        Value::err(Value::String(s), env)
                                    }
                                }
                                Err(e) => {
                                    let s = Value::String(format!("{}", e));
                                    Value::err(s, env)
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
                                    name: "List".into(),
                                },
                                &v,
                                env,
                            );
                            return Err((
                                RestoreValues(saved_values),
                                EvalError::ResumableError(arg_positions[0].clone(), message),
                            ));
                        }
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error(
                        &TypeName {
                            name: "String".into(),
                        },
                        v,
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
                    ));
                }
            }
        }
        BuiltinFunctionKind::StringRepr => {
            check_arity(
                &SymbolName {
                    name: "string_repr".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            if expr_value_is_used {
                env.push_value(Value::String(arg_values[0].display(env)));
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
                    name: "list_directory".to_owned(),
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
                        EvalError::ResumableError(receiver_pos.clone(), msg),
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

                    Value::ok(
                        Value::List {
                            items,
                            elem_type: Type::list(Type::path()),
                        },
                        env,
                    )
                }
                Err(e) => {
                    let s = Value::String(format!("{e} {path_s}"));
                    Value::err(s, env)
                }
            };

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        BuiltinFunctionKind::SourceDirectory => {
            check_arity(
                &SymbolName {
                    name: "source_directory".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            let path = position.path.clone();
            let v = match std::fs::canonicalize(path) {
                Ok(abspath) => Value::some(Value::path(abspath.display().to_string()), env),
                Err(_) => Value::none(),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::WorkingDirectory => {
            check_arity(
                &SymbolName {
                    name: "working_directory".to_owned(),
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
                    name: "write_file".to_owned(),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            let content_s = match &arg_values[0] {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error("String", v, env);
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
                    ));
                }
            };

            let path_s = match &arg_values[1] {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    let message = format_type_error("String", v, env);
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
                    ));
                }
            };

            let path = PathBuf::from(path_s);

            let v = match std::fs::write(path, content_s) {
                Ok(()) => Value::ok(Value::unit(), env),
                Err(e) => Value::err(Value::String(format!("{}", e)), env),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinFunctionKind::CheckSnippet => {
            check_arity(
                &SymbolName {
                    name: "error".to_owned(),
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

            let snippet = match &arg_values[0] {
                Value::String(s) => s,
                v => {
                    let message = format_type_error(
                        &TypeName {
                            name: "String".into(),
                        },
                        v,
                        env,
                    );
                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(arg_positions[0].clone(), message),
                    ));
                }
            };

            let v = check_snippet(snippet, env);
            if expr_value_is_used {
                env.push_value(v);
            }
        }
    }

    Ok(())
}

fn check_snippet(src: &str, env: &Env) -> Value {
    let path = PathBuf::from("snippet.gdn");

    let mut id_gen = IdGenerator::default();
    let (_, errors) = parse_toplevel_items(&path, src, &mut id_gen);

    if errors.is_empty() {
        Value::ok(Value::unit(), env)
    } else {
        let error_messages = errors
            .iter()
            .map(|e| {
                let m = e.message();
                Value::String(m.0.clone())
            })
            .collect::<Vec<_>>();

        Value::err(
            Value::List {
                items: error_messages,
                elem_type: Type::string(),
            },
            env,
        )
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
        arg_positions.push(arg.position.clone());
    }
    let receiver_value = env
        .pop_value()
        .expect("Popped an empty value stack for call receiver");

    let stack_frame = env.current_frame_mut();
    let frame_type_bindings = stack_frame.type_bindings.clone();

    match &receiver_value {
        Value::Closure(bindings, fun_info) => {
            let mut bindings = bindings.clone();

            if fun_info.params.params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                let message = ErrorMessage(format!(
                    "Closure expects {} argument{}, but got {}",
                    fun_info.params.params.len(),
                    if fun_info.params.params.len() == 1 {
                        ""
                    } else {
                        "s"
                    },
                    arg_values.len()
                ));
                return Err((
                    RestoreValues(saved_values),
                    EvalError::ResumableError(caller_expr.position.clone(), message),
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

            return Ok(Some(StackFrame {
                caller_pos: Some(caller_expr.position.clone()),
                caller_expr_id: Some(caller_expr.id),
                bindings: Bindings {
                    block_bindings: bindings,
                },
                type_bindings,
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![Value::unit()],
                enclosing_fun: Some(fun_info.clone()),
                enclosing_name: EnclosingSymbol::Closure,
                src: fun_info.src_string.clone(),
                caller_uses_value: expr_value_is_used,
            }));
        }
        Value::Fun {
            name_sym,
            fun_info:
                fi @ FunInfo {
                    params: ParenthesizedParameters { params, .. },
                    body,
                    ..
                },
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

            let mut is_self_call = false;
            if let Some(enclosing_fi) = &stack_frame.enclosing_fun {
                if enclosing_fi == fi {
                    is_self_call = true;
                }
            }

            // Always update prev_call_args, unless we're in a
            // self-recursive call, as the initial arguments are
            // generally more interesting.
            if !is_self_call {
                env.prev_call_args
                    .insert(name_sym.name.clone(), arg_values.to_vec());
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

            return Ok(Some(StackFrame {
                enclosing_fun: Some(fi.clone()),
                src: fi.src_string.clone(),
                caller_pos: Some(caller_expr.position.clone()),
                caller_expr_id: Some(caller_expr.id),
                enclosing_name: EnclosingSymbol::Fun(name_sym.clone()),
                bindings: Bindings::new_with(fun_bindings),
                type_bindings,
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![Value::unit()],
                caller_uses_value: expr_value_is_used,
            }));
        }
        Value::BuiltinFunction(kind, _) => eval_builtin_call(
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
        Value::EnumConstructor {
            type_name,
            variant_idx,
            ..
        } => {
            check_arity(
                &SymbolName {
                    name: type_name.name.clone(),
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
                &Type::from_value(&arg_values[0], &env.types, &frame_type_bindings),
            )
            .unwrap_or(Type::no_value());

            let value = Value::EnumVariant {
                type_name: type_name.clone(),
                variant_idx: *variant_idx,
                // TODO: check type of arg_values[0] is compatible
                // with the declared type of the variant.
                payload: Some(Box::new(arg_values[0].clone())),
                runtime_type,
            };

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        v => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            let message = format_type_error(
                &TypeName {
                    name: "Function".into(),
                },
                v,
                env,
            );
            return Err((
                RestoreValues(saved_values),
                EvalError::ResumableError(caller_expr.position.clone(), message),
            ));
        }
    }

    Ok(None)
}

/// Evaluate a function call.
///
/// If we're calling a userland function, return the new stackframe to
/// evaluate next.
fn eval_assert(
    env: &mut Env,
    expr_value_is_used: bool,
    outer_expr: Rc<Expression>,
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
                Some((lhs_value, kind, rhs_value)) => format!(
                    "Assertion failed `{} {} {}` at {}",
                    lhs_value.display(env),
                    kind,
                    rhs_value.display(env),
                    outer_expr.position.as_ide_string()
                ),
                None => format!(
                    "Assertion failed at {}",
                    outer_expr.position.as_ide_string()
                ),
            };

            return Err((
                RestoreValues(vec![receiver_value]),
                EvalError::AssertionFailed(recv_expr.position.clone(), ErrorMessage(message)),
            ));
        }
    } else {
        let message = format_type_error(
            &TypeName {
                name: "Bool".into(),
            },
            &receiver_value,
            env,
        );
        return Err((
            RestoreValues(vec![receiver_value]),
            EvalError::ResumableError(recv_expr.position.clone(), message),
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
        name: None,
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
                        EvalError::ResumableError(
                            arg_positions[i].clone(),
                            ErrorMessage(format!("Unbound type in hint: {}", e)),
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
                    EvalError::ResumableError(
                        arg_positions[i].clone(),
                        ErrorMessage(format!("Incorrect type for argument: {}", msg.0)),
                    ),
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
    let mut arg_values: Vec<Value> = vec![];
    let mut arg_positions: Vec<Position> = vec![];
    for arg in &paren_args.arguments {
        arg_values.push(
            env.pop_value()
                .expect("Popped an empty value for stack for method call arguments."),
        );
        arg_positions.push(arg.position.clone());
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

    let Some(receiver_methods) = env.methods.get(&receiver_type_name) else {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        return Err((
            RestoreValues(saved_values),
            EvalError::ResumableError(
                meth_name.position.clone(),
                ErrorMessage(format!("No methods defined on `{}`.", receiver_type_name)),
            ),
        ));
    };

    let Some(receiver_method) = receiver_methods.get(&meth_name.name) else {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        return Err((
            RestoreValues(saved_values),
            EvalError::ResumableError(
                meth_name.position.clone(),
                ErrorMessage(format!(
                    "No method named `{}` on `{}`.",
                    meth_name.name, receiver_type_name
                )),
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
        MethodKind::UserDefinedMethod(fun_info) => fun_info.clone(),
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

    Ok(Some(StackFrame {
        enclosing_fun: Some(fun_info.clone()),
        enclosing_name: EnclosingSymbol::Method(receiver_type_name, meth_name.clone()),
        src: fun_info.src_string.clone(),
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
    let Value::Struct {
        type_name, fields, ..
    } = value
    else {
        return Err(format_type_error(
            &TypeName {
                name: "Path".into(),
            },
            value,
            env,
        ));
    };

    if type_name.name != "Path" {
        return Err(format_type_error(
            &TypeName {
                name: "Path".into(),
            },
            value,
            env,
        ));
    }

    let Some((field_name, field_value)) = fields.first() else {
        return Err(ErrorMessage(
            "Malformed `Path` struct (expected some fields).".to_owned(),
        ));
    };
    if field_name.name != "p" {
        return Err(ErrorMessage(format!(
            "Malformed `Path` struct (expected a field named `p`, got `{}`).",
            field_name,
        )));
    }

    match field_value {
        Value::String(s) => Ok(s.clone()),
        _ => Err(ErrorMessage(format!(
            "Malformed `Path` struct (expected `p` to contain a string, got `{}`).",
            field_value.display(env),
        ))),
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
                    name: "List::append".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match &receiver_value {
                Value::List { items, .. } => {
                    let mut new_items = items.clone();
                    new_items.push(arg_values[0].clone());

                    if expr_value_is_used {
                        let stack_frame = env.current_frame();
                        let elem_type = Type::from_value(
                            &arg_values[0],
                            &env.types,
                            &stack_frame.type_bindings,
                        );

                        // TODO: check that the new value has the same
                        // type as the existing list items.
                        env.push_value(Value::List {
                            items: new_items,
                            elem_type,
                        });
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            receiver_pos.clone(),
                            format_type_error(
                                &TypeName {
                                    name: "List".into(),
                                },
                                v,
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
                    name: "List::get".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match (&receiver_value, &arg_values[0]) {
                (Value::List { items, .. }, Value::Integer(i)) => {
                    let index: usize = if *i >= items.len() as i64 || *i < 0 {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        let message = ErrorMessage(if items.is_empty() {
                            format!("Tried to index into an empty list with index {}", *i)
                        } else {
                            format!(
                                "The list index must be between 0 and {} (inclusive), but got: {}",
                                items.len() - 1,
                                i
                            )
                        });

                        return Err((
                            RestoreValues(saved_values),
                            EvalError::ResumableError(arg_positions[0].clone(), message),
                        ));
                    } else {
                        *i as usize
                    };

                    if expr_value_is_used {
                        env.push_value(items[index].clone());
                    }
                }
                (v, Value::Integer(_)) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "List".into(),
                                },
                                v,
                                env,
                            ),
                        ),
                    ));
                }
                (_, v) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[1].clone(),
                            format_type_error(&TypeName { name: "Int".into() }, v, env),
                        ),
                    ));
                }
            }
        }
        BuiltinMethodKind::ListLen => {
            check_arity(
                &SymbolName {
                    name: "List::len".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match &receiver_value {
                Value::List { items, .. } => {
                    if expr_value_is_used {
                        env.push_value(Value::Integer(items.len() as i64));
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "List".into(),
                                },
                                v,
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
                    name: "Path::exists".to_owned(),
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
                        EvalError::ResumableError(receiver_pos.clone(), msg),
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
                    name: "Path::read".to_owned(),
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
                        EvalError::ResumableError(receiver_pos.clone(), msg),
                    ));
                }
            };

            let path = PathBuf::from(path_s.clone());

            let v = match std::fs::read_to_string(path) {
                Ok(s) => Value::ok(Value::String(s), env),
                Err(e) => Value::err(Value::String(format!("{e} {path_s}")), env),
            };

            if expr_value_is_used {
                env.push_value(v);
            }
        }
        BuiltinMethodKind::StringIndexOf => {
            check_arity(
                &SymbolName {
                    name: "String::index_of".to_owned(),
                },
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let receiver_s = match &receiver_value {
                Value::String(s) => s.clone(),
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "String".into(),
                                },
                                v,
                                env,
                            ),
                        ),
                    ));
                }
            };
            let arg_s = match &arg_values[0] {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "String".into(),
                                },
                                v,
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
                        value = Value::some(Value::Integer(i as i64), env);
                        break;
                    }
                }
            }

            if expr_value_is_used {
                env.push_value(value);
            }
        }
        BuiltinMethodKind::StringLen => {
            check_arity(
                &SymbolName {
                    name: "String::len".to_owned(),
                },
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match &receiver_value {
                Value::String(s) => {
                    if expr_value_is_used {
                        env.push_value(Value::Integer(s.chars().count() as i64));
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "String".into(),
                                },
                                v,
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
                    name: "String::substring".to_owned(),
                },
                receiver_value,
                receiver_pos,
                2,
                arg_positions,
                arg_values,
            )?;

            let s_arg = match &receiver_value {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[0].clone(),
                            format_type_error(
                                &TypeName {
                                    name: "String".into(),
                                },
                                v,
                                env,
                            ),
                        ),
                    ));
                }
            };
            let from_arg = match &arg_values[0] {
                Value::Integer(i) => i,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[1].clone(),
                            format_type_error(&TypeName { name: "Int".into() }, v, env),
                        ),
                    ));
                }
            };
            let to_arg = match &arg_values[1] {
                Value::Integer(i) => i,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err((
                        RestoreValues(saved_values),
                        EvalError::ResumableError(
                            arg_positions[2].clone(),
                            format_type_error(&TypeName { name: "Int".into() }, v, env),
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
                    EvalError::ResumableError(
                        arg_positions[1].clone(),
                        ErrorMessage(format!("The first argument to String::substring must be greater than 0, but got: {}", from_arg)),
                    ),
                ));
            }

            if from_arg > to_arg {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err((
                    RestoreValues(saved_values),
                    EvalError::ResumableError(
                        arg_positions[1].clone(),
                        ErrorMessage(format!("The first argument to String::substring cannot be greater than the second, but got: {} and {}", from_arg, to_arg)),
                    ),
                ));
            }

            if expr_value_is_used {
                env.push_value(Value::String(
                    s_arg
                        .chars()
                        .skip(*from_arg as usize)
                        .take((to_arg - from_arg) as usize)
                        .collect(),
                ));
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
                    env.push_value(Value::Integer(0));

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
                eval_let(env, expr_value_is_used, destination, &expr_position, hint)?;
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
                env.push_value(Value::Integer(*i));
            }
        }
        Expression_::StringLiteral(s) => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            if expr_value_is_used {
                env.push_value(Value::String(s.clone()));
            }
        }
        Expression_::ListLiteral(items) => {
            if expr_state.done_children() {
                let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                let mut element_type = Type::no_value();

                let type_bindings = env.current_frame().type_bindings.clone();
                for _ in 0..items.len() {
                    let element = env
                        .pop_value()
                        .expect("Value stack should have sufficient items for the list literal");
                    // TODO: check that all elements are of a compatible type.
                    // [1, None] should be an error.
                    element_type = Type::from_value(&element, &env.types, &type_bindings);
                    list_values.push(element);
                }

                if expr_value_is_used {
                    env.push_value(Value::List {
                        items: list_values,
                        elem_type: element_type,
                    });
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
        Expression_::TupleLiteral(items) => {
            if expr_state.done_children() {
                let mut items_values: Vec<Value> = Vec::with_capacity(items.len());
                let mut item_types: Vec<Type> = Vec::with_capacity(items.len());

                let type_bindings = env.current_frame().type_bindings.clone();
                for _ in 0..items.len() {
                    let element = env
                        .pop_value()
                        .expect("Value stack should have sufficient items for the tuple literal");

                    item_types.push(Type::from_value(&element, &env.types, &type_bindings));
                    items_values.push(element);
                }

                if expr_value_is_used {
                    env.push_value(Value::Tuple {
                        items: items_values,
                        item_types,
                    });
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
                let suggestion = match most_similar_var(&name_sym.name, env) {
                    Some(closest_name) => {
                        format!(" Did you mean {}?", closest_name)
                    }
                    None => "".to_owned(),
                };

                return Err((
                    RestoreValues(vec![]),
                    EvalError::ResumableError(
                        name_sym.position.clone(),
                        ErrorMessage(format!(
                            "Undefined variable: {}.{}",
                            name_sym.name, suggestion
                        )),
                    ),
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

                env.push_value(Value::Closure(bindings, fun_info.clone()));
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
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, arg.clone());
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
                    env.push_expr_to_eval(ExpressionState::NotEvaluated, arg.clone());
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
        Expression_::Break => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            eval_break(env, expr_value_is_used);
        }
        Expression_::Continue => {
            *expr_state = ExpressionState::EvaluatedAllSubexpressions;
            eval_continue(env);
        }
        Expression_::Invalid => {
            return Err((RestoreValues(vec![]), (EvalError::ResumableError(expr_position, ErrorMessage("Tried to evaluate a syntactically invalid expression. Check your code parses correctly.".to_owned())))));
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
                    eval_assert(env, expr_value_is_used, outer_expr.clone(), expr)?;
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

            // Print the whole call stack every 1000 ticks if the
            // environment variable GDN_PROFILE is set, to enable
            // basic profiling.
            if env.ticks % 1000 == 0 && std::env::var("GDN_PROFILE").is_ok() {
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
                        Value::String(
                            "__ERROR: no expressions evaluated. This is a bug.".to_owned(),
                        )
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
            if let Some(ref fun) = env.current_frame().enclosing_fun {
                if let Some(return_hint) = &fun.return_hint {
                    let err_pos = return_hint.position.clone();

                    let return_ty = match Type::from_hint(return_hint, &env.types, &type_bindings) {
                        Ok(ty) => ty,
                        Err(e) => {
                            return Err(EvalError::ResumableError(err_pos, ErrorMessage(e)));
                        }
                    };

                    if let Err(msg) = check_type(&return_value, &return_ty, env) {
                        env.push_value(return_value.clone());
                        return Err(EvalError::ResumableError(err_pos, msg));
                    }
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

fn eval_dot_access(
    env: &mut Env,
    expr_value_is_used: bool,
    symbol: &Symbol,
    recv_pos: &Position,
) -> Result<(), (RestoreValues, EvalError)> {
    let recv_value = env
        .pop_value()
        .expect("Popped an empty value when evaluating dot access");

    match recv_value {
        Value::Struct { ref fields, .. } => {
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
                    EvalError::ResumableError(
                        symbol.position.clone(),
                        ErrorMessage(format!("This struct has no field named `{}`.", symbol.name)),
                    ),
                ));
            }
        }
        _ => {
            return Err((
                RestoreValues(vec![recv_value.clone()]),
                EvalError::ResumableError(
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
            EvalError::ResumableError(
                type_symbol.position.clone(),
                ErrorMessage(format!("No type exists named `{}`.", type_symbol.name)),
            ),
        ));
    };
    let TypeDef::Struct(struct_info) = type_info.clone() else {
        let message = ErrorMessage(format!(
            "`{}` is not a struct, so it cannot be initialized with struct syntax.",
            type_symbol.name,
        ));

        return Err((
            RestoreValues(vec![]),
            EvalError::ResumableError(type_symbol.position.clone(), message),
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
            let message = ErrorMessage(format!(
                "`{}` does not have a field named `{}`.",
                type_symbol.name, field_sym.name
            ));

            return Err((
                RestoreValues(vec![]), // TODO
                EvalError::ResumableError(field_sym.position.clone(), message),
            ));
        };

        if type_params.contains(&field_info.hint.sym.name) {
            type_arg_bindings.insert(
                field_info.hint.sym.name.clone(),
                Type::from_value(&field_value, &env.types, &type_bindings),
            );
        }

        let expected_ty =
            Type::from_hint(&field_info.hint, &env.types, &type_bindings).unwrap_or_err_ty();
        if let Err(msg) = check_type(&field_value, &expected_ty, env) {
            return Err((
                RestoreValues(vec![]), // TODO
                EvalError::ResumableError(
                    field_expr.position.clone(),
                    ErrorMessage(format!("Incorrect type for field: {}", msg.0)),
                ),
            ));
        }

        fields.push((field_sym.name.clone(), field_value));
    }

    if !expected_fields_by_name.is_empty() {
        // TODO: print the missing field names too.
        let missing: Vec<_> = expected_fields_by_name
            .into_keys()
            .map(|sn| format!("`{}`", sn.name))
            .collect();
        let message = ErrorMessage(format!(
            "Missing fields from `{}`: {}.",
            type_symbol.name,
            missing.join(", ")
        ));

        return Err((
            RestoreValues(vec![]), // TODO
            EvalError::ResumableError(outer_expr_pos.clone(), message),
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
        env.push_value(Value::Struct {
            type_name: type_symbol.name,
            fields,
            runtime_type,
        });
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

    let Value::EnumVariant {
        type_name: value_type_name,
        variant_idx: value_variant_idx,
        payload: value_payload,
        ..
    } = scrutinee_value
    else {
        let stack_frame = env.current_frame();
        let msg = ErrorMessage(format!(
            "Expected an enum value, but got {}: {}",
            Type::from_value(&scrutinee_value, &env.types, &stack_frame.type_bindings),
            scrutinee_value.display(env)
        ));
        return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
    };

    let Some(_type) = env.get_type_def(&value_type_name) else {
        let msg = ErrorMessage(format!(
            "Could not find an enum type named {value_type_name}"
        ));
        return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
    };

    for (pattern, case_expr) in cases {
        if pattern.symbol.name.is_underscore() {
            eval_block(env, expr_value_is_used, case_expr);
            return Ok(());
        }

        let Some(value) = get_var(&pattern.symbol, env) else {
            let msg = ErrorMessage(format!(
                "No such value defined for pattern `{}`",
                pattern.symbol.name
            ));
            return Err(EvalError::ResumableError(
                pattern.symbol.position.clone(),
                msg,
            ));
        };

        let (pattern_type_name, pattern_variant_idx) = match value {
            Value::EnumVariant {
                type_name,
                variant_idx,
                ..
            } => (type_name, variant_idx),
            Value::EnumConstructor {
                type_name,
                variant_idx,
                ..
            } => (type_name, variant_idx),
            _ => {
                // TODO: error messages should include examples of valid code.
                let msg = ErrorMessage(format!(
                    "Patterns must be enum variants, got `{}`",
                    value.display(env)
                ));
                return Err(EvalError::ResumableError(
                    pattern.symbol.position.clone(),
                    msg,
                ));
            }
        };

        if value_type_name == pattern_type_name && value_variant_idx == pattern_variant_idx {
            let mut bindings: Vec<(Symbol, Value)> = vec![];
            match (&value_payload, &pattern.argument) {
                (Some(payload), Some(payload_sym)) => {
                    if !payload_sym.name.is_underscore() {
                        bindings.push((payload_sym.clone(), (**payload).clone()));
                    }
                }
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

    let msg = ErrorMessage("No cases in this `match` statement were reached.".to_owned());
    Err(EvalError::ResumableError(scrutinee_pos.clone(), msg))
}

/// Evaluate the toplevel expressions provided, and then stop. If we
/// had previously interrupted execution, we should still be
/// interrupted at the same place.
pub(crate) fn eval_toplevel_exprs_then_stop(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<Option<Value>, EvalError> {
    let mut exprs = vec![];
    for item in items {
        match &item.2 {
            ToplevelItem_::Expr(expr) => {
                exprs.push(expr.0.clone());
            }
            ToplevelItem_::Block(block) => {
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

    use garden_lang_parser::ast::IdGenerator;
    use garden_lang_parser::parse_toplevel_items;

    fn parse_items_from_str(src: &str, id_gen: &mut IdGenerator) -> Vec<ToplevelItem> {
        let (items, errors) = parse_toplevel_items(&PathBuf::from("__test.gdn"), src, id_gen);
        assert!(errors.is_empty());

        items
    }

    fn parse_exprs_from_str(src: &str, id_gen: &mut IdGenerator) -> Vec<Expression> {
        let (items, errors) = parse_toplevel_items(&PathBuf::from("__test.gdn"), src, id_gen);
        assert!(errors.is_empty());

        let mut exprs = vec![];
        for item in items {
            match item {
                ToplevelItem(_, _, ToplevelItem_::Expr(e)) => exprs.push(e.0),
                ToplevelItem(_, _, ToplevelItem_::Block(b)) => {
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
        let exprs = parse_exprs_from_str("\"a\" == \"b\"", &mut id_gen);

        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_persist_env() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let exprs = parse_exprs_from_str("let foo = 123", &mut env.id_gen);
        eval_exprs(&exprs, &mut env).unwrap();

        let exprs = parse_exprs_from_str("foo", &mut env.id_gen);
        eval_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_exprs() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("True False", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_add() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("1 + 2", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_less_than() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("1 < 2", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_less_than_or_equal() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("3 <= 2", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_list_literal() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4]", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List {
                items: vec![Value::Integer(3), Value::Integer(12)],
                elem_type: Type::int()
            }
        );
    }

    #[test]
    fn test_eval_block() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("{ let x = 1 x + 1 }", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_let() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("let foo = True foo", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_if() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("let foo = if True { 1 } else { 2 } foo", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("if True { let x = 1 } x", &mut id_gen);

        let mut env = Env::new(id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_list_append() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[1, 2].append(3)", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List {
                items: vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)],
                elem_type: Type::int()
            }
        );
    }

    #[test]
    fn test_eval_list_get() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[10, 11].get(1)", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(11));
    }

    #[test]
    fn test_eval_list_get_out_of_bounds() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[10, 11].get(2)", &mut id_gen);

        let mut env = Env::new(id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_get_empty() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[].get(0)", &mut id_gen);

        let mut env = Env::new(id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_length() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("[0, 1].len()", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_string_length() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("\"abc\".len()", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_string_substring() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("\"abcdef\".substring(1, 3)", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::String("bc".into()));
    }

    #[test]
    fn test_eval_call() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f() { True }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(x) { x }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(123)", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(x, y) { y }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(1, 2)", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_call_closure_immediately() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str(
            "fun f() { let x = 1 let f = fun() { x } f() }",
            &mut env.id_gen,
        );
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_call_bad_arity() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(x) { }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_return_closure_and_call() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f() { let x = 1 fun() { x } }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("let y = f() y()", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_method_call() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun (this: String) f() { True }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("\"\".f()", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_method_call_bad_airty() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun (this: String) f() { True }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("\"\".f(123)", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_while() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str("let i = 0 while i < 5 { i = i + 1 }", &mut id_gen);

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_while_block_scope_does_not_leak() {
        let mut id_gen = IdGenerator::default();
        let exprs =
            parse_exprs_from_str("let i = 0 while i < 5 { i = i + 1 let x = 1 }", &mut id_gen);

        let mut env = Env::new(id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_env_after_call() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun id(x) { x }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("let i = 0 id(i) i", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(0));
    }

    #[test]
    fn test_eval_return() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f() { return 1 2 }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_correct_return_type() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(): Int { 1 }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_wrong_argument_type() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(x: Int) { }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(True)", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(): String { 1 }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type_early_return() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(): String { return 1 }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_underscore_param_not_bound() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f(_) { _ }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f(1)", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_not_bound() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f() { let _ = 1 xy }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_repeated() {
        let id_gen = IdGenerator::default();
        let mut env = Env::new(id_gen);

        let items = parse_items_from_str("fun f() { let _ = 1 let _ = 2 }", &mut env.id_gen);
        load_toplevel_items(&items, &mut env);

        let exprs = parse_exprs_from_str("f()", &mut env.id_gen);
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_match() {
        let mut id_gen = IdGenerator::default();
        let exprs = parse_exprs_from_str(
            "let x = Some(1) match x { Some(i) => i + 1 _ => {} }",
            &mut id_gen,
        );

        let mut env = Env::new(id_gen);
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
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
        let mut env = Env::new(id_gen);

        let (defs, errors) = parse_toplevel_items(&PathBuf::new(), "test f {}", &mut env.id_gen);
        assert!(errors.is_empty());
        let eval_result = eval_toplevel_items(&defs, &mut env, &session);
        assert!(eval_result.is_ok());
    }
}
