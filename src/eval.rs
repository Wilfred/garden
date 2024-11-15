// Used in some TODO that eventually should handle Err properly.
#![allow(clippy::manual_flatten)]

use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_items, placeholder_symbol};
use ordered_float::OrderedFloat;

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy};
use crate::json_session::{print_as_json, Response, ResponseKind};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;
use crate::values::{escape_string_literal, type_representation, BuiltinFunctionKind, Value};
use garden_lang_parser::ast::{
    AssignUpdateKind, AstId, BinaryOperatorKind, Block, BuiltinMethodKind, EnumInfo, FunInfo,
    LetDestination, MethodInfo, MethodKind, ParenthesizedArguments, Pattern, SourceString,
    StructInfo, Symbol, SymbolWithHint, SyntaxId, TestInfo, ToplevelItem, TypeHint, TypeName,
    TypeSymbol,
};
use garden_lang_parser::ast::{Definition, Definition_, Expression, Expression_, SymbolName};
use garden_lang_parser::position::Position;

/// Bindings in a single block. For example, `x` is only bound inside
/// the if block here.
///
/// ```garden
/// if y { let x = 1 }
/// ```
#[derive(Debug, Clone)]
pub(crate) struct BlockBindings {
    /// Values bound in this block, such as local variables or
    /// function parameters.
    pub(crate) values: Arc<Mutex<HashMap<SymbolName, Value>>>,
}

impl Default for BlockBindings {
    fn default() -> Self {
        Self {
            values: Arc::new(Mutex::new(HashMap::new())),
        }
    }
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

    fn new_with(outer_scope: HashMap<SymbolName, Value>) -> Self {
        Self {
            block_bindings: vec![BlockBindings {
                values: Arc::new(Mutex::new(outer_scope)),
            }],
        }
    }

    fn get(&self, name: &SymbolName) -> Option<Value> {
        // TODO: this allows shadowing. Is that desirable -- does it
        // make REPL workflows less convenient when it's harder to inspect?
        //
        // (Probably not, as long as users can inspect everything.)
        for block_bindings in self.block_bindings.iter().rev() {
            if let Some(value) = block_bindings.values.lock().unwrap().get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub(crate) fn has(&self, name: &SymbolName) -> bool {
        self.get(name).is_some()
    }

    /// Remove `name` from bindings. If this variable is shadowed,
    /// remove the innermost binding.
    pub(crate) fn remove(&mut self, name: &SymbolName) {
        for block_bindings in self.block_bindings.iter_mut().rev() {
            if block_bindings.values.lock().unwrap().get(name).is_some() {
                block_bindings.values.lock().unwrap().remove(name);
            }
        }
    }

    fn add_new(&mut self, name: &SymbolName, value: Value) {
        if name.is_underscore() {
            return;
        }

        let block_bindings = self
            .block_bindings
            .last_mut()
            .expect("Vec of bindings should always be non-empty");
        block_bindings
            .values
            .lock()
            .unwrap()
            .insert(name.clone(), value);
    }

    fn set_existing(&mut self, name: &SymbolName, value: Value) {
        for block_bindings in self.block_bindings.iter_mut().rev() {
            if block_bindings.values.lock().unwrap().contains_key(name) {
                block_bindings
                    .values
                    .lock()
                    .unwrap()
                    .insert(name.clone(), value);
                return;
            }
        }
        unreachable!()
    }

    pub(crate) fn all(&self) -> Vec<(SymbolName, Value)> {
        let mut res = vec![];
        for block_bindings in self.block_bindings.iter().rev() {
            for (k, v) in block_bindings.values.lock().unwrap().iter() {
                res.push((k.clone(), v.clone()));
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
    EvaluatedSubexpressions,
}

impl ExpressionState {
    pub(crate) fn done_children(&self) -> bool {
        matches!(self, ExpressionState::EvaluatedSubexpressions)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct StackFrame {
    pub(crate) src: SourceString,
    // The name of the function, method or test that we're evaluating.
    pub(crate) enclosing_name: EnclosingSymbol,
    pub(crate) enclosing_fun: Option<FunInfo>,
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
    pub(crate) exprs_to_eval: Vec<(ExpressionState, Expression)>,
    /// The values of subexpressions that we've evaluated so far.
    pub(crate) evalled_values: Vec<Value>,
}

pub(crate) fn most_similar(available: &[&SymbolName], name: &SymbolName) -> Option<SymbolName> {
    let mut res: Vec<_> = available.iter().collect();
    res.sort_by_key(|n| OrderedFloat(strsim::sorensen_dice(&n.0, &name.0)));

    if let Some(closest) = res.last() {
        if strsim::sorensen_dice(&closest.0, &name.0) > 0.2 {
            return Some((**closest).clone());
        }
    }

    None
}

fn most_similar_var(name: &SymbolName, stack_frame: &StackFrame, env: &Env) -> Option<SymbolName> {
    let all_bindings = stack_frame.bindings.all();

    let mut names: Vec<_> = all_bindings.iter().map(|(n, _)| n).collect();
    let local_names: Vec<_> = env.file_scope.keys().collect();
    names.extend_from_slice(&local_names);

    most_similar(&names, name)
}

fn get_var(name: &SymbolName, stack_frame: &StackFrame, env: &Env) -> Option<Value> {
    if let Some(value) = stack_frame.bindings.get(name) {
        return Some(value.clone());
    }

    if let Some(value) = env.file_scope.get(name) {
        return Some(value.clone());
    }

    None
}

#[derive(Debug)]
pub(crate) struct Session {
    pub(crate) interrupted: Arc<AtomicBool>,
    pub(crate) has_attached_stdout: bool,
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
    AssertionFailed(Position),
    /// Ran out of ticks (i.e. program did not terminate in time).
    ReachedTickLimit,
    /// Tried to execute a function that isn't permitted in the
    /// sandbox.
    ForbiddenInSandbox(Position),
}

#[derive(Debug)]
pub(crate) struct ToplevelEvalSummary {
    pub(crate) values: Vec<Value>,
    pub(crate) new_syms: Vec<SymbolName>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    // TODO: Report the names of tests that passed/failed.
    pub(crate) tests_passed: usize,
    pub(crate) tests_failed: Vec<EvalError>,
}

/// Load, but do not evaluate, `items`.
///
/// Function definitions and method definitions are inserted into the
/// environment, but tests are not executed and toplevel expressions
/// are skipped.
pub(crate) fn load_toplevel_items(items: &[ToplevelItem], env: &mut Env) -> ToplevelEvalSummary {
    let mut defs = vec![];
    for item in items {
        match item {
            ToplevelItem::Def(def) => {
                defs.push(def.clone());
            }
            ToplevelItem::Expr(_) => {}
        }
    }

    eval_defs(&defs, env)
}

/// Evaluate all toplevel items: definitions, then tests, then
/// expressions.
pub(crate) fn eval_toplevel_items(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut defs = vec![];
    let mut exprs = vec![];
    for item in items {
        match item {
            ToplevelItem::Def(def) => {
                defs.push(def.clone());
            }
            ToplevelItem::Expr(expr) => {
                exprs.push(expr.0.clone());
            }
        }
    }

    let mut summary = eval_defs(&defs, env);

    summary
        .diagnostics
        .extend(check_toplevel_items_in_env(items, env));

    let test_summary = eval_tests(items, env, session)?;
    summary.tests_passed = test_summary.tests_passed;
    summary.tests_failed = test_summary.tests_failed;

    if !exprs.is_empty() {
        let value = eval_toplevel_exprs(&exprs, env, session)?;
        summary.values = vec![value];
    }

    Ok(summary)
}

/// Evaluate these tests.
pub(crate) fn eval_tests(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut tests_passed = 0;
    let mut tests_failed = vec![];

    let mut test_defs = vec![];
    for item in items {
        if let ToplevelItem::Def(Definition(_, _, Definition_::Test(test))) = item {
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
                tests_passed += 1;
            }
            Err(e) => {
                tests_failed.push(e.clone());
                if matches!(e, EvalError::Interrupted) {
                    break;
                }
            }
        }

        env.stack.pop_to_toplevel();
    }

    Ok(ToplevelEvalSummary {
        values: vec![],
        new_syms: vec![],
        diagnostics: vec![],
        tests_passed,
        tests_failed,
    })
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
        let ToplevelItem::Def(def) = item else {
            continue;
        };
        match &def.2 {
            Definition_::Fun(name_sym, fun_info) => {
                let prev_args = match env.prev_call_args.get(&name_sym.name) {
                    _ if fun_info.params.is_empty() => vec![],
                    Some(prev_args) => prev_args.clone(),
                    None => {
                        continue;
                    }
                };

                for (i, param) in fun_info.params.iter().enumerate() {
                    if param.symbol.id != id {
                        continue;
                    }

                    if let Some(value) = prev_args.get(i) {
                        return Some((value.clone(), param.symbol.position.clone()));
                    }
                }
            }
            Definition_::Method(_) => {
                // TODO
            }
            _ => {}
        }
    }

    None
}

/// Try to evaluate items up to the syntax ID specified.
///
/// Returns None if we couldn't find anything to evaluate (not an error).
pub(crate) fn eval_up_to(
    env: &mut Env,
    session: &Session,
    items: &[ToplevelItem],
    offset: usize,
) -> Option<Result<(Value, Position), EvalError>> {
    let syn_ids = find_item_at(items, offset);

    let mut expr_id: Option<SyntaxId> = None;
    let mut position = None;
    for syn_id in syn_ids.iter().rev() {
        // TODO: this is iterating items twice, which will be slower.
        if let Some(expr) = find_expr_of_id(items, syn_id.id()) {
            expr_id = Some(expr.id);
            position = Some(expr.pos.clone());
            break;
        }
    }

    let expr_id = match expr_id {
        Some(id) => id,
        None => {
            // We didn't find an expression with this ID, try to
            // evaluate a parameter with the ID.
            if let Some(AstId::Sym(syn_id)) = syn_ids.last() {
                if let Some((value, pos)) = eval_up_to_param(env, items, *syn_id) {
                    return Some(Ok((value, pos)));
                }
            }

            return None;
        }
    };

    let position = position?;

    let item = items
        .iter()
        .find(|&item| item.position().contains_offset(offset))?;

    // TODO: this evaluates to the innermost enclosing expr. For
    // destructuring tuple, we want to return the tuple element and
    // position.

    let mut res = match item {
        ToplevelItem::Def(def) => match &def.2 {
            Definition_::Fun(name_sym, fun_info) => {
                load_toplevel_items(&[item.clone()], env);
                let args = match env.prev_call_args.get(&name_sym.name) {
                    _ if fun_info.params.is_empty() => vec![],
                    Some(prev_args) => prev_args.clone(),
                    None => {
                        // We don't have any known values that we can use, give up.
                        return None;
                    }
                };

                env.stop_at_expr_id = Some(expr_id);
                let res = eval_toplevel_call(&name_sym.name, &args, env, session);
                env.stack.pop_to_toplevel();
                env.stop_at_expr_id = None;

                Some(res.map(|v| (v, position)))
            }
            Definition_::Method(method_info) => {
                load_toplevel_items(&[item.clone()], env);
                let type_name = &method_info.receiver_hint.sym.name;

                let prev_calls_for_type = env.prev_method_call_args.get(type_name)?.clone();
                let (prev_recv, prev_args) = prev_calls_for_type.get(&method_info.name_sym.name)?;

                env.stop_at_expr_id = Some(expr_id);
                let res = eval_toplevel_method_call(
                    prev_recv,
                    &method_info.name_sym.name,
                    prev_args,
                    env,
                    session,
                );
                env.stack.pop_to_toplevel();
                env.stop_at_expr_id = None;

                Some(res.map(|v| (v, position)))
            }
            Definition_::Test(test) => {
                env.stop_at_expr_id = Some(expr_id);

                push_test_stackframe(test, env);
                let res = eval(env, session);
                env.stack.pop_to_toplevel();
                env.stop_at_expr_id = None;

                Some(res.map(|v| (v, position)))
            }
            Definition_::Enum(_) | Definition_::Struct(_) => {
                // nothing to do
                None
            }
        },
        ToplevelItem::Expr(_) => {
            env.stop_at_expr_id = Some(expr_id);

            let res = eval_toplevel_items(&[item.clone()], env, session);
            env.stack.pop_to_toplevel();
            env.stop_at_expr_id = None;

            match res {
                Ok(mut eval_summary) => {
                    let value = eval_summary.values.pop()?;
                    Some(Ok((value, position)))
                }
                Err(e) => Some(Err(e)),
            }
        }
    };

    // If the user asked for a single position in a tuple
    // destructuring, handle it here.
    if let Some(Ok((value, pos))) = &mut res {
        if let Some((var_value, var_pos)) = let_var_pos(value, items, &syn_ids) {
            *value = var_value;
            *pos = var_pos;
        }
    }

    res
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

/// Helper for starting evaluation with a function call. Used when
/// running 'eval up to ID' in a function body.
pub(crate) fn eval_toplevel_call(
    name: &SymbolName,
    args: &[Value],
    env: &mut Env,
    session: &Session,
) -> Result<Value, EvalError> {
    let stack_frame = env
        .stack
        .0
        .last_mut()
        .expect("Stack should always be non-empty");

    // TODO: return an Err() rather than kludging a string and letting
    // eval_env() return a type error.
    let recv_value = env.file_scope.get(name).cloned().unwrap_or_else(|| {
        Value::String("ERROR: Tried to call a function that isn't defined".to_owned())
    });
    stack_frame.evalled_values.push(recv_value);

    for value in args.iter().rev() {
        stack_frame.evalled_values.push(value.clone());
    }

    let recv_expr = Expression {
        pos: Position::todo(),
        expr_: Expression_::Variable(Symbol::new(Position::todo(), &name.0, env.id_gen.next())),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(),
        arguments: vec![Expression::invalid(env.id_gen.next()); args.len()],
        close_paren: Position::todo(),
    };

    let call_expr = Expression {
        pos: Position::todo(),
        expr_: Expression_::Call(Box::new(recv_expr), paren_args),
        value_is_used: true,
        id: env.id_gen.next(),
    };
    stack_frame
        .exprs_to_eval
        .push((ExpressionState::EvaluatedSubexpressions, call_expr));

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
) -> Result<Value, EvalError> {
    let stack_frame = env
        .stack
        .0
        .last_mut()
        .expect("Stack should always be non-empty");

    stack_frame.evalled_values.push(recv_value.clone());
    for value in args.iter().rev() {
        stack_frame.evalled_values.push(value.clone());
    }

    // Just create a placeholder symbol for the receiver. Since we
    // don't evaluate children, it doesn't matter.
    let recv_expr = Expression {
        pos: Position::todo(),
        expr_: Expression_::Variable(placeholder_symbol(Position::todo(), &mut env.id_gen)),
        value_is_used: true,
        id: env.id_gen.next(),
    };

    let meth_sym = Symbol {
        position: Position::todo(),
        name: meth_name.clone(),
        id: env.id_gen.next(),
    };

    let paren_args = ParenthesizedArguments {
        open_paren: Position::todo(),
        arguments: vec![Expression::invalid(env.id_gen.next()); args.len()],
        close_paren: Position::todo(),
    };

    let call_expr = Expression {
        pos: Position::todo(),
        expr_: Expression_::MethodCall(Box::new(recv_expr), meth_sym, paren_args),
        value_is_used: true,
        id: env.id_gen.next(),
    };
    stack_frame
        .exprs_to_eval
        .push((ExpressionState::EvaluatedSubexpressions, call_expr));

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
        type_bindings: HashMap::new(),
        bindings_next_block: vec![],
        exprs_to_eval,
        evalled_values: vec![Value::unit()],
        caller_uses_value: true,
    };
    env.stack.0.push(stack_frame);
}

fn eval_defs(definitions: &[Definition], env: &mut Env) -> ToplevelEvalSummary {
    let mut diagnostics: Vec<Diagnostic> = vec![];
    let mut new_syms: Vec<SymbolName> = vec![];

    for definition in definitions {
        match &definition.2 {
            Definition_::Fun(name_symbol, fun_info) => {
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
            Definition_::Method(meth_info) => {
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

                new_syms.push(SymbolName(meth_info.full_name()));
            }
            Definition_::Test(test) => {
                env.tests.insert(test.name_sym.name.clone(), test.clone());
            }
            Definition_::Enum(enum_info) => {
                // Add the enum definition to the type environment.
                env.add_type(
                    enum_info.name_sym.name.clone(),
                    TypeDef::Enum(enum_info.clone()),
                );

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
                        let runtime_type = enum_value_runtime_type(
                            env,
                            &enum_info.name_sym.name,
                            variant_idx,
                            &Type::Top,
                        )
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

                let name_as_sym = SymbolName(enum_info.name_sym.name.name.clone());
                new_syms.push(name_as_sym);
            }
            Definition_::Struct(struct_info) => {
                if is_builtin_type(struct_info) {
                    update_builtin_type_info(struct_info, env, &mut diagnostics);
                } else {
                    // Add the struct definition to the type environment.
                    env.add_type(
                        struct_info.name_sym.name.clone(),
                        TypeDef::Struct(struct_info.clone()),
                    );
                }

                let name_as_sym = SymbolName(struct_info.name_sym.name.name.clone());
                new_syms.push(name_as_sym);
            }
        }
    }

    ToplevelEvalSummary {
        values: vec![],
        new_syms,
        diagnostics,
        tests_passed: 0,
        tests_failed: vec![],
    }
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

    field.sym.name.0 == "__BUILTIN_IMPLEMENTATION"
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
                Type::from_value(value, env, &env.stack.type_bindings()),
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
        Expression_::Variable(variable) => variable.name.0 == "__BUILTIN_IMPLEMENTATION",
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
    mut stack_frame: StackFrame,
    expr_to_eval: (ExpressionState, Expression),
    evalled_values: &[Value],
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    stack_frame.exprs_to_eval.push(expr_to_eval);

    env.stack.0.push(stack_frame);
}

/// Values to push back to the evalled_values stack if we encounter an
/// error, so we can resume.
#[derive(Debug, Clone)]
struct RestoreValues(Vec<Value>);

fn eval_if(
    env: &mut Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    position: &Position,
    bool_position: &Position,
    then_body: &Block,
    else_body: Option<&Block>,
) -> Result<(), (RestoreValues, EvalError)> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    if let Some(b) = condition_value.as_rust_bool() {
        if b {
            stack_frame.exprs_to_eval.push((
                ExpressionState::NotEvaluated,
                Expression {
                    pos: position.clone(),
                    expr_: Expression_::Block(then_body.clone()),
                    value_is_used: expr_value_is_used,
                    id: env.id_gen.next(),
                },
            ));
        } else {
            match else_body {
                Some(else_body) => {
                    stack_frame.exprs_to_eval.push((
                        ExpressionState::NotEvaluated,
                        Expression {
                            pos: position.clone(),
                            expr_: Expression_::Block(else_body.clone()),
                            value_is_used: expr_value_is_used,
                            id: env.id_gen.next(),
                        },
                    ));
                }
                None => {
                    if expr_value_is_used {
                        stack_frame.evalled_values.push(Value::unit());
                    }
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
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    condition_pos: &Position,
    expr: Expression,
    body: &Block,
) -> Result<(), (RestoreValues, EvalError)> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
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

    if b {
        // After the loop body, we will want to evaluate the expression again.
        stack_frame
            .exprs_to_eval
            .push((ExpressionState::NotEvaluated, expr.clone()));

        // Evaluate the body.
        stack_frame.bindings.push_block();
        eval_block(stack_frame, expr_value_is_used, &expr, body);
    } else {
        // We're done.
        stack_frame
            .exprs_to_eval
            .push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
    }

    Ok(())
}

fn eval_for_in(
    env: &mut Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    iter_symbol: &Symbol,
    iteree_pos: &Position,
    outer_expr: Expression,
    body: &Block,
) -> Result<(), (RestoreValues, EvalError)> {
    let iteree_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for `for` loop iteree");

    let iteree_idx = match stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for `for` loop index")
    {
        Value::Integer(i) => i,
        _ => unreachable!("`for` loop index should always be an integer"),
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
        // We're done with this for loop.
        if expr_value_is_used {
            stack_frame.evalled_values.push(Value::unit());
        }
        return Ok(());
    }

    // After an iteration the loop body, evaluate again. We don't
    // re-evaluate the iteree expression though.
    stack_frame
        .exprs_to_eval
        .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

    stack_frame
        .evalled_values
        .push(Value::Integer(iteree_idx + 1));
    stack_frame.evalled_values.push(iteree_value.clone());

    let mut bindings: Vec<(Symbol, Value)> = vec![];
    if !iter_symbol.name.is_underscore() {
        bindings.push((iter_symbol.clone(), items[iteree_idx as usize].clone()));
    }

    stack_frame.bindings_next_block = bindings;
    stack_frame.exprs_to_eval.push((
        ExpressionState::NotEvaluated,
        Expression {
            pos: outer_expr.pos,
            expr_: Expression_::Block(body.clone()),
            value_is_used: false,
            id: env.id_gen.next(),
        },
    ));

    Ok(())
}

fn eval_assign_update(
    env: &Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    position: &Position,
    variable: &Symbol,
    op: AssignUpdateKind,
) -> Result<(), (RestoreValues, EvalError)> {
    let var_name = &variable.name;
    if !stack_frame.bindings.has(var_name) {
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

    let expr_value = stack_frame.evalled_values.pop().expect(&format!(
        "Popped an empty value stack for `{}`",
        op.as_src()
    ));

    let mut num = match expr_value {
        Value::Integer(i) => i,
        _ => {
            return Err((
                RestoreValues(vec![expr_value.clone()]),
                EvalError::ResumableError(
                    position.clone(),
                    format_type_error(&TypeName { name: "Int".into() }, &expr_value, env),
                ),
            ));
        }
    };

    match op {
        AssignUpdateKind::Add => {
            num += 1;
        }
        AssignUpdateKind::Subtract => {
            num -= 1;
        }
    }

    stack_frame
        .bindings
        .set_existing(var_name, Value::Integer(num));

    if expr_value_is_used {
        stack_frame.evalled_values.push(Value::unit());
    }

    Ok(())
}

fn eval_assign(
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    variable: &Symbol,
) -> Result<(), (RestoreValues, EvalError)> {
    let var_name = &variable.name;
    if !stack_frame.bindings.has(var_name) {
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

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame.bindings.set_existing(var_name, expr_value);

    if expr_value_is_used {
        stack_frame.evalled_values.push(Value::unit());
    }

    Ok(())
}

/// Bind `variable` in the current local environment.
fn eval_let(
    env: &Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    destination: &LetDestination,
    position: &Position,
    hint: &Option<TypeHint>,
) -> Result<(), (RestoreValues, EvalError)> {
    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");

    if let Some(hint) = hint {
        let expected_ty = match Type::from_hint(hint, env, &stack_frame.type_bindings) {
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

    match destination {
        LetDestination::Symbol(symbol) => stack_frame.bindings.add_new(&symbol.name, expr_value),
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
                    stack_frame.bindings.add_new(&symbol.name, item.clone());
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
    //
    // It's annoying if the type checker complains here.
    if expr_value_is_used {
        stack_frame.evalled_values.push(Value::unit());
    }

    Ok(())
}

fn format_type_error<T: ToString + ?Sized>(expected: &T, value: &Value, env: &Env) -> ErrorMessage {
    let actual_ty = Type::from_value(value, env, &env.stack.type_bindings());

    let msg = if actual_ty.is_unit() {
        format!("Expected `{}`, but got `Unit`", expected.to_string(),)
    } else {
        format!(
            "Expected `{}`, but got `{}`: {}",
            expected.to_string(),
            Type::from_value(value, env, &env.stack.type_bindings()),
            value.display(env)
        )
    };

    ErrorMessage(msg)
}

fn eval_boolean_binop(
    env: &Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), (RestoreValues, EvalError)> {
    {
        let rhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for LHS of binary operator");

        let lhs_bool = match lhs_value.as_rust_bool() {
            Some(b) => b,
            None => {
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
            }
        };

        let rhs_bool = match rhs_value.as_rust_bool() {
            Some(b) => b,
            None => {
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
            }
        };

        if expr_value_is_used {
            match op {
                BinaryOperatorKind::And => {
                    stack_frame
                        .evalled_values
                        .push(Value::bool(lhs_bool && rhs_bool));
                }
                BinaryOperatorKind::Or => {
                    stack_frame
                        .evalled_values
                        .push(Value::bool(lhs_bool || rhs_bool));
                }
                _ => unreachable!(),
            }
        }
    }
    Ok(())
}

fn eval_equality_binop(
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    op: BinaryOperatorKind,
) {
    let rhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for RHS of binary operator");
    let lhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for LHS of binary operator");

    if expr_value_is_used {
        match op {
            BinaryOperatorKind::Equal => {
                stack_frame
                    .evalled_values
                    .push(Value::bool(lhs_value == rhs_value));
            }
            BinaryOperatorKind::NotEqual => {
                stack_frame
                    .evalled_values
                    .push(Value::bool(lhs_value != rhs_value));
            }
            _ => unreachable!(),
        }
    }
}

fn eval_integer_binop(
    env: &Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    position: &Position,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), (RestoreValues, EvalError)> {
    {
        let rhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = stack_frame
            .evalled_values
            .pop()
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
                        RestoreValues(vec![lhs_value, rhs_value.clone()]),
                        EvalError::ResumableError(
                            position.clone(),
                            // TODO: this looks wrong, it should be the LHS value we print.
                            ErrorMessage(format!(
                                "Tried to divide {} by zero.",
                                rhs_value.display(env)
                            )),
                        ),
                    ));
                }

                Value::Integer(lhs_num / rhs_num)
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
            stack_frame.evalled_values.push(value);
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
    let value_type = Type::from_value(value, env, &env.stack.type_bindings());

    if is_subtype(&value_type, expected) {
        Ok(())
    } else {
        Err(format_type_error(expected, value, env))
    }
}

fn eval_builtin_call(
    env: &Env,
    kind: BuiltinFunctionKind,
    receiver_value: &Value,
    receiver_pos: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    position: &Position,
    session: &Session,
) -> Result<(), (RestoreValues, EvalError)> {
    match kind {
        BuiltinFunctionKind::Error => {
            check_arity(
                &SymbolName("error".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

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
        BuiltinFunctionKind::Assert => {
            check_arity(
                &SymbolName("assert".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            match arg_values[0].as_rust_bool() {
                Some(b) => {
                    if b {
                    } else {
                        return Err((
                            RestoreValues(saved_values),
                            EvalError::AssertionFailed(position.clone()),
                        ));
                    }
                }
                None => {
                    let message = format_type_error(
                        &TypeName {
                            name: "Bool".into(),
                        },
                        &arg_values[0],
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
                &SymbolName("print".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match &arg_values[0] {
                Value::String(s) => {
                    if session.has_attached_stdout {
                        print!("{}", s);
                    } else {
                        let response = Response {
                            kind: ResponseKind::Printed { s: s.clone() },
                            position: None,
                            id: None,
                        };
                        print_as_json(&response, session.pretty_print_json);
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

            if expr_value_is_used {
                stack_frame.evalled_values.push(Value::unit());
            }
        }
        BuiltinFunctionKind::Println => {
            check_arity(
                &SymbolName("println".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            match &arg_values[0] {
                Value::String(s) => {
                    if session.has_attached_stdout {
                        println!("{}", s);
                    } else {
                        let response = Response {
                            kind: ResponseKind::Printed {
                                s: format!("{}\n", s),
                            },
                            position: None,
                            id: None,
                        };
                        print_as_json(&response, session.pretty_print_json);
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

            if expr_value_is_used {
                stack_frame.evalled_values.push(Value::unit());
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
                &SymbolName("shell".to_owned()),
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
                                stack_frame.evalled_values.push(v);
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
                &SymbolName("string_repr".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            if expr_value_is_used {
                stack_frame
                    .evalled_values
                    .push(Value::String(arg_values[0].display(env)));
            }
        }
        BuiltinFunctionKind::PathExists => {
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
                &SymbolName("path_exists".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            // TODO: define a separate path type in Garden.
            let path_s = match &arg_values[0] {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                        saved_values.push(receiver_value.clone());
                    }

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

            let path = PathBuf::from(path_s);

            if expr_value_is_used {
                stack_frame.evalled_values.push(Value::bool(path.exists()));
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
                &SymbolName("list_directory".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            // TODO: define a separate path type in Garden.
            let path_s = match &arg_values[0] {
                Value::String(s) => s,
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
            };

            let path = PathBuf::from(path_s);

            let value = match path.read_dir() {
                Ok(dir_iter) => {
                    let mut items = vec![];
                    for entry in dir_iter {
                        // TODO: don't silently discard errors.
                        if let Ok(entry) = entry {
                            items.push(Value::String(entry.path().display().to_string()));
                        }
                    }

                    Value::ok(
                        Value::List {
                            items,
                            elem_type: Type::list(Type::string()),
                        },
                        env,
                    )
                }
                Err(e) => {
                    let s = Value::String(format!("{}", e));
                    Value::err(s, env)
                }
            };

            if expr_value_is_used {
                stack_frame.evalled_values.push(value);
            }
        }
        BuiltinFunctionKind::ReadFile => {
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
                &SymbolName("read_file".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            // TODO: define a separate path type in Garden.
            let path_s = match &arg_values[0] {
                Value::String(s) => s,
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
            };

            let path = PathBuf::from(path_s);

            let v = match std::fs::read_to_string(path) {
                Ok(s) => Value::ok(Value::String(s), env),
                Err(e) => Value::err(Value::String(e.to_string()), env),
            };

            if expr_value_is_used {
                stack_frame.evalled_values.push(v);
            }
        }
        BuiltinFunctionKind::WorkingDirectory => {
            check_arity(
                &SymbolName("working_directory".to_owned()),
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            // TODO: when we have a userland result type, use that.
            let path = std::env::current_dir().unwrap_or_default();

            if expr_value_is_used {
                stack_frame
                    .evalled_values
                    .push(Value::String(path.display().to_string()));
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
                &SymbolName("write_file".to_owned()),
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
                stack_frame.evalled_values.push(v);
            }
        }
    }

    Ok(())
}

/// Evaluate a function call.
///
/// If we're calling a userland function, return the new stackframe to
/// evaluate next.
fn eval_call(
    env: &mut Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    caller_expr: &Expression,
    arg_positions: &[Position],
    arg_values: &[Value],
    receiver_value: &Value,
    session: &Session,
) -> Result<Option<StackFrame>, (RestoreValues, EvalError)> {
    match &receiver_value {
        Value::Closure(bindings, fun_info) => {
            let mut bindings = bindings.clone();

            if fun_info.params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                let message = ErrorMessage(format!(
                    "Closure expects {} argument{}, but got {}",
                    fun_info.params.len(),
                    if fun_info.params.len() == 1 { "" } else { "s" },
                    arg_values.len()
                ));
                return Err((
                    RestoreValues(saved_values),
                    EvalError::ResumableError(caller_expr.pos.clone(), message),
                ));
            }

            let mut fun_subexprs = vec![];
            for expr in fun_info.body.exprs.iter().rev() {
                fun_subexprs.push((ExpressionState::NotEvaluated, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
                let param_name = &param.symbol.name;
                if !param_name.is_underscore() {
                    fun_bindings.insert(param_name.clone(), value.clone());
                }
            }

            let mut type_bindings = TypeVarEnv::new();
            for type_param in &fun_info.type_params {
                // TODO: compute the value of these type params properly.
                type_bindings.insert(type_param.name.clone(), Some(Type::Top));
            }

            bindings.push(BlockBindings {
                values: Arc::new(Mutex::new(fun_bindings)),
            });

            return Ok(Some(StackFrame {
                caller_pos: Some(caller_expr.pos.clone()),
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
            fun_info: fi @ FunInfo { params, body, .. },
        } => {
            // Calling a user-defined function.

            check_arity(
                &name_sym.name,
                receiver_value,
                &caller_expr.pos,
                params.len(),
                arg_positions,
                arg_values,
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

            let mut type_bindings = TypeVarEnv::new();
            for param_sym in &fi.type_params {
                // TODO: calculate the value of type parameters properly.
                type_bindings.insert(param_sym.name.clone(), Some(Type::Top));
            }

            check_param_types(
                env,
                receiver_value,
                params,
                arg_positions,
                arg_values,
                &type_bindings,
            )?;

            let mut fun_subexprs = vec![];
            for expr in body.exprs.iter().rev() {
                fun_subexprs.push((ExpressionState::NotEvaluated, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                let param_name = &param.symbol.name;
                if !param_name.is_underscore() {
                    fun_bindings.insert(param_name.clone(), value.clone());
                }
            }

            return Ok(Some(StackFrame {
                enclosing_fun: Some(fi.clone()),
                src: fi.src_string.clone(),
                caller_pos: Some(caller_expr.pos.clone()),
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
            receiver_value,
            &caller_expr.pos,
            arg_positions,
            arg_values,
            stack_frame,
            expr_value_is_used,
            &caller_expr.pos,
            session,
        )?,
        Value::EnumConstructor {
            type_name,
            variant_idx,
            ..
        } => {
            check_arity(
                &SymbolName(type_name.name.clone()),
                receiver_value,
                &caller_expr.pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let runtime_type = enum_value_runtime_type(
                env,
                type_name,
                *variant_idx,
                &Type::from_value(&arg_values[0], env, &stack_frame.type_bindings),
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
                stack_frame.evalled_values.push(value);
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
                EvalError::ResumableError(caller_expr.pos.clone(), message),
            ));
        }
    }

    Ok(None)
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
        Type::from_hint(payload_hint, env, &env.stack.type_bindings()).unwrap_or_err_ty()
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

    match &variant_info.payload_hint {
        Some(hint) => {
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
        }
        None => {
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
            let param_ty = match Type::from_hint(param_hint, env, type_bindings) {
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
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    caller_expr: &Expression,
    meth_name: &Symbol,
    paren_args: &ParenthesizedArguments,
) -> Result<Option<StackFrame>, (RestoreValues, EvalError)> {
    let mut arg_values: Vec<Value> = vec![];
    let mut arg_positions: Vec<Position> = vec![];
    for arg in &paren_args.arguments {
        arg_values.push(
            stack_frame
                .evalled_values
                .pop()
                .expect("Popped an empty value for stack for method call arguments."),
        );
        arg_positions.push(arg.pos.clone());
    }
    let receiver_value = stack_frame
        .evalled_values
        .pop()
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

    let receiver_method = match env.methods.get(&receiver_type_name) {
        Some(receiver_methods) => {
            if let Some(method) = receiver_methods.get(&meth_name.name) {
                method
            } else {
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
            }
        }
        None => {
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
        }
    };

    let fun_info = match &receiver_method.kind {
        MethodKind::BuiltinMethod(kind, _) => {
            eval_builtin_method_call(
                env,
                *kind,
                &receiver_value,
                &caller_expr.pos,
                &arg_positions,
                &arg_values,
                stack_frame,
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
        &caller_expr.pos,
        fun_info.params.len(),
        &arg_positions,
        &arg_values,
    )?;

    // TODO: check for duplicate parameter names.
    // TODO: parameter names must not clash with the receiver name.
    let mut fun_bindings: HashMap<SymbolName, Value> = HashMap::new();
    for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
        let param_name = &param.symbol.name;
        fun_bindings.insert(param_name.clone(), value.clone());
    }
    fun_bindings.insert(receiver_method.receiver_sym.name.clone(), receiver_value);

    let mut type_bindings = TypeVarEnv::new();
    for type_param in &fun_info.type_params {
        // TODO: compute the value of these type params properly.
        type_bindings.insert(type_param.name.clone(), Some(Type::Top));
    }

    Ok(Some(StackFrame {
        enclosing_fun: Some(fun_info.clone()),
        enclosing_name: EnclosingSymbol::Method(receiver_type_name, meth_name.clone()),
        src: fun_info.src_string.clone(),
        caller_pos: Some(caller_expr.pos.clone()),
        caller_expr_id: Some(caller_expr.id),
        bindings: Bindings::new_with(fun_bindings),
        type_bindings,
        bindings_next_block: vec![],
        exprs_to_eval: method_subexprs,
        evalled_values: vec![Value::unit()],
        caller_uses_value: expr_value_is_used,
    }))
}

fn eval_builtin_method_call(
    env: &Env,
    kind: BuiltinMethodKind,
    receiver_value: &Value,
    receiver_pos: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
) -> Result<(), (RestoreValues, EvalError)> {
    match kind {
        BuiltinMethodKind::ListAppend => {
            check_arity(
                &SymbolName("List::append".to_owned()),
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
                        // TODO: check that the new value has the same
                        // type as the existing list items.
                        stack_frame.evalled_values.push(Value::List {
                            items: new_items,
                            elem_type: Type::from_value(
                                &arg_values[0],
                                env,
                                &stack_frame.type_bindings,
                            ),
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
                &SymbolName("List::get".to_owned()),
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
                        stack_frame.evalled_values.push(items[index].clone());
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
                &SymbolName("List::len".to_owned()),
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match &receiver_value {
                Value::List { items, .. } => {
                    if expr_value_is_used {
                        stack_frame
                            .evalled_values
                            .push(Value::Integer(items.len() as i64));
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
        BuiltinMethodKind::StringAppend => {
            check_arity(
                &SymbolName("String::append".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            let mut arg1 = match &receiver_value {
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
            let arg2 = match &arg_values[0] {
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

            arg1.push_str(arg2);

            if expr_value_is_used {
                stack_frame.evalled_values.push(Value::String(arg1));
            }
        }
        BuiltinMethodKind::StringLen => {
            check_arity(
                &SymbolName("String::len".to_owned()),
                receiver_value,
                receiver_pos,
                0,
                arg_positions,
                arg_values,
            )?;

            match &receiver_value {
                Value::String(s) => {
                    if expr_value_is_used {
                        stack_frame
                            .evalled_values
                            .push(Value::Integer(s.chars().count() as i64));
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
                &SymbolName("String::substring".to_owned()),
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
                stack_frame.evalled_values.push(Value::String(
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

/// Execute the expressions in the stack on `env`.
pub(crate) fn eval(env: &mut Env, session: &Session) -> Result<Value, EvalError> {
    while let Some(mut stack_frame) = env.stack.0.pop() {
        if let Some((mut expr_state, outer_expr)) = stack_frame.exprs_to_eval.pop() {
            env.ticks += 1;

            let expr_position = outer_expr.pos.clone();
            let expr_value_is_used =
                outer_expr.value_is_used || env.stop_at_expr_id.as_ref() == Some(&outer_expr.id);
            let expr_id = outer_expr.id;

            if session.interrupted.load(Ordering::SeqCst) {
                session.interrupted.store(false, Ordering::SeqCst);
                restore_stack_frame(env, stack_frame, (expr_state, outer_expr), &[]);
                return Err(EvalError::Interrupted);
            }

            if let Some(tick_limit) = env.tick_limit {
                if env.ticks >= tick_limit {
                    restore_stack_frame(env, stack_frame, (expr_state, outer_expr), &[]);
                    return Err(EvalError::ReachedTickLimit);
                }
            }

            if session.trace_exprs {
                println!("{:?} {:?}\n", &outer_expr.expr_, expr_state);
            }
            match &outer_expr.expr_ {
                Expression_::Match(scrutinee, cases) => {
                    if expr_state.done_children() {
                        eval_match_cases(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &scrutinee.pos,
                            cases,
                        )?;
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *scrutinee.clone()));
                    }
                }
                Expression_::If(condition, ref then_body, ref else_body) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_if(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &expr_position,
                            &condition.pos,
                            then_body,
                            else_body.as_ref(),
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *condition.clone()));
                    }
                }
                Expression_::While(condition, ref body) => {
                    match expr_state {
                        ExpressionState::NotEvaluated => {
                            // Once we've evaluated the condition, we can consider evaluating the body.
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::PartiallyEvaluated, outer_expr.clone()));
                            // Evaluate the loop condition first.
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, *condition.clone()));
                        }
                        ExpressionState::PartiallyEvaluated => {
                            // Evaluated condition, can possibly evaluate body.
                            if let Err((RestoreValues(restore_values), eval_err)) = eval_while_body(
                                env,
                                &mut stack_frame,
                                expr_value_is_used,
                                &condition.pos,
                                outer_expr.clone(),
                                body,
                            ) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (expr_state, outer_expr.clone()),
                                    &restore_values,
                                );
                                return Err(eval_err);
                            }
                        }
                        ExpressionState::EvaluatedSubexpressions => {
                            // Done condition and body, nothing left to do.
                            if expr_value_is_used {
                                stack_frame.evalled_values.push(Value::unit());
                            }
                        }
                    }
                }
                Expression_::ForIn(sym, expr, body) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_for_in(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            sym,
                            &expr.pos,
                            outer_expr.clone(),
                            body,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        // The initial value of the loop index.
                        stack_frame.evalled_values.push(Value::Integer(0));

                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *expr.clone()));
                    }
                }
                Expression_::Return(expr) => {
                    if expr_state.done_children() {
                        // No more expressions to evaluate in this function, we're returning.
                        stack_frame.exprs_to_eval.clear();
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        if let Some(expr) = expr {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, *expr.clone()));
                        } else {
                            // `return` is the same as `return Unit`.
                            stack_frame.evalled_values.push(Value::unit());
                        }
                    }
                }
                Expression_::Assign(variable, expr) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) =
                            eval_assign(&mut stack_frame, expr_value_is_used, variable)
                        {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *expr.clone()));
                    }
                }
                Expression_::AssignUpdate(variable, op, expr) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_assign_update(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &expr_position,
                            variable,
                            *op,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *expr.clone()));
                    }
                }
                Expression_::Let(destination, hint, expr) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_let(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            destination,
                            &expr_position,
                            hint,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *expr.clone()));
                    }
                }
                Expression_::IntLiteral(i) => {
                    expr_state = ExpressionState::EvaluatedSubexpressions;
                    if expr_value_is_used {
                        stack_frame.evalled_values.push(Value::Integer(*i));
                    }
                }
                Expression_::StringLiteral(s) => {
                    expr_state = ExpressionState::EvaluatedSubexpressions;
                    if expr_value_is_used {
                        stack_frame.evalled_values.push(Value::String(s.clone()));
                    }
                }
                Expression_::ListLiteral(items) => {
                    if expr_state.done_children() {
                        let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                        let mut element_type = Type::no_value();

                        for _ in 0..items.len() {
                            let element = stack_frame.evalled_values.pop().expect(
                                "Value stack should have sufficient items for the list literal",
                            );
                            // TODO: check that all elements are of a compatible type.
                            // [1, None] should be an error.
                            element_type =
                                Type::from_value(&element, env, &stack_frame.type_bindings);
                            list_values.push(element);
                        }

                        if expr_value_is_used {
                            stack_frame.evalled_values.push(Value::List {
                                items: list_values,
                                elem_type: element_type,
                            });
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        for item in items.iter() {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, item.clone()));
                        }
                    }
                }
                Expression_::TupleLiteral(items) => {
                    if expr_state.done_children() {
                        let mut items_values: Vec<Value> = Vec::with_capacity(items.len());
                        let mut item_types: Vec<Type> = Vec::with_capacity(items.len());

                        for _ in 0..items.len() {
                            let element = stack_frame.evalled_values.pop().expect(
                                "Value stack should have sufficient items for the tuple literal",
                            );

                            item_types.push(Type::from_value(
                                &element,
                                env,
                                &stack_frame.type_bindings,
                            ));
                            items_values.push(element);
                        }

                        if expr_value_is_used {
                            stack_frame.evalled_values.push(Value::Tuple {
                                items: items_values,
                                item_types,
                            });
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        for item in items.iter() {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, item.clone()));
                        }
                    }
                }
                Expression_::StructLiteral(type_sym, field_exprs) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_struct_value(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            type_sym.clone(),
                            field_exprs,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        for (_, field_expr) in field_exprs.iter() {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, field_expr.clone()));
                        }
                    }
                }
                Expression_::Variable(name_sym) => {
                    if let Some(value) = get_var(&name_sym.name, &stack_frame, env) {
                        expr_state = ExpressionState::EvaluatedSubexpressions;

                        if expr_value_is_used {
                            stack_frame.evalled_values.push(value);
                        }
                    } else {
                        let suggestion = match most_similar_var(&name_sym.name, &stack_frame, env) {
                            Some(closest_name) => {
                                format!(" Did you mean {}?", closest_name)
                            }
                            None => "".to_owned(),
                        };

                        restore_stack_frame(
                            env,
                            stack_frame,
                            (expr_state, outer_expr.clone()),
                            &[],
                        );

                        return Err(EvalError::ResumableError(
                            name_sym.position.clone(),
                            ErrorMessage(format!(
                                "Undefined variable: {}.{}",
                                name_sym.name, suggestion
                            )),
                        ));
                    }
                }
                Expression_::BinaryOperator(
                    lhs,
                    op @ (BinaryOperatorKind::Add
                    | BinaryOperatorKind::Subtract
                    | BinaryOperatorKind::Multiply
                    | BinaryOperatorKind::Divide
                    | BinaryOperatorKind::LessThan
                    | BinaryOperatorKind::LessThanOrEqual
                    | BinaryOperatorKind::GreaterThan
                    | BinaryOperatorKind::GreaterThanOrEqual),
                    rhs,
                ) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_integer_binop(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &expr_position,
                            &lhs.pos,
                            &rhs.pos,
                            *op,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *rhs.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *lhs.clone()));
                    }
                }
                Expression_::BinaryOperator(
                    lhs,
                    op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
                    rhs,
                ) => {
                    if expr_state.done_children() {
                        eval_equality_binop(&mut stack_frame, expr_value_is_used, *op)
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *rhs.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *lhs.clone()));
                    }
                }
                Expression_::BinaryOperator(
                    lhs,
                    op @ (BinaryOperatorKind::And | BinaryOperatorKind::Or),
                    rhs,
                ) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_boolean_binop(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &lhs.pos,
                            &rhs.pos,
                            *op,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        // TODO: do short-circuit evaluation of && and ||.
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *rhs.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *lhs.clone()));
                    }
                }
                Expression_::FunLiteral(fun_info) => {
                    expr_state = ExpressionState::EvaluatedSubexpressions;

                    if expr_value_is_used {
                        stack_frame.evalled_values.push(Value::Closure(
                            stack_frame.bindings.block_bindings.clone(),
                            fun_info.clone(),
                        ));
                    }
                }
                Expression_::Call(receiver, paren_args) => {
                    if expr_state.done_children() {
                        let mut arg_values = vec![];
                        let mut arg_positions = vec![];
                        for arg in &paren_args.arguments {
                            arg_values.push(
                                stack_frame
                                    .evalled_values
                                    .pop()
                                    .expect("Popped an empty value for stack for call arguments"),
                            );
                            arg_positions.push(arg.pos.clone());
                        }
                        let receiver_value = stack_frame
                            .evalled_values
                            .pop()
                            .expect("Popped an empty value stack for call receiver");

                        match eval_call(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &outer_expr,
                            &arg_positions,
                            &arg_values,
                            &receiver_value,
                            session,
                        ) {
                            Ok(Some(new_stack_frame)) => {
                                env.stack.0.push(stack_frame);
                                env.stack.0.push(new_stack_frame);
                                continue;
                            }
                            Ok(None) => {}
                            Err((RestoreValues(restore_values), eval_error)) => {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (expr_state, outer_expr.clone()),
                                    &restore_values,
                                );
                                return Err(eval_error);
                            }
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        for arg in &paren_args.arguments {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, arg.clone()));
                        }
                        // Push the receiver after arguments, so
                        // we evaluate it before arguments. This
                        // makes it easier to use :replace on bad
                        // functions.
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *receiver.clone()));
                    }
                }
                Expression_::MethodCall(receiver_expr, meth_name, paren_args) => {
                    if expr_state.done_children() {
                        match eval_method_call(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            &outer_expr,
                            meth_name,
                            paren_args,
                        ) {
                            Ok(Some(new_stack_frame)) => {
                                env.stack.0.push(stack_frame);
                                env.stack.0.push(new_stack_frame);
                                continue;
                            }
                            Ok(None) => {}
                            Err((RestoreValues(restore_values), eval_err)) => {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (expr_state, outer_expr.clone()),
                                    &restore_values,
                                );
                                return Err(eval_err);
                            }
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

                        for arg in &paren_args.arguments {
                            stack_frame
                                .exprs_to_eval
                                .push((ExpressionState::NotEvaluated, arg.clone()));
                        }
                        // Push the receiver after arguments, so
                        // we evaluate it before arguments.
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *receiver_expr.clone()));
                    }
                }
                Expression_::Block(block) => {
                    if expr_state.done_children() {
                        stack_frame.bindings.pop_block();

                        if block.exprs.is_empty() && expr_value_is_used {
                            stack_frame.evalled_values.push(Value::unit());
                        }
                    } else {
                        stack_frame.bindings.push_block();
                        eval_block(&mut stack_frame, expr_value_is_used, &outer_expr, block);
                    }
                }
                Expression_::DotAccess(recv, sym) => {
                    if expr_state.done_children() {
                        if let Err((RestoreValues(restore_values), eval_err)) = eval_dot_access(
                            env,
                            &mut stack_frame,
                            expr_value_is_used,
                            sym,
                            &recv.pos,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (expr_state, outer_expr.clone()),
                                &restore_values,
                            );
                            return Err(eval_err);
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));
                        stack_frame
                            .exprs_to_eval
                            .push((ExpressionState::NotEvaluated, *recv.clone()));
                    }
                }
                Expression_::Break => {
                    expr_state = ExpressionState::EvaluatedSubexpressions;
                    eval_break(&mut stack_frame, expr_value_is_used);
                }
                Expression_::Continue => {
                    expr_state = ExpressionState::EvaluatedSubexpressions;
                    eval_continue(&mut stack_frame);
                }
                Expression_::Invalid => {
                    restore_stack_frame(env, stack_frame, (expr_state, outer_expr.clone()), &[]);
                    return Err(EvalError::ResumableError(expr_position, ErrorMessage("Tried to evaluate a syntactically invalid expression. Check your code parses correctly.".to_owned())));
                }
            }

            // If we've just finished evaluating the expression that
            // we were requested to stop at, return that value
            // immediately.
            //
            // For nested expressions, we want to stop when the
            // evaluation is fully done. For evaluations that don't
            // recurse (e.g. variable lookup), we set done_children in
            // the match above.
            if expr_state.done_children()
                && env.stop_at_expr_id.is_some()
                && env.stop_at_expr_id.as_ref() == Some(&expr_id)
            {
                let v = match stack_frame.evalled_values.last() {
                    Some(value) => value.clone(),
                    None => {
                        // TODO: this should probably be an Err() case.
                        Value::String(
                            "__ERROR: no expressions evaluated. This is a bug.".to_owned(),
                        )
                    }
                };
                return Ok(v);
            }
        }

        if stack_frame.exprs_to_eval.is_empty() {
            // No more statements in this stack frame.
            if env.stack.0.is_empty() {
                // Don't pop the outer scope: that's for the top level environment.
                env.stack.0.push(stack_frame);
                break;
            }

            // Check that the value matches the return type.
            let return_value = stack_frame
                .evalled_values
                .pop()
                .expect("Should have a value");

            if let Some(ref fun) = stack_frame.enclosing_fun {
                if let Some(return_hint) = &fun.return_hint {
                    let err_pos = return_hint.position.clone();

                    let return_ty =
                        match Type::from_hint(return_hint, env, &stack_frame.type_bindings) {
                            Ok(ty) => ty,
                            Err(e) => {
                                return Err(EvalError::ResumableError(err_pos, ErrorMessage(e)));
                            }
                        };

                    if let Err(msg) = check_type(&return_value, &return_ty, env) {
                        stack_frame.evalled_values.push(return_value.clone());
                        env.stack.0.push(stack_frame);

                        return Err(EvalError::ResumableError(err_pos, msg));
                    }
                }
            }

            // We've just finished evaluating a call and we were
            // requested to stop at this call expression and return
            // that result.
            if stack_frame.caller_expr_id.is_some()
                && env.stop_at_expr_id == stack_frame.caller_expr_id
            {
                env.stack.pop_to_toplevel();
                return Ok(return_value);
            }

            // The final evaluation result of the function
            // call should be used in the previous stack
            // frame.
            if stack_frame.caller_uses_value {
                env.stack
                    .0
                    .last_mut()
                    .unwrap()
                    .evalled_values
                    .push(return_value);
            }
        } else {
            // Keep going on this stack frame.
            env.stack.0.push(stack_frame);
        }
    }

    Ok(env
        .stack
        .0
        .last_mut()
        .expect("toplevel stack frame should exist")
        .evalled_values
        .pop()
        .expect("Should have a value from the last expression"))
}

fn eval_block(
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    outer_expr: &Expression,
    block: &Block,
) {
    stack_frame
        .exprs_to_eval
        .push((ExpressionState::EvaluatedSubexpressions, outer_expr.clone()));

    let bindings_next_block = std::mem::take(&mut stack_frame.bindings_next_block);
    for (sym, expr) in bindings_next_block {
        stack_frame.bindings.add_new(&sym.name, expr);
    }

    for expr in block.exprs.iter().rev() {
        let mut expr = expr.clone();

        // Mark this expression as unused. This ensures we recursively
        // mark blocks as unused inside e.g. if expressions, unlike
        // the parser.
        //
        // TODO: do we still need this in the parser?
        if !expr_value_is_used {
            expr.value_is_used = false;
        }

        stack_frame
            .exprs_to_eval
            .push((ExpressionState::NotEvaluated, expr));
    }
}

fn eval_break(stack_frame: &mut StackFrame, expr_value_is_used: bool) {
    // Pop all the currently evaluating expressions until we are no
    // longer inside the innermost loop.
    while let Some((expr_state, expr)) = stack_frame.exprs_to_eval.pop() {
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
        stack_frame.evalled_values.push(Value::unit());
    }
}

fn eval_continue(stack_frame: &mut StackFrame) {
    // Pop all the currently evaluating expressions until we are back
    // at the loop.
    while let Some((expr_state, expr)) = stack_frame.exprs_to_eval.pop() {
        if matches!(
            expr.expr_,
            Expression_::While(_, _) | Expression_::ForIn(_, _, _)
        ) {
            stack_frame.exprs_to_eval.push((expr_state, expr));
            break;
        }
    }
}

fn eval_dot_access(
    env: &Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    symbol: &Symbol,
    recv_pos: &Position,
) -> Result<(), (RestoreValues, EvalError)> {
    let recv_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value when evaluating dot access");

    match recv_value {
        Value::Struct { ref fields, .. } => {
            let mut found = false;

            for (field_name, field_value) in fields {
                if *field_name == symbol.name {
                    if expr_value_is_used {
                        stack_frame.evalled_values.push(field_value.clone());
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
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    type_symbol: TypeSymbol,
    field_exprs: &[(Symbol, Expression)],
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
    let TypeDef::Struct(struct_info) = type_info else {
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
    let mut type_arg_bindings = HashMap::new();

    let mut expected_fields_by_name = HashMap::new();
    for field_info in &struct_info.fields {
        expected_fields_by_name.insert(&field_info.sym.name, field_info.clone());
    }

    let mut fields = vec![];

    for (field_sym, _) in field_exprs {
        let field_value = stack_frame
            .evalled_values
            .pop()
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
                Type::from_value(&field_value, env, &env.stack.type_bindings()),
            );
        }

        // TODO: check that all field values are of a compatible type.
        fields.push((field_sym.name.clone(), field_value));
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
        stack_frame.evalled_values.push(Value::Struct {
            type_name: type_symbol.name,
            fields,
            runtime_type,
        });
    }

    Ok(())
}

fn eval_match_cases(
    env: &mut Env,
    stack_frame: &mut StackFrame,
    expr_value_is_used: bool,
    scrutinee_pos: &Position,
    cases: &[(Pattern, Box<Expression>)],
) -> Result<(), EvalError> {
    let scrutinee_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for match");

    let Value::EnumVariant {
        type_name: value_type_name,
        variant_idx: value_variant_idx,
        payload: value_payload,
        ..
    } = scrutinee_value
    else {
        let msg = ErrorMessage(format!(
            "Expected an enum value, but got {}: {}",
            Type::from_value(&scrutinee_value, env, &stack_frame.type_bindings),
            scrutinee_value.display(env)
        ));
        return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
    };

    let _type = match env.get_type_def(&value_type_name) {
        Some(type_) => type_,
        None => {
            let msg = ErrorMessage(format!(
                "Could not find an enum type named {value_type_name}"
            ));
            return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
        }
    };

    for (pattern, case_expr) in cases {
        if pattern.symbol.name.is_underscore() {
            stack_frame
                .exprs_to_eval
                .push((ExpressionState::NotEvaluated, (**case_expr).clone()));
            return Ok(());
        }

        let Some(value) = get_var(&pattern.symbol.name, stack_frame, env) else {
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

            let case_expr_pos = &case_expr.pos;
            let case_block = Expression_::Block(Block {
                open_brace: case_expr_pos.clone(),
                exprs: vec![(**case_expr).clone()],
                close_brace: case_expr_pos.clone(),
            });

            stack_frame.bindings_next_block = bindings;

            stack_frame.exprs_to_eval.push((
                ExpressionState::NotEvaluated,
                Expression {
                    pos: case_expr_pos.clone(),
                    expr_: case_block,
                    value_is_used: expr_value_is_used,
                    id: env.id_gen.next(),
                },
            ));
            return Ok(());
        }
    }

    let msg = ErrorMessage("No cases in this `match` statement were reached.".to_owned());
    Err(EvalError::ResumableError(scrutinee_pos.clone(), msg))
}

pub(crate) fn eval_toplevel_exprs(
    exprs: &[Expression],
    env: &mut Env,
    session: &Session,
) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::unit());
    }

    let mut exprs_to_eval = vec![];
    for expr in exprs.iter().rev() {
        exprs_to_eval.push((ExpressionState::NotEvaluated, expr.clone()));
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

    use garden_lang_parser::ast::SyntaxIdGenerator;
    use garden_lang_parser::parse_toplevel_items;

    fn parse_defs_from_str(src: &str) -> Vec<Definition> {
        let mut id_gen = SyntaxIdGenerator::default();
        let (items, errors) = parse_toplevel_items(&PathBuf::from("__test.gdn"), src, &mut id_gen);
        assert!(errors.is_empty());

        let mut defs = vec![];
        for item in items {
            match item {
                ToplevelItem::Def(def) => {
                    defs.push(def.clone());
                }
                ToplevelItem::Expr(_) => unreachable!(),
            }
        }

        defs
    }

    fn parse_exprs_from_str(src: &str) -> Vec<Expression> {
        let mut id_gen = SyntaxIdGenerator::default();
        let (items, errors) = parse_toplevel_items(&PathBuf::from("__test.gdn"), src, &mut id_gen);
        assert!(errors.is_empty());

        let mut exprs = vec![];
        for item in items {
            match item {
                ToplevelItem::Def(_) => unreachable!(),
                ToplevelItem::Expr(e) => exprs.push(e.0),
            }
        }

        exprs
    }

    use super::*;

    fn eval_toplevel_exprs(exprs: &[Expression], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let session = Session {
            interrupted,
            has_attached_stdout: false,
            start_time: Instant::now(),
            trace_exprs: false,
            pretty_print_json: true,
        };

        super::eval_toplevel_exprs(exprs, env, &session)
    }

    #[test]
    fn test_eval_equality() {
        let exprs = parse_exprs_from_str("\"a\" == \"b\"");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let exprs = parse_exprs_from_str("let foo = 123");
        eval_toplevel_exprs(&exprs, &mut env).unwrap();

        let exprs = parse_exprs_from_str("foo");
        eval_toplevel_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_exprs() {
        let exprs = parse_exprs_from_str("True False");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_add() {
        let exprs = parse_exprs_from_str("1 + 2");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_less_than() {
        let exprs = parse_exprs_from_str("1 < 2");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_less_than_or_equal() {
        let exprs = parse_exprs_from_str("3 <= 2");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(false));
    }

    #[test]
    fn test_eval_list_literal() {
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4]");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
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
        let exprs = parse_exprs_from_str("{ let x = 1 x + 1 }");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_block_scope_should_not_leak() {
        let exprs = parse_exprs_from_str("{ let x = 1 } x");

        let mut env = Env::default();
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_let() {
        let exprs = parse_exprs_from_str("let foo = True foo");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_if() {
        let exprs = parse_exprs_from_str("let foo = if True { 1 } else { 2 } foo");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let exprs = parse_exprs_from_str("if True { let x = 1 } x");

        let mut env = Env::default();
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_toplevel_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_list_append() {
        let exprs = parse_exprs_from_str("[1, 2].append(3)");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
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
        let exprs = parse_exprs_from_str("[10, 11].get(1)");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(11));
    }

    #[test]
    fn test_eval_list_get_out_of_bounds() {
        let exprs = parse_exprs_from_str("[10, 11].get(2)");

        let mut env = Env::default();
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_get_empty() {
        let exprs = parse_exprs_from_str("[].get(0)");

        let mut env = Env::default();
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_length() {
        let exprs = parse_exprs_from_str("[0, 1].len()");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_string_length() {
        let exprs = parse_exprs_from_str("\"abc\".len()");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_string_substring() {
        let exprs = parse_exprs_from_str("\"abcdef\".substring(1, 3)");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::String("bc".into()));
    }

    #[test]
    fn test_eval_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { True }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x) { x }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(123)");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x, y) { y }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(1, 2)");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_call_closure_immediately() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let x = 1 let f = fun() { x } f() }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_call_bad_arity() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x) { }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_return_closure_and_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let x = 1 fun() { x } }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("let y = f() y()");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_method_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun (this: String) f() { True }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f()");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::bool(true));
    }

    #[test]
    fn test_eval_method_call_bad_airty() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun (this: String) f() { True }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f(123)");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_while() {
        let exprs = parse_exprs_from_str("let i = 0 while i < 5 { i = i + 1 }");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::unit());
    }

    #[test]
    fn test_eval_while_block_scope_does_not_leak() {
        let exprs = parse_exprs_from_str("let i = 0 while i < 5 { i = i + 1 let x = 1 }");

        let mut env = Env::default();
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_env_after_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun id(x) { x }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("let i = 0 id(i) i");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(0));
    }

    #[test]
    fn test_eval_return() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { return 1 2 }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_correct_return_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): Int { 1 }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_wrong_argument_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x: Int) { }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(True)");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): String { 1 }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type_early_return() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): String { return 1 }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_underscore_param_not_bound() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(_) { _ }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(1)");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_not_bound() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let _ = 1 _ }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_repeated() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let _ = 1 let _ = 2 }");
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f()");
        assert!(eval_toplevel_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_match() {
        let exprs = parse_exprs_from_str("let x = Some(1) match x { Some(i) => i + 1 _ => {} }");

        let mut env = Env::default();
        let value = eval_toplevel_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_empty_test() {
        let interrupted = Arc::new(AtomicBool::new(false));
        let session = Session {
            interrupted,
            has_attached_stdout: false,
            start_time: Instant::now(),
            trace_exprs: false,
            pretty_print_json: true,
        };

        let mut env = Env::default();

        let mut id_gen = SyntaxIdGenerator::default();
        let (defs, errors) = parse_toplevel_items(&PathBuf::new(), "test f {}", &mut id_gen);
        assert!(errors.is_empty());
        let eval_result = eval_toplevel_items(&defs, &mut env, &session);
        assert!(eval_result.is_ok());
    }
}
