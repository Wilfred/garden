// Used in some TODO that eventually should handle Err properly.
#![allow(clippy::manual_flatten)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Write;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use garden_lang_parser::diagnostics::ErrorMessage;
use ordered_float::OrderedFloat;
use strsim::normalized_levenshtein;

use crate::checks::{check_free_variables, check_types_exist};
use crate::diagnostics::Warning;
use crate::env::Env;
use crate::json_session::{Response, ResponseKind};
use crate::types::Type;
use crate::values::{
    bool_value, result_err_value, result_ok_value, runtime_type, type_representation, unit_value,
    BuiltinFunctionKind, RuntimeType, Value,
};
use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, BuiltinMethodKind, FunInfo, MethodInfo, MethodKind, Pattern,
    Position, SourceString, Symbol, SymbolWithType, TestInfo, ToplevelItem, TypeName, TypeSymbol,
};
use garden_lang_parser::ast::{Definition, Definition_, Expression, Expression_, SymbolName};

// TODO: Is it correct to define equality here? Closures should only
// have reference equality probably.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BlockBindings(pub(crate) Rc<RefCell<HashMap<SymbolName, Value>>>);

impl Default for BlockBindings {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(HashMap::new())))
    }
}

#[derive(Debug)]
pub(crate) struct Bindings(pub(crate) Vec<BlockBindings>);

impl Bindings {
    fn new_with(outer_scope: HashMap<SymbolName, Value>) -> Self {
        Self(vec![BlockBindings(Rc::new(RefCell::new(outer_scope)))])
    }

    fn get(&self, name: &SymbolName) -> Option<Value> {
        // TODO: this allows shadowing. Is that desirable -- does it
        // make REPL workflows less convenient when it's harder to inspect?
        //
        // (Probably not, as long as users can inspect everything.)
        for block_bindings in self.0.iter().rev() {
            if let Some(value) = block_bindings.0.borrow().get(name) {
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
        for block_bindings in self.0.iter_mut().rev() {
            if block_bindings.0.borrow().get(name).is_some() {
                block_bindings.0.borrow_mut().remove(name);
            }
        }
    }

    fn add_new(&mut self, name: &SymbolName, value: Value) {
        // TODO: Handle underscore checks here rather than at all the call sites.
        let block_bindings = self
            .0
            .last_mut()
            .expect("Vec of bindings should always be non-empty");
        block_bindings.0.borrow_mut().insert(name.clone(), value);
    }

    fn set_existing(&mut self, name: &SymbolName, value: Value) {
        for block_bindings in self.0.iter_mut().rev() {
            if block_bindings.0.borrow().contains_key(name) {
                block_bindings.0.borrow_mut().insert(name.clone(), value);
                return;
            }
        }
        unreachable!()
    }

    pub(crate) fn all(&self) -> Vec<(SymbolName, Value)> {
        let mut res = vec![];
        for block_bindings in self.0.iter().rev() {
            for (k, v) in block_bindings.0.borrow().iter() {
                res.push((k.clone(), v.clone()));
            }
        }

        res
    }
}

impl Default for Bindings {
    fn default() -> Self {
        Self(vec![BlockBindings::default()])
    }
}

#[derive(Debug)]
pub(crate) struct StackFrame {
    pub(crate) src: SourceString,
    // The name of the function, method or test that we're evaluating.
    pub(crate) enclosing_name: SymbolName,
    pub(crate) enclosing_fun: Option<FunInfo>,
    /// The position of the call site.
    pub(crate) caller_pos: Option<Position>,
    pub(crate) bindings: Bindings,
    /// If we are entering a block with extra bindings that are only
    /// defined for the duration of the block, pass them here.
    ///
    /// For example:
    /// ```garden
    /// match (x) { Some(y) => { y + 1; } _ => {}}
    /// ```
    ///
    /// We want `y` to be bound, but only in the block.
    pub(crate) bindings_next_block: Vec<(Symbol, Value)>,
    pub(crate) exprs_to_eval: Vec<(bool, Expression)>,
    pub(crate) evalled_values: Vec<Value>,
}

impl StackFrame {
    fn enter_block(&mut self) {
        self.bindings.0.push(BlockBindings::default());
    }

    fn exit_block(&mut self) {
        self.bindings.0.pop();
        assert!(!self.bindings.0.is_empty());
    }
}

fn most_similar(available: &[&SymbolName], name: &SymbolName) -> Option<SymbolName> {
    let mut res: Vec<_> = available.iter().collect();
    res.sort_by_key(|n| OrderedFloat(normalized_levenshtein(&n.0, &name.0)));
    res.last().map(|n| (**n).clone())
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
pub(crate) struct Session<'a> {
    pub(crate) interrupted: &'a Arc<AtomicBool>,
    pub(crate) has_attached_stdout: bool,
    pub(crate) start_time: Instant,
    pub(crate) trace_exprs: bool,
}

#[derive(Debug)]
pub(crate) enum EvalError {
    Interrupted,
    ResumableError(Position, ErrorMessage),
}

#[derive(Debug)]
pub(crate) struct ToplevelEvalSummary {
    pub(crate) values: Vec<Value>,
    pub(crate) new_syms: Vec<SymbolName>,
    pub(crate) warnings: Vec<Warning>,
    // TODO: Report the names of tests that passed/failed.
    pub(crate) tests_passed: usize,
    pub(crate) tests_failed: usize,
}

/// Evaluate toplevel definitions, but ignore toplevel expressions and tests.
pub(crate) fn eval_toplevel_defs(items: &[ToplevelItem], env: &mut Env) -> ToplevelEvalSummary {
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
pub(crate) fn eval_all_toplevel_items(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &mut Session,
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

    let test_summary = eval_toplevel_tests(items, env, session)?;
    summary.tests_passed = test_summary.tests_passed;
    summary.tests_failed = test_summary.tests_failed;

    if exprs.is_empty() {
        return Ok(summary);
    }

    let value = eval_exprs(&exprs, env, session)?;
    summary.values = vec![value];

    Ok(summary)
}

/// Evaluate toplevel tests.
pub(crate) fn eval_toplevel_tests(
    items: &[ToplevelItem],
    env: &mut Env,
    session: &mut Session,
) -> Result<ToplevelEvalSummary, EvalError> {
    let mut tests_passed = 0;

    let mut test_defs = vec![];
    for item in items {
        if let ToplevelItem::Def(Definition(_, _, Definition_::Test(test))) = item {
            test_defs.push(test);
        }
    }

    // Update all the test definitions in the environment before
    // evaluating anything.
    for test in &test_defs {
        if let Some(test_sym) = &test.name {
            env.tests.insert(test_sym.name.clone(), (*test).clone());
        }
    }

    for test in test_defs {
        push_test_stackframe(test, env);
        eval_env(env, session)?;

        tests_passed += 1;
    }

    Ok(ToplevelEvalSummary {
        values: vec![],
        new_syms: vec![],
        warnings: vec![],
        tests_passed,
        tests_failed: 0,
    })
}

pub(crate) fn push_test_stackframe(test: &TestInfo, env: &mut Env) {
    let enclosing_name = match &test.name {
        Some(name_sym) => name_sym.name.clone(),
        None => SymbolName("__unnamed_test".to_owned()),
    };
    let mut exprs_to_eval: Vec<(bool, Expression)> = vec![];
    for expr in test.body.exprs.iter().rev() {
        exprs_to_eval.push((false, expr.clone()));
    }

    let stack_frame = StackFrame {
        src: test.src_string.clone(),
        enclosing_name,
        enclosing_fun: None,
        caller_pos: None,
        bindings: Bindings::default(),
        bindings_next_block: vec![],
        exprs_to_eval,
        evalled_values: vec![unit_value()],
    };
    env.stack.push(stack_frame);
}

pub(crate) fn eval_defs(definitions: &[Definition], env: &mut Env) -> ToplevelEvalSummary {
    let mut warnings: Vec<Warning> = vec![];
    let mut new_syms: Vec<SymbolName> = vec![];

    for definition in definitions {
        match &definition.2 {
            Definition_::Fun(name_sym, fun_info) => {
                if is_builtin_stub(fun_info) {
                    update_builtin_fun_info(fun_info, env, &mut warnings);
                } else {
                    env.set_with_file_scope(
                        &name_sym.name,
                        Value::Fun(name_sym.clone(), fun_info.clone()),
                    );
                    warnings.extend(check_types_exist(fun_info, env));
                    warnings.extend(check_free_variables(fun_info, env));
                }

                new_syms.push(name_sym.name.clone());
            }
            Definition_::Method(meth_info) => {
                if let MethodKind::UserDefinedMethod(fun_info) = &meth_info.kind {
                    if is_builtin_stub(fun_info) {
                        update_builtin_meth_info(meth_info, fun_info, env, &mut warnings);
                    } else {
                        // TODO: check that types in definitions are defined, and emit
                        // warnings otherwise.
                        //
                        // ```
                        // fun (self: NoSuchType) foo(x: NoSuchType): NoSuchType {}
                        // ```
                        env.add_method(meth_info);
                    }
                }

                new_syms.push(meth_info.name_sym.name.clone());
            }
            Definition_::Test(test) => {
                if let Some(test_sym) = &test.name {
                    env.tests.insert(test_sym.name.clone(), test.clone());
                    new_syms.push(test_sym.name.clone());
                }
            }
            Definition_::Enum(enum_info) => {
                // Add the enum definition to the type environment.
                env.types.insert(
                    enum_info.name_sym.name.clone(),
                    Type::Enum(enum_info.clone()),
                );

                // Add the values in the enum to the value environment.
                for (idx, variant_sym) in enum_info.variants.iter().enumerate() {
                    // TODO: warn if we're clobbering a name from a
                    // different enum (i.e. not just redefining the
                    // current enum).
                    env.set_with_file_scope(
                        &variant_sym.name_sym.name,
                        Value::Enum(enum_info.name_sym.name.clone(), idx, None),
                    );
                }

                let name_as_sym = SymbolName(enum_info.name_sym.name.name.clone());
                new_syms.push(name_as_sym);
            }
            Definition_::Struct(struct_info) => {
                // Add the struct definition to the type environment.
                env.types.insert(
                    struct_info.name_sym.name.clone(),
                    Type::Struct(struct_info.clone()),
                );

                let name_as_sym = SymbolName(struct_info.name_sym.name.name.clone());
                new_syms.push(name_as_sym);
            }
        }
    }

    ToplevelEvalSummary {
        values: vec![],
        new_syms,
        warnings,
        tests_passed: 0,
        tests_failed: 0,
    }
}

fn update_builtin_meth_info(
    meth_info: &MethodInfo,
    fun_info: &FunInfo,
    env: &mut Env,
    warnings: &mut Vec<Warning>,
) {
    let type_name = &meth_info.receiver_type.sym.name;
    let Some(type_methods) = env.methods.get_mut(type_name) else {
        warnings.push(Warning {
            message: format!(
                "Tried to update a built-in stub for a type {} that doesn't exist.",
                type_name
            ),
        });
        return;
    };

    let Some(curr_meth_info) = type_methods.get_mut(&meth_info.name_sym.name) else {
        warnings.push(Warning {
            message: format!(
                "Tried to update a built-in stub for a method {} that doesn't exist on {}.",
                meth_info.name_sym.name, type_name
            ),
        });
        return;
    };

    let MethodKind::BuiltinMethod(kind, _) = &curr_meth_info.kind else {
        warnings.push(Warning {
            message: format!(
                // TODO: we need a better design principle around
                // warning phrasing. It should probably always include
                // an explanation of what will happen (in this case
                // nothing).
                "{}::{} is not a built-in method.",
                type_name, meth_info.name_sym.name
            ),
        });
        return;
    };

    curr_meth_info.kind = MethodKind::BuiltinMethod(*kind, Some(fun_info.clone()));
}

fn update_builtin_fun_info(fun_info: &FunInfo, env: &mut Env, warnings: &mut Vec<Warning>) {
    let Some(symbol) = &fun_info.name else {
        return;
    };

    let Some(value) = env.file_scope.get(&symbol.name) else {
        warnings.push(Warning {
            message: format!(
                "Tried to update a built-in stub for a function {} that doesn't exist.",
                symbol.name
            ),
        });
        return;
    };

    let Value::BuiltinFunction(kind, _) = value else {
        warnings.push(Warning {
            message: format!(
                "Tried to update a built-in stub but {} isn't a built-in function (it's a {}).",
                symbol.name,
                runtime_type(value),
            ),
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

    let expr_ = &exprs[0].1;
    match expr_ {
        Expression_::Variable(variable) => variable.name.0 == "__BUILTIN_IMPLEMENTATION",
        _ => false,
    }
}

// If value is a list of strings, return the strings as a vec. Return
// an error otherwise.
fn as_string_list(value: &Value) -> Result<Vec<String>, Value> {
    match value {
        Value::List(items, _runtime_type) => {
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
    expr_to_eval: (bool, Expression),
    evalled_values: &[Value],
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    stack_frame.exprs_to_eval.push(expr_to_eval);

    env.stack.push(stack_frame);
}

/// Information about an error during evaluation.
#[derive(Debug)]
struct ErrorInfo {
    error_position: Position,
    message: ErrorMessage,
    /// Values that were popped from the stack frame to evaluate the
    /// current subexpression. We will need to restore these in order
    /// to halt in a state where the user can retry.
    restore_values: Vec<Value>,
}

fn eval_if(
    env: &Env,
    stack_frame: &mut StackFrame,
    position: &Position,
    bool_position: &Position,
    then_body: &Block,
    else_body: Option<&Block>,
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    if let Some(b) = to_rust_bool(&condition_value) {
        if b {
            stack_frame.exprs_to_eval.push((
                false,
                Expression(position.clone(), Expression_::Block(then_body.clone())),
            ));
        } else {
            match else_body {
                Some(else_body) => {
                    stack_frame.exprs_to_eval.push((
                        false,
                        Expression(position.clone(), Expression_::Block(else_body.clone())),
                    ));
                }
                None => {
                    stack_frame.evalled_values.push(unit_value());
                }
            }
        }
    } else {
        return Err(ErrorInfo {
            message: format_type_error(
                &TypeName {
                    name: "Bool".into(),
                },
                &condition_value,
                env,
            ),
            restore_values: vec![condition_value],
            error_position: bool_position.clone(),
        });
    }

    Ok(())
}

/// If `value` is a Bool value, convert it to a Rust bool.
fn to_rust_bool(value: &Value) -> Option<bool> {
    match value {
        Value::Enum(name, variant_idx, _) if name.name == "Bool" => {
            // TODO: this assumes users never redefine Bool.
            Some(*variant_idx == 0)
        }
        _ => None,
    }
}

fn eval_while(
    env: &Env,
    stack_frame: &mut StackFrame,
    condition_pos: &Position,
    expr: Expression,
    body: &Block,
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    if let Some(b) = to_rust_bool(&condition_value) {
        if b {
            // Start loop evaluation again.
            stack_frame.exprs_to_eval.push((false, expr.clone()));

            // Evaluate the body.
            stack_frame
                .exprs_to_eval
                .push((false, Expression(expr.0, Expression_::Block(body.clone()))))
        } else {
            // TODO: It's weird using the position of the
            // condition when there's no else.
            stack_frame.evalled_values.push(unit_value());
        }
    } else {
        return Err(ErrorInfo {
            message: format_type_error(
                &TypeName {
                    name: "Bool".into(),
                },
                &condition_value,
                env,
            ),
            restore_values: vec![condition_value],
            error_position: condition_pos.clone(),
        });
    }

    Ok(())
}

fn eval_assign(stack_frame: &mut StackFrame, variable: &Symbol) -> Result<(), ErrorInfo> {
    let var_name = &variable.name;
    if !stack_frame.bindings.has(var_name) {
        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "{} is not currently bound. Try `let {} = something`.",
                var_name, var_name
            )),
            restore_values: vec![],
            error_position: variable.position.clone(),
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .set_existing(var_name, expr_value.clone());
    stack_frame.evalled_values.push(expr_value);

    Ok(())
}

/// Bind `variable` in the current local environment.
fn eval_let(stack_frame: &mut StackFrame, variable: &Symbol) -> Result<(), ErrorInfo> {
    let var_name = &variable.name;
    if stack_frame.bindings.has(var_name) {
        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "{} is already bound. Try `{} = something` instead.",
                var_name, var_name
            )),
            restore_values: vec![],
            error_position: variable.position.clone(),
        });
    }

    if var_name.is_underscore() {
        return Ok(());
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame.bindings.add_new(var_name, expr_value.clone());
    stack_frame.evalled_values.push(expr_value);
    Ok(())
}

fn format_type_error(expected: &TypeName, value: &Value, env: &Env) -> ErrorMessage {
    ErrorMessage(format!(
        "Expected {}, but got {}: {}",
        expected.name,
        runtime_type(value),
        value.display(env)
    ))
}

fn eval_boolean_binop(
    env: &Env,
    stack_frame: &mut StackFrame,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
    {
        let rhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for LHS of binary operator");

        let lhs_bool = match to_rust_bool(&lhs_value) {
            Some(b) => b,
            None => {
                return Err(ErrorInfo {
                    message: format_type_error(
                        &TypeName {
                            name: "Bool".into(),
                        },
                        &lhs_value,
                        env,
                    ),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_position.clone(),
                });
            }
        };

        let rhs_bool = match to_rust_bool(&rhs_value) {
            Some(b) => b,
            None => {
                return Err(ErrorInfo {
                    message: format_type_error(
                        &TypeName {
                            name: "Bool".into(),
                        },
                        &rhs_value,
                        env,
                    ),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_position.clone(),
                });
            }
        };

        match op {
            BinaryOperatorKind::And => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_bool && rhs_bool));
            }
            BinaryOperatorKind::Or => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_bool || rhs_bool));
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn eval_equality_binop(
    stack_frame: &mut StackFrame,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
    let rhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for RHS of binary operator");
    let lhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for LHS of binary operator");

    match op {
        BinaryOperatorKind::Equal => {
            stack_frame
                .evalled_values
                .push(bool_value(lhs_value == rhs_value));
        }
        BinaryOperatorKind::NotEqual => {
            stack_frame
                .evalled_values
                .push(bool_value(lhs_value != rhs_value));
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn eval_integer_binop(
    env: &Env,
    stack_frame: &mut StackFrame,
    position: &Position,
    lhs_position: &Position,
    rhs_position: &Position,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
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
                return Err(ErrorInfo {
                    message: format_type_error(&TypeName { name: "Int".into() }, &lhs_value, env),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_position.clone(),
                });
            }
        };
        let rhs_num = match rhs_value {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: format_type_error(&TypeName { name: "Int".into() }, &rhs_value, env),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_position.clone(),
                });
            }
        };

        match op {
            BinaryOperatorKind::Add => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_add(rhs_num)));
            }
            BinaryOperatorKind::Subtract => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_sub(rhs_num)));
            }
            BinaryOperatorKind::Multiply => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_mul(rhs_num)));
            }
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err(ErrorInfo {
                        // TODO: this looks wrong, it should be the LHS value we print.
                        message: ErrorMessage(format!(
                            "Tried to divide {} by zero.",
                            rhs_value.display(env)
                        )),
                        restore_values: vec![lhs_value, rhs_value.clone()],
                        error_position: position.clone(),
                    });
                }

                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num / rhs_num));
            }
            BinaryOperatorKind::LessThan => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_num < rhs_num));
            }
            BinaryOperatorKind::GreaterThan => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_num > rhs_num));
            }
            BinaryOperatorKind::LessThanOrEqual => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_num <= rhs_num));
            }
            BinaryOperatorKind::GreaterThanOrEqual => {
                stack_frame
                    .evalled_values
                    .push(bool_value(lhs_num >= rhs_num));
            }
            _ => {
                unreachable!()
            }
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
) -> Result<(), ErrorInfo> {
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

        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "Function {} requires {} argument{}, but got {}",
                fun_name,
                expected,
                if expected == 1 { "" } else { "s" },
                arg_values.len()
            )),
            restore_values: saved_values,
            error_position,
        });
    }

    Ok(())
}

/// Check that `value` has `expected` type.
fn check_type(value: &Value, expected: &TypeSymbol, env: &Env) -> Result<(), ErrorMessage> {
    let actual_type = type_representation(value);

    if actual_type != expected.name {
        return Err(format_type_error(&expected.name, value, env));
    }

    Ok(())
}

fn eval_builtin_call(
    env: &Env,
    kind: BuiltinFunctionKind,
    receiver_value: &Value,
    receiver_pos: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    stack_frame: &mut StackFrame,
    position: &Position,
    session: &Session,
) -> Result<(), ErrorInfo> {
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
                    return Err(ErrorInfo {
                        message: ErrorMessage(msg.clone()),
                        restore_values: saved_values,
                        error_position: position.clone(),
                    });
                }
                v => {
                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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
                            kind: ResponseKind::Printed,
                            value: Ok(s.clone()),
                            warnings: vec![],
                        };
                        let serialized = serde_json::to_string(&response).unwrap();
                        println!("{}", serialized);
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
            }
            stack_frame.evalled_values.push(unit_value());
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
                            kind: ResponseKind::Printed,
                            value: Ok(format!("{}\n", s)),
                            warnings: vec![],
                        };
                        let serialized = serde_json::to_string(&response).unwrap();
                        println!("{}", serialized);
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
            }
            stack_frame.evalled_values.push(unit_value());
        }
        BuiltinFunctionKind::DebugPrint => {
            check_arity(
                &SymbolName("dbg".to_owned()),
                receiver_value,
                receiver_pos,
                1,
                arg_positions,
                arg_values,
            )?;

            // TODO: define a proper pretty-printer for values
            // rather than using Rust's Debug.
            let value = &arg_values[0];
            if session.has_attached_stdout {
                println!("{:?}", value);
            } else {
                let response = Response {
                    kind: ResponseKind::Printed,
                    value: Ok(format!("{:?}\n", value)),
                    warnings: vec![],
                };
                let serialized = serde_json::to_string(&response).unwrap();
                println!("{}", serialized);
            }

            stack_frame.evalled_values.push(unit_value());
        }
        BuiltinFunctionKind::Shell => {
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

                            let output = command.output().expect("failed to execute process");

                            let mut s = String::new();
                            // TODO: complain if output is not UTF-8.
                            s.write_str(&String::from_utf8_lossy(&output.stdout))
                                .unwrap();
                            s.write_str(&String::from_utf8_lossy(&output.stderr))
                                .unwrap();

                            let v = if output.status.success() {
                                result_ok_value(Value::String(s))
                            } else {
                                result_err_value(Value::String(s))
                            };

                            stack_frame.evalled_values.push(v);
                        }
                        Err(v) => {
                            let mut saved_values = vec![];
                            for value in arg_values.iter().rev() {
                                saved_values.push(value.clone());
                            }
                            saved_values.push(receiver_value.clone());

                            return Err(ErrorInfo {
                                message: format_type_error(
                                    &TypeName {
                                        name: "List".into(),
                                    },
                                    &v,
                                    env,
                                ),
                                restore_values: saved_values,
                                error_position: arg_positions[0].clone(),
                            });
                        }
                    }
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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

            stack_frame
                .evalled_values
                .push(Value::String(arg_values[0].display(env)));
        }
        BuiltinFunctionKind::PathExists => {
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
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
            };

            let path = PathBuf::from(path_s);
            stack_frame.evalled_values.push(bool_value(path.exists()));
        }
        BuiltinFunctionKind::ListDirectory => {
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

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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

                    Value::List(items, RuntimeType::string_list())
                }
                Err(_) => {
                    // TODO: list_directory() should return a Result
                    // rather than silently returning an empty list on
                    // failure.
                    Value::List(vec![], RuntimeType::empty_list())
                }
            };

            stack_frame.evalled_values.push(value);
        }
        BuiltinFunctionKind::ReadFile => {
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

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
            };

            let path = PathBuf::from(path_s);

            let v = match std::fs::read_to_string(path) {
                Ok(s) => result_ok_value(Value::String(s)),
                Err(e) => result_err_value(Value::String(e.to_string())),
            };

            stack_frame.evalled_values.push(v);
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

            stack_frame
                .evalled_values
                .push(Value::String(path.display().to_string()));
        }
    }

    Ok(())
}

/// Evaluate a function call.
///
/// If we're calling a userland function, return the new stackframe to
/// evaluate next.
fn eval_call(
    env: &Env,
    stack_frame: &mut StackFrame,
    position: &Position,
    arg_positions: &[Position],
    arg_values: &[Value],
    receiver_value: &Value,
    receiver_pos: &Position,
    session: &Session,
) -> Result<Option<StackFrame>, ErrorInfo> {
    match &receiver_value {
        Value::Closure(bindings, fun_info) => {
            let mut bindings = bindings.clone();

            if fun_info.params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    message: ErrorMessage(format!(
                        "Closure expects {} argument{}, but got {}",
                        fun_info.params.len(),
                        if fun_info.params.len() == 1 { "" } else { "s" },
                        arg_values.len()
                    )),
                    restore_values: saved_values,
                    error_position: receiver_pos.clone(),
                });
            }

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in fun_info.body.exprs.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
                let param_name = &param.symbol.name;
                if !param_name.is_underscore() {
                    fun_bindings.insert(param_name.clone(), value.clone());
                }
            }

            bindings.push(BlockBindings(Rc::new(RefCell::new(fun_bindings))));

            return Ok(Some(StackFrame {
                caller_pos: Some(position.clone()),
                bindings: Bindings(bindings),
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![unit_value()],
                enclosing_fun: Some(fun_info.clone()),
                enclosing_name: SymbolName("(closure)".to_string()),
                src: fun_info.src_string.clone(),
            }));
        }
        Value::Fun(name_sym, fi @ FunInfo { params, body, .. }) => {
            // Calling a user-defined function.

            check_arity(
                &name_sym.name,
                receiver_value,
                receiver_pos,
                params.len(),
                arg_positions,
                arg_values,
            )?;

            check_param_types(env, receiver_value, params, arg_positions, arg_values)?;

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in body.exprs.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
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
                caller_pos: Some(receiver_pos.clone()),
                enclosing_name: name_sym.name.clone(),
                bindings: Bindings::new_with(fun_bindings),
                bindings_next_block: vec![],
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![unit_value()],
            }));
        }
        Value::BuiltinFunction(kind, _) => eval_builtin_call(
            env,
            *kind,
            receiver_value,
            receiver_pos,
            arg_positions,
            arg_values,
            stack_frame,
            position,
            session,
        )?,
        v => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            return Err(ErrorInfo {
                error_position: receiver_pos.clone(),
                message: format_type_error(
                    &TypeName {
                        name: "Function".into(),
                    },
                    v,
                    env,
                ),
                restore_values: saved_values,
            });
        }
    }

    Ok(None)
}

fn eval_enum_constructor(
    env: &Env,
    position: &Position,
    arg_values: &[Value],
    receiver_pos: &Position,
    receiver_value: &Value,
) -> Result<Value, ErrorInfo> {
    match receiver_value {
        Value::Enum(ref name, variant_idx, None) => {
            let wrapped_value = if arg_values.len() == 1 {
                &arg_values[0]
            } else {
                return Err(ErrorInfo {
                    error_position: position.clone(),
                    message: ErrorMessage(format!(
                        "Enum variant constructors should take 1 argument, got {}",
                        arg_values.len()
                    )),
                    restore_values: vec![],
                });
            };

            match env.types.get(name) {
                Some(type_) => match type_ {
                    // TODO: these are probably reachable if the user
                    // defines an enum whose name clashes with
                    // built-in types.
                    Type::Builtin(_) => unreachable!(),
                    Type::Struct(_) => unreachable!(),
                    Type::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => {
                            if variant_sym.payload_hint.is_some() {
                                Ok(Value::Enum(
                                    name.clone(),
                                    *variant_idx,
                                    Some(Box::new(wrapped_value.clone())),
                                ))
                            } else {
                                let mut saved_values = vec![];
                                for value in arg_values.iter().rev() {
                                    saved_values.push(value.clone());
                                }
                                saved_values.push(receiver_value.clone());

                                Err(ErrorInfo {
                                    error_position: position.clone(),
                                    message: ErrorMessage(format!(
                                        "{}::{} does not take an argument",
                                        name, variant_sym.name_sym.name
                                    )),
                                    restore_values: saved_values,
                                })
                            }
                        }
                        None => {
                            // Assume that the previous definition
                            // accepted a payload.
                            //
                            // TODO: store a copy of the old
                            // definition so this behaves the same as
                            // the current definition.
                            Ok(Value::Enum(
                                name.clone(),
                                *variant_idx,
                                Some(Box::new(wrapped_value.clone())),
                            ))
                        }
                    },
                },
                None => {
                    // Type no longer exists.
                    //
                    // TODO: store a copy of the old definition so
                    // this behaves the same as the current
                    // definition.
                    Err(ErrorInfo {
                        error_position: position.clone(),
                        message: ErrorMessage(format!("{name} no longer has this variant")),
                        restore_values: vec![],
                    })
                }
            }
        }
        _ => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            Err(ErrorInfo {
                error_position: receiver_pos.clone(),
                message: format_type_error(
                    &TypeName {
                        name: "Function".into(),
                    },
                    receiver_value,
                    env,
                ),
                restore_values: saved_values,
            })
        }
    }
}

fn check_param_types(
    env: &Env,
    receiver_value: &Value,
    params: &[SymbolWithType],
    arg_positions: &[Position],
    arg_values: &[Value],
) -> Result<(), ErrorInfo> {
    for (i, (param, arg_value)) in params.iter().zip(arg_values).enumerate() {
        if let Some(param_ty) = &param.type_ {
            if let Err(msg) = check_type(arg_value, &param_ty.sym, env) {
                let mut saved_values = vec![];
                saved_values.push(receiver_value.clone());
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    error_position: arg_positions[i].clone(),
                    message: ErrorMessage(format!("Incorrect type for argument: {}", msg.0)),
                    restore_values: saved_values,
                });
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
    receiver_pos: &Position,
    meth_name: &Symbol,
    args: &[Expression],
) -> Result<Option<StackFrame>, ErrorInfo> {
    let mut arg_values: Vec<Value> = vec![];
    let mut arg_positions: Vec<Position> = vec![];
    for arg in args {
        arg_values.push(
            stack_frame
                .evalled_values
                .pop()
                .expect("Popped an empty value for stack for method call arguments."),
        );
        arg_positions.push(arg.0.clone());
    }
    let receiver_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for method call receiver.");

    let receiver_type_name = type_representation(&receiver_value);
    let receiver_method = match env.methods.get(&receiver_type_name) {
        Some(receiver_methods) => {
            if let Some(method) = receiver_methods.get(&meth_name.name) {
                method
            } else {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    message: ErrorMessage(format!(
                        "No method named `{}` on `{}`.",
                        meth_name.name, receiver_type_name
                    )),
                    restore_values: saved_values,
                    error_position: meth_name.position.clone(),
                });
            }
        }
        None => {
            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            return Err(ErrorInfo {
                message: ErrorMessage(format!("No methods defined on `{}`.", receiver_type_name)),
                restore_values: saved_values,
                error_position: meth_name.position.clone(),
            });
        }
    };

    let fun_info = match &receiver_method.kind {
        MethodKind::BuiltinMethod(kind, _) => {
            eval_builtin_method_call(
                env,
                *kind,
                &receiver_value,
                receiver_pos,
                &arg_positions,
                &arg_values,
                stack_frame,
            )?;
            return Ok(None);
        }
        MethodKind::UserDefinedMethod(fun_info) => fun_info.clone(),
    };

    let mut method_subexprs: Vec<(bool, Expression)> = vec![];
    for expr in fun_info.body.exprs.iter().rev() {
        method_subexprs.push((false, expr.clone()));
    }

    // TODO: use a fully qualified method name here?
    check_arity(
        &meth_name.name,
        &receiver_value,
        receiver_pos,
        fun_info.params.len(),
        &arg_positions,
        &arg_values,
    )?;

    // TODO: check for duplicate parameter names.
    // TODO: parameter names must not clash with the receiver name.
    let mut fun_bindings = HashMap::new();
    for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
        let param_name = &param.symbol.name;
        fun_bindings.insert(param_name.clone(), value.clone());
    }
    fun_bindings.insert(receiver_method.receiver_name.clone(), receiver_value);

    Ok(Some(StackFrame {
        enclosing_fun: Some(fun_info.clone()),
        enclosing_name: SymbolName(format!("{}::{}", receiver_type_name, meth_name.name)),
        src: fun_info.src_string.clone(),
        caller_pos: Some(receiver_pos.clone()),
        bindings: Bindings::new_with(fun_bindings),
        bindings_next_block: vec![],
        exprs_to_eval: method_subexprs,
        evalled_values: vec![unit_value()],
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
) -> Result<(), ErrorInfo> {
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
                Value::List(items, runtime_type) => {
                    let mut new_items = items.clone();
                    new_items.push(arg_values[0].clone());

                    // TODO: check that the new value has the same
                    // type as the existing list items.
                    stack_frame
                        .evalled_values
                        .push(Value::List(new_items, runtime_type.clone()));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "List".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: receiver_pos.clone(),
                    });
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
                (Value::List(items, _element_type), Value::Integer(i)) => {
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

                        return Err(ErrorInfo {
                            message,
                            restore_values: saved_values,
                            error_position: arg_positions[0].clone(),
                        });
                    } else {
                        *i as usize
                    };

                    stack_frame.evalled_values.push(items[index].clone());
                }
                (v, Value::Integer(_)) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "List".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
                (_, v) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(&TypeName { name: "Int".into() }, v, env),
                        restore_values: saved_values,
                        error_position: arg_positions[1].clone(),
                    });
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
                Value::List(items, _) => {
                    stack_frame
                        .evalled_values
                        .push(Value::Integer(items.len() as i64));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "List".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
                }
            }
        }
        BuiltinMethodKind::StringConcat => {
            check_arity(
                &SymbolName("String::concat".to_owned()),
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

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[1].clone(),
                    });
                }
            };

            arg1.push_str(arg2);
            stack_frame.evalled_values.push(Value::String(arg1));
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
                    stack_frame
                        .evalled_values
                        .push(Value::Integer(s.chars().count() as i64));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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

                    return Err(ErrorInfo {
                        message: format_type_error(
                            &TypeName {
                                name: "String".into(),
                            },
                            v,
                            env,
                        ),
                        restore_values: saved_values,
                        error_position: arg_positions[0].clone(),
                    });
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

                    return Err(ErrorInfo {
                        message: format_type_error(&TypeName { name: "Int".into() }, v, env),
                        restore_values: saved_values,
                        error_position: arg_positions[1].clone(),
                    });
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

                    return Err(ErrorInfo {
                        message: format_type_error(&TypeName { name: "Int".into() }, v, env),
                        restore_values: saved_values,
                        error_position: arg_positions[2].clone(),
                    });
                }
            };

            if *from_arg < 0 {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err(ErrorInfo {
                    message: ErrorMessage(format!("The first argument to String::substring must be greater than 0, but got: {}", from_arg)),
                        restore_values: saved_values,
                        error_position: arg_positions[1].clone(),
                    });
            }

            if from_arg > to_arg {
                let mut saved_values = vec![];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }
                saved_values.push(receiver_value.clone());

                return Err(ErrorInfo {
                    message: ErrorMessage(format!("The first argument to String::substring cannot be greater than the second, but got: {} and {}", from_arg, to_arg)),
                        restore_values: saved_values,
                        error_position: arg_positions[1].clone(),
                    });
            }

            stack_frame.evalled_values.push(Value::String(
                s_arg
                    .chars()
                    .skip(*from_arg as usize)
                    .take((to_arg - from_arg) as usize)
                    .collect(),
            ));
        }
    }

    Ok(())
}

pub(crate) fn eval_env(env: &mut Env, session: &mut Session) -> Result<Value, EvalError> {
    while let Some(mut stack_frame) = env.stack.pop() {
        if let Some((done_children, Expression(expr_position, expr_))) =
            stack_frame.exprs_to_eval.pop()
        {
            if session.interrupted.load(Ordering::SeqCst) {
                session.interrupted.store(false, Ordering::SeqCst);
                restore_stack_frame(
                    env,
                    stack_frame,
                    (done_children, Expression(expr_position, expr_)),
                    &[],
                );
                return Err(EvalError::Interrupted);
            }

            let expr_copy = expr_.clone();

            if session.trace_exprs {
                println!("{:?} {}", expr_, done_children);
            }
            match expr_ {
                Expression_::Match(scrutinee, cases) => {
                    if done_children {
                        eval_match_cases(env, &mut stack_frame, &scrutinee.0, &cases)?;
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *scrutinee.clone()));
                    }
                }
                Expression_::If(condition, ref then_body, ref else_body) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_if(
                            env,
                            &mut stack_frame,
                            &expr_position,
                            &condition.0,
                            then_body,
                            else_body.as_ref(),
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *condition.clone()));
                    }
                }
                Expression_::While(condition, ref body) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_while(
                            env,
                            &mut stack_frame,
                            &condition.0,
                            Expression(expr_position.clone(), expr_copy.clone()),
                            body,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *condition.clone()));
                    }
                }
                Expression_::Return(expr) => {
                    if done_children {
                        // No more expressions to evaluate in this function.
                        stack_frame.exprs_to_eval.clear();
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *expr.clone()));
                    }
                }
                Expression_::Assign(variable, expr) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_assign(&mut stack_frame, &variable)
                        {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *expr.clone()));
                    }
                }
                Expression_::Let(variable, expr) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_let(&mut stack_frame, &variable)
                        {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *expr.clone()));
                    }
                }
                Expression_::IntLiteral(i) => {
                    stack_frame.evalled_values.push(Value::Integer(i));
                }
                Expression_::StringLiteral(s) => {
                    stack_frame.evalled_values.push(Value::String(s));
                }
                Expression_::ListLiteral(items) => {
                    if done_children {
                        let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                        let mut element_type = RuntimeType::no_value();

                        for _ in 0..items.len() {
                            let element = stack_frame.evalled_values.pop().expect(
                                "Value stack should have sufficient items for the list literal",
                            );
                            // TODO: check that all elements are of a compatible type.
                            // [1, None] should be an error.
                            element_type = runtime_type(&element);
                            list_values.push(element);
                        }

                        stack_frame
                            .evalled_values
                            .push(Value::List(list_values, element_type));
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));

                        for item in items.iter() {
                            stack_frame.exprs_to_eval.push((false, item.clone()));
                        }
                    }
                }
                Expression_::StructLiteral(name_sym, field_exprs) => {
                    // TODO: we should check that the definition of this struct still matches these fields.
                    if done_children {
                        let mut fields = HashMap::new();

                        for _ in 0..field_exprs.len() {
                            let field_value = stack_frame.evalled_values.pop().expect(
                                "Value stack should have sufficient items for the struct literal",
                            );
                            // TODO: check that all field values are of a compatible type.
                            todo!();
                        }

                        stack_frame
                            .evalled_values
                            .push(Value::Struct(name_sym.name, fields));
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));

                        for (_, field_expr) in field_exprs.iter() {
                            stack_frame.exprs_to_eval.push((false, field_expr.clone()));
                        }
                    }
                }
                Expression_::Variable(name_sym) => {
                    if let Some(value) = get_var(&name_sym.name, &stack_frame, env) {
                        stack_frame.evalled_values.push(value);
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
                            (done_children, Expression(expr_position, expr_copy)),
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
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_integer_binop(
                            env,
                            &mut stack_frame,
                            &expr_position,
                            &lhs.0,
                            &rhs.0,
                            op,
                        ) {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                        stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                    }
                }
                Expression_::BinaryOperator(
                    lhs,
                    op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
                    rhs,
                ) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_equality_binop(&mut stack_frame, op)
                        {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                        stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                    }
                }
                Expression_::BinaryOperator(
                    lhs,
                    op @ (BinaryOperatorKind::And | BinaryOperatorKind::Or),
                    rhs,
                ) => {
                    if done_children {
                        if let Err(ErrorInfo {
                            message,
                            restore_values,
                            error_position: position,
                        }) = eval_boolean_binop(env, &mut stack_frame, &lhs.0, &rhs.0, op)
                        {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &restore_values,
                            );
                            return Err(EvalError::ResumableError(position, message));
                        }
                    } else {
                        // TODO: do short-circuit evaluation of && and ||.
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));
                        stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                        stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                    }
                }
                Expression_::FunLiteral(fun_info) => {
                    stack_frame
                        .evalled_values
                        .push(Value::Closure(stack_frame.bindings.0.clone(), fun_info));
                }
                Expression_::Call(receiver, ref args) => {
                    if done_children {
                        let mut arg_values = vec![];
                        let mut arg_positions = vec![];
                        for arg in args {
                            arg_values.push(
                                stack_frame
                                    .evalled_values
                                    .pop()
                                    .expect("Popped an empty value for stack for call arguments"),
                            );
                            arg_positions.push(arg.0.clone());
                        }
                        let receiver_value = stack_frame
                            .evalled_values
                            .pop()
                            .expect("Popped an empty value stack for call receiver");

                        if matches!(receiver_value, Value::Enum(_, _, _)) {
                            match eval_enum_constructor(
                                env,
                                &expr_position,
                                &arg_values,
                                &receiver.0,
                                &receiver_value,
                            ) {
                                Ok(value) => {
                                    stack_frame.evalled_values.push(value);
                                }
                                Err(ErrorInfo {
                                    message,
                                    restore_values,
                                    error_position,
                                }) => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(expr_position, expr_copy)),
                                        &restore_values,
                                    );
                                    return Err(EvalError::ResumableError(error_position, message));
                                }
                            }
                        } else {
                            match eval_call(
                                env,
                                &mut stack_frame,
                                &expr_position,
                                &arg_positions,
                                &arg_values,
                                &receiver_value,
                                &receiver.0,
                                session,
                            ) {
                                Ok(Some(new_stack_frame)) => {
                                    env.stack.push(stack_frame);
                                    env.stack.push(new_stack_frame);
                                    continue;
                                }
                                Ok(None) => {}
                                Err(ErrorInfo {
                                    message,
                                    restore_values,
                                    error_position: position,
                                }) => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(expr_position, expr_copy)),
                                        &restore_values,
                                    );
                                    return Err(EvalError::ResumableError(position, message));
                                }
                            }
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));

                        for arg in args {
                            stack_frame.exprs_to_eval.push((false, arg.clone()));
                        }
                        // Push the receiver after arguments, so
                        // we evaluate it before arguments. This
                        // makes it easier to use :replace on bad
                        // functions.
                        stack_frame.exprs_to_eval.push((false, *receiver.clone()));
                    }
                }
                Expression_::MethodCall(receiver_expr, meth_name, args) => {
                    if done_children {
                        match eval_method_call(
                            env,
                            &mut stack_frame,
                            &receiver_expr.0,
                            &meth_name,
                            &args,
                        ) {
                            Ok(Some(new_stack_frame)) => {
                                env.stack.push(stack_frame);
                                env.stack.push(new_stack_frame);
                                continue;
                            }
                            Ok(None) => {}
                            Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position,
                            }) => {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                );
                                return Err(EvalError::ResumableError(error_position, message));
                            }
                        }
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));

                        for arg in args {
                            stack_frame.exprs_to_eval.push((false, arg.clone()));
                        }
                        // Push the receiver after arguments, so
                        // we evaluate it before arguments.
                        stack_frame
                            .exprs_to_eval
                            .push((false, *receiver_expr.clone()));
                    }
                }
                Expression_::Block(block) => {
                    if done_children {
                        stack_frame.exit_block();
                    } else {
                        stack_frame
                            .exprs_to_eval
                            .push((true, Expression(expr_position, expr_copy)));

                        stack_frame.enter_block();

                        let bindings_next_block =
                            std::mem::take(&mut stack_frame.bindings_next_block);
                        for (sym, expr) in bindings_next_block {
                            stack_frame.bindings.add_new(&sym.name, expr);
                        }

                        for expr in block.exprs.iter().rev() {
                            stack_frame.exprs_to_eval.push((false, expr.clone()));
                        }
                    }
                }
            }
        }

        if stack_frame.exprs_to_eval.is_empty() {
            // No more statements in this stack frame.
            if env.stack.is_empty() {
                // Don't pop the outer scope: that's for the top level environment.
                env.stack.push(stack_frame);
                break;
            }

            // Check that the value matches the return type.
            let return_value = stack_frame
                .evalled_values
                .pop()
                .expect("Should have a value");

            if let Some(ref fun) = stack_frame.enclosing_fun {
                if let Some(return_type) = &fun.return_type {
                    if let Err(msg) = check_type(&return_value, &return_type.sym, env) {
                        stack_frame.evalled_values.push(return_value.clone());
                        env.stack.push(stack_frame);

                        return Err(EvalError::ResumableError(Position::todo(), msg));
                    }
                }
            }

            // The final evaluation result of the function
            // call should be used in the previous stack
            // frame.
            env.stack
                .last_mut()
                .unwrap()
                .evalled_values
                .push(return_value);
        } else {
            // Keep going on this stack frame.
            env.stack.push(stack_frame);
        }
    }

    Ok(env
        .stack
        .last_mut()
        .expect("toplevel stack frame should exist")
        .evalled_values
        .pop()
        .expect("Should have a value from the last expression"))
}

fn eval_match_cases(
    env: &mut Env,
    stack_frame: &mut StackFrame,
    scrutinee_pos: &Position,
    cases: &[(Pattern, Box<Expression>)],
) -> Result<(), EvalError> {
    let scrutinee_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for match");

    let Value::Enum(value_type_name, value_variant_idx, value_payload) = scrutinee_value else {
        let msg = ErrorMessage(format!(
            "Expected an enum value, but got {}: {}",
            runtime_type(&scrutinee_value),
            scrutinee_value.display(env)
        ));
        return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
    };

    let _type = match env.types.get(&value_type_name) {
        Some(type_) => type_,
        None => {
            let msg = ErrorMessage(format!(
                "Could not find an enum type named {value_type_name}",
            ));
            return Err(EvalError::ResumableError(scrutinee_pos.clone(), msg));
        }
    };

    for (pattern, case_expr) in cases {
        if pattern.symbol.name.is_underscore() {
            stack_frame
                .exprs_to_eval
                .push((false, (**case_expr).clone()));
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

        let Value::Enum(pattern_type_name, pattern_variant_idx, _) = value else {
            // TODO: error messages should include examples of valid code.
            let msg = ErrorMessage(format!(
                "Patterns must be enum variants, got `{}`",
                value.display(env)
            ));
            return Err(EvalError::ResumableError(
                pattern.symbol.position.clone(),
                msg,
            ));
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

            let case_expr_pos = &case_expr.0;
            let case_block = Expression_::Block(Block {
                open_brace: case_expr_pos.clone(),
                exprs: vec![(**case_expr).clone()],
                close_brace: case_expr_pos.clone(),
            });

            stack_frame.bindings_next_block = bindings;

            stack_frame
                .exprs_to_eval
                .push((false, Expression(case_expr_pos.clone(), case_block)));
            return Ok(());
        }
    }

    let msg = ErrorMessage("Non-exhaustive match expression".to_string());
    Err(EvalError::ResumableError(scrutinee_pos.clone(), msg))
}

pub(crate) fn eval_exprs(
    exprs: &[Expression],
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    let mut exprs_to_eval = vec![];
    for expr in exprs.iter().rev() {
        exprs_to_eval.push((false, expr.clone()));
    }

    let top_stack = env.stack.last_mut().unwrap();
    // TODO: do this setup outside of this function.
    top_stack.exprs_to_eval = exprs_to_eval;

    eval_env(env, session)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use garden_lang_parser::ast::Position;
    use garden_lang_parser::{parse_defs_from_str, parse_exprs_from_str, parse_toplevel_items};

    use super::*;

    fn eval_exprs(exprs: &[Expression], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut session = Session {
            interrupted: &interrupted,
            has_attached_stdout: false,
            start_time: Instant::now(),
            trace_exprs: false,
        };

        super::eval_exprs(exprs, env, &mut session)
    }

    #[test]
    fn test_eval_equality() {
        let exprs = parse_exprs_from_str("\"a\" == \"b\";").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(false));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let exprs = vec![Expression(
            Position {
                start_offset: 0,
                end_offset: 0,
                line_number: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Let(
                Symbol {
                    position: Position {
                        start_offset: 0,
                        end_offset: 0,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn"),
                    },
                    name: SymbolName("foo".into()),
                },
                Box::new(Expression(
                    Position {
                        start_offset: 0,
                        end_offset: 0,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn"),
                    },
                    Expression_::IntLiteral(123),
                )),
            ),
        )];
        eval_exprs(&exprs, &mut env).unwrap();

        let exprs = vec![Expression(
            Position {
                start_offset: 0,
                end_offset: 0,
                line_number: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Variable(Symbol {
                position: Position {
                    start_offset: 0,
                    end_offset: 0,
                    line_number: 0,
                    path: PathBuf::from("__test.gdn"),
                },
                name: SymbolName("foo".into()),
            }),
        )];
        eval_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_exprs() {
        let exprs = parse_exprs_from_str("True; False;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(false));
    }

    #[test]
    fn test_eval_add() {
        let exprs = parse_exprs_from_str("1 + 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_less_than() {
        let exprs = parse_exprs_from_str("1 < 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(true));
    }

    #[test]
    fn test_eval_less_than_or_equal() {
        let exprs = parse_exprs_from_str("3 <= 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(false));
    }

    #[test]
    fn test_eval_list_literal() {
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4];").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(
                vec![Value::Integer(3), Value::Integer(12)],
                RuntimeType::int()
            )
        );
    }

    #[test]
    fn test_eval_block() {
        let exprs = parse_exprs_from_str("{ let x = 1; x + 1; };").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_block_scope_should_not_leak() {
        let exprs = parse_exprs_from_str("{ let x = 1; }; x;").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_let() {
        let exprs = parse_exprs_from_str("let foo = True; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let exprs = parse_exprs_from_str("let foo = True; let foo = False;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env);
        assert!(value.is_err());
    }

    #[test]
    fn test_eval_if() {
        let exprs = parse_exprs_from_str("let foo = if (True) { 1; } else { 2; }; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let exprs = parse_exprs_from_str("if (True) { let x = 1; } x;").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, unit_value());
    }

    #[test]
    fn test_eval_list_append() {
        let exprs = parse_exprs_from_str("[1, 2].append(3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(
                vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)],
                RuntimeType::int()
            )
        );
    }

    #[test]
    fn test_eval_list_get() {
        let exprs = parse_exprs_from_str("[10, 11].get(1);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(11));
    }

    #[test]
    fn test_eval_list_get_out_of_bounds() {
        let exprs = parse_exprs_from_str("[10, 11].get(2);").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_get_empty() {
        let exprs = parse_exprs_from_str("[].get(0);").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_list_length() {
        let exprs = parse_exprs_from_str("[0, 1].len();").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_string_length() {
        let exprs = parse_exprs_from_str("\"abc\".len();").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_string_substring() {
        let exprs = parse_exprs_from_str("\"abcdef\".substring(1, 3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::String("bc".into()));
    }

    #[test]
    fn test_eval_string_concat() {
        let exprs = parse_exprs_from_str("\"abc\".concat(\"def\");").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::String("abcdef".into()));
    }

    #[test]
    fn test_eval_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { True; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x) { x; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(123);").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x, y) { y; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(1, 2);").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_call_closure_immediately() {
        let mut env = Env::default();

        let defs =
            parse_defs_from_str("fun f() { let x = 1; let f = fun() { x; }; f(); }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_call_bad_arity() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x) { }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_return_closure_and_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let x = 1; fun() { x; }; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("let y = f(); y();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_method_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun (self: String) f() { True; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, bool_value(true));
    }

    #[test]
    fn test_eval_method_call_bad_airty() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun (self: String) f() { True; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f(123);").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_while() {
        let exprs = parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, unit_value());
    }

    #[test]
    fn test_eval_while_block_scope_does_not_leak() {
        let exprs =
            parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1; let x = 1; }").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_env_after_call() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun id(x) { x; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("let i = 0; id(i); i;").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(0));
    }

    #[test]
    fn test_eval_return() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { return 1; 2; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_correct_return_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): Int { 1; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_wrong_argument_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(x: Int) { }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(True);").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): String { 1; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_wrong_return_type_early_return() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(): String { return 1; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_underscore_param_not_bound() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f(_) { _; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f(1);").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_not_bound() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let _ = 1; _; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_local_underscore_repeated() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun f() { let _ = 1; let _ = 2; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_ok());
    }

    #[test]
    fn test_eval_match() {
        let exprs =
            parse_exprs_from_str("let x = Some(1); match (x) { Some(i) => i + 1 _ => {}}").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_empty_test() {
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut session = Session {
            interrupted: &interrupted,
            has_attached_stdout: false,
            start_time: Instant::now(),
            trace_exprs: false,
        };

        let mut env = Env::default();

        let defs = parse_toplevel_items(&PathBuf::new(), "test f {}").unwrap();
        let eval_result = eval_all_toplevel_items(&defs, &mut env, &mut session);
        assert!(eval_result.is_ok());
    }
}
