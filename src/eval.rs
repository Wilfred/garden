use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Write;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use ordered_float::OrderedFloat;
use strsim::normalized_levenshtein;
use strum::IntoEnumIterator;

use crate::ast::{
    BinaryOperatorKind, Block, BuiltinMethodKind, FunInfo, MethodInfo, MethodKind, Position,
    SourceString, Symbol, SymbolWithType, TestInfo, ToplevelItem, TypeName,
};
use crate::ast::{Definition, Definition_, Expression, Expression_, SymbolName};
use crate::json_session::{Response, ResponseKind};
use crate::values::{type_representation, BuiltinFunctionKind, Value};

// TODO: Is it correct to define equality here? Closures should only
// have reference equality probably.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockBindings(Rc<RefCell<HashMap<SymbolName, Value>>>);

impl Default for BlockBindings {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(HashMap::new())))
    }
}

#[derive(Debug)]
pub struct Bindings(Vec<BlockBindings>);

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

    pub fn has(&self, name: &SymbolName) -> bool {
        self.get(name).is_some()
    }

    /// Remove `name` from bindings. If this variable is shadowed,
    /// remove the innermost binding.
    pub fn remove(&mut self, name: &SymbolName) {
        for block_bindings in self.0.iter_mut().rev() {
            if block_bindings.0.borrow().get(name).is_some() {
                block_bindings.0.borrow_mut().remove(name);
            }
        }
    }

    fn add_new(&mut self, name: &SymbolName, value: Value) {
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

    pub fn all(&self) -> Vec<(SymbolName, Value)> {
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
pub struct StackFrame {
    pub src: SourceString,
    // The name of the function, method or test that we're evaluating.
    pub enclosing_name: SymbolName,
    pub enclosing_fun: Option<FunInfo>,
    /// The position of the call site.
    pub caller_pos: Option<Position>,
    pub bindings: Bindings,
    pub exprs_to_eval: Vec<(bool, Expression)>,
    pub evalled_values: Vec<(Position, Value)>,
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

#[derive(Debug)]
pub struct Env {
    // TODO: trace_exprs would be clearer in Session.
    pub trace_exprs: bool,
    pub file_scope: HashMap<SymbolName, Value>,
    pub methods: HashMap<TypeName, HashMap<SymbolName, MethodInfo>>,
    pub tests: HashMap<SymbolName, TestInfo>,
    pub types: Vec<TypeName>,
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
        let types = vec![
            // TODO: String literals are duplicated with type_representation.
            TypeName("Int".into()),
            TypeName("Bool".into()),
            TypeName("Fun".into()),
            TypeName("String".into()),
            TypeName("List".into()),
            TypeName("Void".into()),
        ];

        Self {
            trace_exprs: false,
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
        }
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
pub struct Session<'a> {
    pub history: String,
    pub interrupted: &'a Arc<AtomicBool>,
    pub has_attached_stdout: bool,
}

#[derive(Debug)]
pub enum EvalError {
    Interrupted,
    ResumableError(Position, ErrorMessage),
}

#[derive(Debug)]
pub struct ToplevelEvalSummary {
    pub values: Vec<Value>,
    // TODO: Prefer Vec<SymbolName>
    pub definitions: usize,
    pub tests_passed: usize,
    pub tests_failed: usize,
}

pub fn eval_toplevel_defs(items: &[ToplevelItem], env: &mut Env) -> ToplevelEvalSummary {
    let mut defs = vec![];
    for item in items {
        match item {
            ToplevelItem::Def(def) => {
                defs.push(def.clone());
            }
            ToplevelItem::Expr(_) => {}
        }
    }

    eval_defs(&defs, env);
    ToplevelEvalSummary {
        values: vec![],
        definitions: defs.len(),
        tests_passed: 0,
        tests_failed: 0,
    }
}

pub fn eval_toplevel_items(
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
                exprs.push(expr.1.clone());
            }
        }
    }

    eval_defs(&defs, env);

    let test_summary = eval_toplevel_tests(items, env, session)?;

    if exprs.is_empty() {
        return Ok(ToplevelEvalSummary {
            values: vec![],
            definitions: defs.len(),
            tests_passed: test_summary.tests_passed,
            tests_failed: test_summary.tests_failed,
        });
    }

    let value = eval_exprs(&exprs, env, session)?;
    Ok(ToplevelEvalSummary {
        values: vec![value],
        definitions: defs.len(),
        tests_passed: test_summary.tests_passed,
        tests_failed: test_summary.tests_failed,
    })
}

pub fn eval_toplevel_tests(
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
        push_test_stackframe(test, env, session);
        eval_env(env, session)?;

        tests_passed += 1;
    }

    Ok(ToplevelEvalSummary {
        values: vec![],
        definitions: 0,
        tests_passed,
        tests_failed: 0,
    })
}

pub fn push_test_stackframe(test: &TestInfo, env: &mut Env, session: &mut Session<'_>) {
    let enclosing_name = match &test.name {
        Some(name) => name.name.clone(),
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
        exprs_to_eval,
        evalled_values: vec![],
    };
    env.stack.push(stack_frame);
}

pub fn eval_defs(definitions: &[Definition], env: &mut Env) {
    for definition in definitions {
        // TODO: check that types in definitions are defined, and emit
        // warnings otherwise.
        //
        // ```
        // fun (self: NoSuchType) foo(x: NoSuchType): NoSuchType {}
        // ```
        match &definition.2 {
            Definition_::Fun(name, fun_info) => {
                env.set_with_file_scope(&name.name, Value::Fun(name.clone(), fun_info.clone()));
            }
            Definition_::Method(meth_info) => {
                env.add_method(meth_info);
            }
            Definition_::Test(_) => {}
        }
    }
}

// If value is a list of strings, return the strings as a vec. Return
// an error otherwise.
fn as_string_list(value: &Value) -> Result<Vec<String>, Value> {
    match value {
        Value::List(items) => {
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
    evalled_values: &[(Position, Value)],
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    stack_frame.exprs_to_eval.push(expr_to_eval);

    env.stack.push(stack_frame);
}

#[derive(Debug)]
pub struct ErrorMessage(pub String);

/// Information about an error during evaluation.
#[derive(Debug)]
struct ErrorInfo {
    error_position: Position,
    message: ErrorMessage,
    /// Values that were popped from the stack frame to evaluate the
    /// current subexpression. We will need to restore these in order
    /// to halt in a state where the user can retry.
    restore_values: Vec<(Position, Value)>,
}

fn eval_if(
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

    match &condition_value.1 {
        Value::Boolean(b) => {
            if *b {
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
                        stack_frame
                            .evalled_values
                            .push((position.clone(), Value::Void));
                    }
                }
            }
        }
        v => {
            return Err(ErrorInfo {
                message: ErrorMessage(format!(
                    "Expected a boolean when evaluating `if`, but got: {}",
                    v
                )),
                restore_values: vec![condition_value],
                error_position: bool_position.clone(),
            });
        }
    }

    Ok(())
}

fn eval_while(
    stack_frame: &mut StackFrame,
    condition_pos: &Position,
    expr: Expression,
    body: &Block,
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    match &condition_value.1 {
        Value::Boolean(b) => {
            if *b {
                // Start loop evaluation again.
                stack_frame.exprs_to_eval.push((false, expr.clone()));

                // Evaluate the body.
                stack_frame
                    .exprs_to_eval
                    .push((false, Expression(expr.0, Expression_::Block(body.clone()))))
            } else {
                // TODO: It's weird using the position of the
                // condition when there's no else.
                stack_frame
                    .evalled_values
                    .push((condition_pos.clone(), Value::Void));
            }
        }
        v => {
            return Err(ErrorInfo {
                message: ErrorMessage(format!(
                    "Expected a boolean when evaluating `while`, but got: {}",
                    v
                )),
                restore_values: vec![condition_value],
                error_position: condition_pos.clone(),
            });
        }
    }

    Ok(())
}

fn eval_assign(stack_frame: &mut StackFrame, variable: &Symbol) -> Result<(), ErrorInfo> {
    let var_name = &variable.name;
    if !stack_frame.bindings.has(var_name) {
        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "{} is not currently bound. Try `let {} = something`.",
                var_name.0, var_name.0
            )),
            restore_values: vec![],
            error_position: variable.pos.clone(),
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .set_existing(var_name, expr_value.1.clone());
    stack_frame.evalled_values.push(expr_value);

    Ok(())
}

fn eval_let(stack_frame: &mut StackFrame, variable: &Symbol) -> Result<(), ErrorInfo> {
    let var_name = &variable.name;
    if stack_frame.bindings.has(var_name) {
        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "{} is already bound. Try `{} = something` instead.",
                var_name.0, var_name.0
            )),
            restore_values: vec![],
            error_position: variable.pos.clone(),
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame.bindings.add_new(var_name, expr_value.1.clone());
    stack_frame.evalled_values.push(expr_value);
    Ok(())
}

fn eval_boolean_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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

        let lhs_bool = match lhs_value.1 {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: ErrorMessage(format!("Expected a bool, but got: {}", lhs_value.1)),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_value.0,
                });
            }
        };
        let rhs_bool = match rhs_value.1 {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: ErrorMessage(format!("Expected a bool, but got: {}", rhs_value.1)),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_value.0,
                });
            }
        };

        match op {
            BinaryOperatorKind::And => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_bool && rhs_bool)));
            }
            BinaryOperatorKind::Or => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_bool || rhs_bool)));
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn eval_equality_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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
                .push((position.clone(), Value::Boolean(lhs_value.1 == rhs_value.1)));
        }
        BinaryOperatorKind::NotEqual => {
            stack_frame
                .evalled_values
                .push((position.clone(), Value::Boolean(lhs_value.1 != rhs_value.1)));
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn eval_integer_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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

        let lhs_num = match lhs_value.1 {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: ErrorMessage(format!("Expected an integer, but got: {}", lhs_value.1)),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_value.0,
                });
            }
        };
        let rhs_num = match rhs_value.1 {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: ErrorMessage(format!("Expected an integer, but got: {}", rhs_value.1)),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_value.0,
                });
            }
        };

        match op {
            BinaryOperatorKind::Add => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_add(rhs_num)),
                ));
            }
            BinaryOperatorKind::Subtract => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_sub(rhs_num)),
                ));
            }
            BinaryOperatorKind::Multiply => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_mul(rhs_num)),
                ));
            }
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Tried to divide {} by zero.", rhs_value.1)),
                        restore_values: vec![lhs_value, rhs_value.clone()],
                        error_position: rhs_value.0,
                    });
                }

                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Integer(lhs_num / rhs_num)));
            }
            BinaryOperatorKind::LessThan => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num < rhs_num)));
            }
            BinaryOperatorKind::GreaterThan => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num > rhs_num)));
            }
            BinaryOperatorKind::LessThanOrEqual => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num <= rhs_num)));
            }
            BinaryOperatorKind::GreaterThanOrEqual => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num >= rhs_num)));
            }
            _ => {
                unreachable!()
            }
        }
    }
    Ok(())
}

fn check_arity(
    fun_name: &str,
    receiver_value: &(Position, Value),
    expected: usize,
    arg_values: &[(Position, Value)],
) -> Result<(), ErrorInfo> {
    if arg_values.len() != expected {
        let mut saved_values = vec![receiver_value.clone()];
        for value in arg_values.iter().rev() {
            saved_values.push(value.clone());
        }

        return Err(ErrorInfo {
            message: ErrorMessage(format!(
                "Function {} requires {} argument{}, but got: {}",
                fun_name,
                expected,
                if expected == 1 { "" } else { "s" },
                arg_values.len()
            )),
            restore_values: saved_values,
            error_position: receiver_value.0.clone(),
        });
    }

    Ok(())
}

fn check_type(value: &Value, expected: &TypeName) -> Result<(), ErrorMessage> {
    let actual_type = type_representation(value);

    if actual_type != *expected {
        // TODO: Print the value as well as its type.
        return Err(ErrorMessage(format!(
            "Expected a {}, but got a {}",
            expected.0, actual_type.0
        )));
    }

    Ok(())
}

fn eval_builtin_call(
    kind: BuiltinFunctionKind,
    receiver_value: (Position, Value),
    arg_values: &[(Position, Value)],
    stack_frame: &mut StackFrame,
    position: &Position,
    session: &Session,
) -> Result<(), ErrorInfo> {
    match kind {
        BuiltinFunctionKind::Error => {
            check_arity("error", &receiver_value, 1, arg_values)?;

            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            match &arg_values[0].1 {
                Value::String(msg) => {
                    return Err(ErrorInfo {
                        message: ErrorMessage(msg.clone()),
                        restore_values: saved_values,
                        error_position: position.clone(),
                    });
                }
                v => {
                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
        }
        BuiltinFunctionKind::Print => {
            check_arity("print", &receiver_value, 1, arg_values)?;

            match &arg_values[0].1 {
                Value::String(s) => {
                    if session.has_attached_stdout {
                        print!("{}", s);
                    } else {
                        let response = Response {
                            kind: ResponseKind::Printed,
                            value: Ok(s.clone()),
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
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
            stack_frame
                .evalled_values
                .push((position.clone(), Value::Void));
        }
        BuiltinFunctionKind::Println => {
            check_arity("println", &receiver_value, 1, arg_values)?;

            match &arg_values[0].1 {
                Value::String(s) => {
                    if session.has_attached_stdout {
                        println!("{}", s);
                    } else {
                        let response = Response {
                            kind: ResponseKind::Printed,
                            value: Ok(format!("{}\n", s)),
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
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
            stack_frame
                .evalled_values
                .push((position.clone(), Value::Void));
        }
        BuiltinFunctionKind::DebugPrint => {
            check_arity("dbg", &receiver_value, 1, arg_values)?;

            // TODO: define a proper pretty-printer for values
            // rather than using Rust's Debug.
            let value = &arg_values[0].1;
            if session.has_attached_stdout {
                println!("{:?}", value);
            } else {
                let response = Response {
                    kind: ResponseKind::Printed,
                    value: Ok(format!("{:?}\n", value)),
                };
                let serialized = serde_json::to_string(&response).unwrap();
                println!("{}", serialized);
            }

            stack_frame
                .evalled_values
                .push((position.clone(), Value::Void));
        }
        BuiltinFunctionKind::Shell => {
            check_arity("shell", &receiver_value, 2, arg_values)?;

            match &arg_values[0].1 {
                Value::String(s) => {
                    match as_string_list(&arg_values[1].1) {
                        Ok(items) => {
                            let mut command = std::process::Command::new(s);
                            for item in items {
                                command.arg(item);
                            }

                            // TODO: define a result type in garden to report errors to the user.
                            let output = command.output().expect("failed to execute process");

                            let mut s = String::new();
                            // TODO: complain if output is not UTF-8.
                            s.write_str(&String::from_utf8_lossy(&output.stdout))
                                .unwrap();
                            s.write_str(&String::from_utf8_lossy(&output.stderr))
                                .unwrap();

                            stack_frame
                                .evalled_values
                                .push((position.clone(), Value::String(s)));
                        }
                        Err(v) => {
                            let mut saved_values = vec![];
                            for value in arg_values.iter().rev() {
                                saved_values.push(value.clone());
                            }
                            saved_values.push(receiver_value.clone());

                            return Err(ErrorInfo {
                                message: ErrorMessage(format!("Expected a list, but got: {}", v)),
                                restore_values: saved_values,
                                error_position: arg_values[0].0.clone(),
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
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
        }
        BuiltinFunctionKind::StringRepr => {
            check_arity("string_repr", &receiver_value, 1, arg_values)?;

            stack_frame.evalled_values.push((
                position.clone(),
                Value::String(format!("{}", arg_values[0].1)),
            ));
        }
        BuiltinFunctionKind::PathExists => {
            check_arity("path_exists", &receiver_value, 1, arg_values)?;

            // TODO: define a separate path type in Garden.
            let path_s = match &arg_values[0].1 {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            };

            let path = PathBuf::from(path_s);
            stack_frame
                .evalled_values
                .push((position.clone(), Value::Boolean(path.exists())));
        }
        BuiltinFunctionKind::WorkingDirectory => {
            check_arity("working_directory", &receiver_value, 0, arg_values)?;

            // TODO: when we have a userland result type, use that.
            let path = std::env::current_dir().unwrap_or_default();

            stack_frame
                .evalled_values
                .push((position.clone(), Value::String(path.display().to_string())));
        }
    }

    Ok(())
}

/// Evaluate a function call.
///
/// If we're calling a userland function, return the new stackframe to
/// evaluate next.
fn eval_call(
    stack_frame: &mut StackFrame,
    position: &Position,
    args: &[Expression],
    session: &Session,
) -> Result<Option<StackFrame>, ErrorInfo> {
    let mut arg_values = vec![];
    for _ in 0..args.len() {
        arg_values.push(
            stack_frame
                .evalled_values
                .pop()
                .expect("Popped an empty value for stack for call arguments"),
        );
    }
    let receiver_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for call receiver");

    match &receiver_value.1 {
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
                    error_position: receiver_value.0,
                });
            }

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in fun_info.body.exprs.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
                let param_name = &param.symbol.name;
                fun_bindings.insert(param_name.clone(), value.1.clone());
            }

            bindings.push(BlockBindings(Rc::new(RefCell::new(fun_bindings))));

            return Ok(Some(StackFrame {
                caller_pos: Some(position.clone()),
                bindings: Bindings(bindings),
                exprs_to_eval: fun_subexprs,
                // TODO: find a better position for the void value,
                // perhaps the position of the curly brace function
                // body.
                evalled_values: vec![(receiver_value.0, Value::Void)],
                enclosing_fun: Some(fun_info.clone()),
                enclosing_name: SymbolName("(closure)".to_string()),
                src: fun_info.src_string.clone(),
            }));
        }
        Value::Fun(name, fi @ FunInfo { params, body, .. }) => {
            // Calling a user-defined function.

            check_arity(&name.name.0, &receiver_value, params.len(), &arg_values)?;

            check_param_types(&receiver_value, params, &arg_values)?;

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in body.exprs.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                let param_name = &param.symbol.name;
                fun_bindings.insert(param_name.clone(), value.1.clone());
            }

            return Ok(Some(StackFrame {
                enclosing_fun: Some(fi.clone()),
                src: fi.src_string.clone(),
                caller_pos: Some(receiver_value.0.clone()),
                enclosing_name: name.name.clone(),
                bindings: Bindings::new_with(fun_bindings),
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![(name.pos.clone(), Value::Void)],
            }));
        }
        Value::BuiltinFunction(kind) => eval_builtin_call(
            *kind,
            receiver_value,
            &arg_values,
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
                error_position: receiver_value.0,
                message: ErrorMessage(format!("Expected a function, but got: {}", v)),
                restore_values: saved_values,
            });
        }
    }

    Ok(None)
}

fn check_param_types(
    receiver_value: &(Position, Value),
    params: &[SymbolWithType],
    arg_values: &[(Position, Value)],
) -> Result<(), ErrorInfo> {
    for (param, arg_value) in params.iter().zip(arg_values) {
        if let Some(param_ty) = &param.type_ {
            if let Err(msg) = check_type(&arg_value.1, param_ty) {
                let mut saved_values = vec![];
                saved_values.push(receiver_value.clone());
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    error_position: arg_value.0.clone(),
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
    position: &Position,
    meth_name: &Symbol,
    args: &[Expression],
) -> Result<Option<StackFrame>, ErrorInfo> {
    let mut arg_values = vec![];
    for _ in 0..args.len() {
        arg_values.push(
            stack_frame
                .evalled_values
                .pop()
                .expect("Popped an empty value for stack for method call arguments."),
        );
    }
    let receiver_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for method call receiver.");

    let receiver_type_name = type_representation(&receiver_value.1);
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
                        meth_name.name.0, receiver_type_name.0
                    )),
                    restore_values: saved_values,
                    error_position: meth_name.pos.clone(),
                });
            }
        }
        None => {
            let mut saved_values = vec![receiver_value.clone()];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }

            return Err(ErrorInfo {
                message: ErrorMessage(format!("No methods defined on `{}`.", receiver_type_name.0)),
                restore_values: saved_values,
                error_position: meth_name.pos.clone(),
            });
        }
    };

    let fun_info = match &receiver_method.kind {
        MethodKind::BuiltinMethod(kind) => {
            eval_builtin_method_call(*kind, receiver_value, arg_values, stack_frame, position)?;
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
        &meth_name.name.0,
        &receiver_value,
        fun_info.params.len(),
        &arg_values,
    )?;

    // TODO: check for duplicate parameter names.
    // TODO: parameter names must not clash with the receiver name.
    let mut fun_bindings = HashMap::new();
    for (param, value) in fun_info.params.iter().zip(arg_values.iter()) {
        let param_name = &param.symbol.name;
        fun_bindings.insert(param_name.clone(), value.1.clone());
    }
    fun_bindings.insert(receiver_method.receiver_name.clone(), receiver_value.1);

    Ok(Some(StackFrame {
        enclosing_fun: Some(fun_info.clone()),
        enclosing_name: SymbolName(format!("{}::{}", receiver_type_name.0, meth_name.name.0)),
        src: fun_info.src_string.clone(),
        caller_pos: Some(receiver_value.0.clone()),
        bindings: Bindings::new_with(fun_bindings),
        exprs_to_eval: method_subexprs,
        // TODO: find a better position for the void value,
        // perhaps the position of the curly brace function
        // body.
        evalled_values: vec![(receiver_value.0, Value::Void)],
    }))
}

fn eval_builtin_method_call(
    kind: BuiltinMethodKind,
    receiver_value: (Position, Value),
    arg_values: Vec<(Position, Value)>,
    stack_frame: &mut StackFrame,
    position: &Position,
) -> Result<(), ErrorInfo> {
    match kind {
        BuiltinMethodKind::ListAppend => {
            check_arity("List::append", &receiver_value, 1, &arg_values)?;

            match &receiver_value.1 {
                Value::List(items) => {
                    let mut new_items = items.clone();
                    new_items.push(arg_values[0].1.clone());
                    stack_frame
                        .evalled_values
                        .push((position.clone(), Value::List(new_items)));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a list, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: receiver_value.0.clone(),
                    });
                }
            }
        }
        BuiltinMethodKind::ListGet => {
            check_arity("List::get", &receiver_value, 1, &arg_values)?;

            match (&receiver_value.1, &arg_values[0].1) {
                (Value::List(items), Value::Integer(i)) => {
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
                            error_position: arg_values[0].0.clone(),
                        });
                    } else {
                        *i as usize
                    };

                    stack_frame
                        .evalled_values
                        .push((position.clone(), items[index].clone()));
                }
                (v, Value::Integer(_)) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a list, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
                (_, v) => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected an integer, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[1].0.clone(),
                    });
                }
            }
        }
        BuiltinMethodKind::ListLen => {
            check_arity("List::len", &receiver_value, 0, &arg_values)?;

            match &receiver_value.1 {
                Value::List(items) => {
                    stack_frame
                        .evalled_values
                        .push((position.clone(), Value::Integer(items.len() as i64)));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a list, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
        }
        BuiltinMethodKind::StringConcat => {
            check_arity("String::concat", &receiver_value, 1, &arg_values)?;

            let mut arg1 = match &receiver_value.1 {
                Value::String(s) => s.clone(),
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            };
            let arg2 = match &arg_values[0].1 {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[1].0.clone(),
                    });
                }
            };

            arg1.push_str(arg2);
            stack_frame
                .evalled_values
                .push((position.clone(), Value::String(arg1)));
        }
        BuiltinMethodKind::StringLen => {
            check_arity("String::len", &receiver_value, 0, &arg_values)?;

            match &receiver_value.1 {
                Value::String(s) => {
                    stack_frame
                        .evalled_values
                        .push((position.clone(), Value::Integer(s.chars().count() as i64)));
                }
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            }
        }
        BuiltinMethodKind::StringSubstring => {
            check_arity("String::substring", &receiver_value, 2, &arg_values)?;

            let s_arg = match &receiver_value.1 {
                Value::String(s) => s,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected a string, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[0].0.clone(),
                    });
                }
            };
            let from_arg = match &arg_values[0].1 {
                Value::Integer(i) => i,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected an integer, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[1].0.clone(),
                    });
                }
            };
            let to_arg = match &arg_values[1].1 {
                Value::Integer(i) => i,
                v => {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: ErrorMessage(format!("Expected an integer, but got: {}", v)),
                        restore_values: saved_values,
                        error_position: arg_values[2].0.clone(),
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
                        error_position: arg_values[1].0.clone(),
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
                        error_position: arg_values[1].0.clone(),
                    });
            }

            stack_frame.evalled_values.push((
                position.clone(),
                Value::String(
                    s_arg
                        .chars()
                        .skip(*from_arg as usize)
                        .take((to_arg - from_arg) as usize)
                        .collect(),
                ),
            ));
        }
    }

    Ok(())
}

pub fn eval_env(env: &mut Env, session: &mut Session) -> Result<Value, EvalError> {
    loop {
        if let Some(mut stack_frame) = env.stack.pop() {
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

                if env.trace_exprs {
                    println!("{:?} {}", expr_, done_children);
                }
                match expr_ {
                    Expression_::If(condition, ref then_body, ref else_body) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position: position,
                            }) = eval_if(
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
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::Integer(i)));
                    }
                    Expression_::BoolLiteral(b) => {
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::Boolean(b)));
                    }
                    Expression_::StringLiteral(s) => {
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::String(s)));
                    }
                    Expression_::ListLiteral(items) => {
                        if done_children {
                            let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                            for _ in 0..items.len() {
                                list_values.push(stack_frame.evalled_values.pop().expect(
                                    "Value stack should have sufficient items for the list literal",
                                ).1);
                            }

                            stack_frame
                                .evalled_values
                                .push((expr_position, Value::List(list_values)));
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));

                            for item in items.iter() {
                                stack_frame.exprs_to_eval.push((false, item.clone()));
                            }
                        }
                    }
                    Expression_::Variable(name) => {
                        if let Some(value) = get_var(&name.name, &stack_frame, env) {
                            stack_frame.evalled_values.push((expr_position, value));
                        } else {
                            let suggestion = match most_similar_var(&name.name, &stack_frame, env) {
                                Some(closest_name) => format!(" Did you mean {}?", closest_name.0),
                                None => "".to_owned(),
                            };

                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &[],
                            );

                            return Err(EvalError::ResumableError(
                                name.pos.clone(),
                                ErrorMessage(format!(
                                    "Undefined variable: {}.{}",
                                    name.name.0, suggestion
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
                            }) = eval_integer_binop(&mut stack_frame, &expr_position, op)
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
                        op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
                        rhs,
                    ) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position: position,
                            }) = eval_equality_binop(&mut stack_frame, &expr_position, op)
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
                            }) = eval_boolean_binop(&mut stack_frame, &expr_position, op)
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
                        stack_frame.evalled_values.push((
                            expr_position,
                            Value::Closure(stack_frame.bindings.0.clone(), fun_info),
                        ));
                    }
                    Expression_::Call(receiver, ref args) => {
                        if done_children {
                            match eval_call(&mut stack_frame, &expr_position, args, session) {
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
                                &expr_position,
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
                } else {
                    // Check that the value matches the return type.
                    let ret_val_and_pos = stack_frame
                        .evalled_values
                        .pop()
                        .expect("Should have a value");
                    let (return_value_pos, return_value) = ret_val_and_pos.clone();

                    if let Some(ref fun) = stack_frame.enclosing_fun {
                        if let Some(return_type) = &fun.return_type {
                            if let Err(msg) = check_type(&return_value, return_type) {
                                stack_frame.evalled_values.push(ret_val_and_pos.clone());
                                env.stack.push(stack_frame);

                                return Err(EvalError::ResumableError(return_value_pos, msg));
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
                        .push(ret_val_and_pos);
                }
            } else {
                // Keep going on this stack frame.
                env.stack.push(stack_frame);
            }
        } else {
            unreachable!();
        }
    }

    Ok(env
        .stack
        .last_mut()
        .expect("toplevel stack frame should exist")
        .evalled_values
        .pop()
        .expect("Should have a value from the last expression")
        .1)
}

pub fn eval_exprs(
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

    use crate::ast::Position;
    use crate::parse::{parse_defs_from_str, parse_exprs_from_str};

    use super::*;

    fn eval_exprs(exprs: &[Expression], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut session = Session {
            history: String::new(),
            interrupted: &interrupted,
            has_attached_stdout: false,
        };

        super::eval_exprs(exprs, env, &mut session)
    }

    #[test]
    fn test_eval_bool_literal() {
        let exprs = vec![Expression(
            Position {
                start_offset: 0,
                end_offset: 4,
                line_number: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::BoolLiteral(true),
        )];

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_equality() {
        let exprs = parse_exprs_from_str("\"a\" == \"b\";").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
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
                    pos: Position {
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
                    Expression_::BoolLiteral(true),
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
                pos: Position {
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
        let exprs = parse_exprs_from_str("true; false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
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
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_less_than_or_equal() {
        let exprs = parse_exprs_from_str("3 <= 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_list_literal() {
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4];").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(vec![Value::Integer(3), Value::Integer(12)])
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
        let exprs = parse_exprs_from_str("let foo = true; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let exprs = parse_exprs_from_str("let foo = true; let foo = false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env);
        assert!(value.is_err());
    }

    #[test]
    fn test_eval_if() {
        let exprs = parse_exprs_from_str("let foo = if (true) { 1; } else { 2; }; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let exprs = parse_exprs_from_str("if (true) { let x = 1; } x;").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_list_append() {
        let exprs = parse_exprs_from_str("[1, 2].append(3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])
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

        let defs = parse_defs_from_str("fun f() { true; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
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

        let defs = parse_defs_from_str("fun (self: String) f() { true; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_method_call_bad_airty() {
        let mut env = Env::default();

        let defs = parse_defs_from_str("fun (self: String) f() { true; }").unwrap();
        eval_defs(&defs, &mut env);

        let exprs = parse_exprs_from_str("\"\".f(123);").unwrap();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_while() {
        let exprs = parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Void);
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

        let exprs = parse_exprs_from_str("f(true);").unwrap();
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
}
