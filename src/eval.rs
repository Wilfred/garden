use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{collections::HashMap, fmt::Display};

use crate::json_session::{Response, ResponseKind};
use crate::parse::{BinaryOperatorKind, Position, Statement};
use crate::parse::{
    Definition, Definition_, DefinitionsOrExpression, Expression, Expression_, Statement_,
    VariableName,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(
        Option<String>,
        VariableName,
        Vec<VariableName>,
        Vec<Expression>,
    ),
    BuiltinFunction(BuiltinFunctionKind),
    String(String),
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinFunctionKind {
    Print,
    IntToString,
}

pub fn builtin_fun_doc(kind: &BuiltinFunctionKind) -> &str {
    match kind {
        BuiltinFunctionKind::Print => {
            "Write a string to stdout.

```
print(\"hello world\");
```"
        }
        BuiltinFunctionKind::IntToString => {
            "Convert an integer to its decimal representation as a string.

```
int_to_string(123); // \"123\"
```"
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(_, name, _, _) => write!(f, "(function: {})", name.0),
            Value::BuiltinFunction(kind) => {
                let name = match kind {
                    BuiltinFunctionKind::Print => "print",
                    BuiltinFunctionKind::IntToString => "int_to_string",
                };
                write!(f, "(function: {})", name)
            }
            Value::Void => write!(f, "void"),
            Value::String(s) => {
                write!(f, "\"")?;

                // Escape inner double quotes and backslashes.
                for c in s.chars() {
                    match c {
                        '"' => write!(f, "\\\"")?,
                        '\\' => write!(f, "\\\\")?,
                        _ => write!(f, "{}", c)?,
                    }
                }

                write!(f, "\"")
            }
        }
    }
}

#[derive(Debug)]
pub struct StackFrame {
    pub fun_name: VariableName,
    pub bindings: HashMap<VariableName, Value>,
    pub exprs_to_eval: Vec<(bool, Expression)>,
    pub evalled_values: Vec<Value>,
}

#[derive(Debug)]
pub struct Env {
    pub trace_exprs: bool,
    pub file_scope: HashMap<VariableName, Value>,
    pub stack: Vec<StackFrame>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope = HashMap::new();
        file_scope.insert(
            VariableName("print".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::Print),
        );
        file_scope.insert(
            VariableName("int_to_string".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::IntToString),
        );

        Self {
            trace_exprs: false,
            file_scope,
            stack: vec![StackFrame {
                fun_name: VariableName("toplevel".into()),
                bindings: HashMap::new(),
                exprs_to_eval: vec![],
                evalled_values: vec![Value::Void],
            }],
        }
    }
}

impl Env {
    pub fn pop_to_toplevel(&mut self) {
        self.stack.truncate(1);
        self.stack[0].evalled_values.truncate(1);
    }

    pub fn set_with_file_scope(&mut self, name: &VariableName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }
}

fn get_var(name: &VariableName, stack_frame: &StackFrame, env: &Env) -> Option<Value> {
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ErrorKind {
    /// An inappropriate value for the current expression, such as a
    /// type error or divide by zero.
    BadValue,
    /// A malformed expression, such as an arity error or assigning to
    /// an undefined variable.
    MalformedExpression,
}

#[derive(Debug)]
pub enum EvalError {
    Interrupted,
    ResumableError(String),
    Stop(Option<ErrorKind>),
}

// TODO: result is really Result<Value, ErrorWithSuspendedEnv>
pub fn eval_def_or_exprs(
    items: &DefinitionsOrExpression,
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    match items {
        DefinitionsOrExpression::Defs(defs) => {
            eval_defs(defs, env);
            Ok(Value::Void)
        }
        DefinitionsOrExpression::Expr(e) => {
            let stmts = vec![Statement(Position(0), Statement_::Expr(e.clone()))];
            eval_stmts(&stmts, env, session)
        }
    }
}

pub fn eval_defs(definitions: &[Definition], env: &mut Env) {
    for definition in definitions {
        match &definition.1 {
            Definition_::Fun(doc_comment, name, params, body) => {
                env.set_with_file_scope(
                    name,
                    Value::Fun(
                        doc_comment.clone(),
                        name.clone(),
                        params.clone(),
                        body.clone(),
                    ),
                );
            }
        }
    }
}

fn restore_stack_frame(
    env: &mut Env,
    mut stack_frame: StackFrame,
    expr_to_eval: (bool, Expression),
    evalled_values: &[Value],
    error_kind: Option<ErrorKind>,
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    let offset = expr_to_eval.1 .0;
    stack_frame.exprs_to_eval.push(expr_to_eval);
    stack_frame
        .exprs_to_eval
        .push((false, Expression(offset, Expression_::Stop(error_kind))));

    env.stack.push(stack_frame);
}

pub fn eval_env(env: &mut Env, session: &mut Session) -> Result<Value, EvalError> {
    loop {
        if let Some(mut stack_frame) = env.stack.pop() {
            if let Some((done_children, Expression(offset, expr_))) =
                stack_frame.exprs_to_eval.pop()
            {
                if session.interrupted.load(Ordering::SeqCst) {
                    session.interrupted.store(false, Ordering::SeqCst);
                    restore_stack_frame(
                        env,
                        stack_frame,
                        (done_children, Expression(offset, expr_)),
                        &[],
                        None,
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
                            let condition_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for if condition");
                            match condition_value.clone() {
                                Value::Boolean(b) => {
                                    if b {
                                        for expr in then_body.iter().rev() {
                                            stack_frame.exprs_to_eval.push((false, expr.clone()));
                                        }
                                    } else {
                                        for expr in else_body.iter().rev() {
                                            stack_frame.exprs_to_eval.push((false, expr.clone()));
                                        }
                                    }
                                }
                                v => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[condition_value],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected a boolean when evaluating `if`, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *condition.clone()));
                        }
                    }
                    Expression_::While(condition, ref body) => {
                        if done_children {
                            let condition_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for if condition");
                            match condition_value.clone() {
                                Value::Boolean(b) => {
                                    if b {
                                        // Start loop evaluation again.
                                        stack_frame
                                            .exprs_to_eval
                                            .push((false, Expression(offset, expr_copy)));

                                        // Evaluate the body.
                                        for expr in body.iter().rev() {
                                            stack_frame.exprs_to_eval.push((false, expr.clone()));
                                        }
                                    } else {
                                        stack_frame.evalled_values.push(Value::Void);
                                    }
                                }
                                v => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[condition_value],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected a boolean when evaluating `while`, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
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
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Assign(variable, expr) => {
                        if done_children {
                            if !stack_frame.bindings.contains_key(&variable) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &[],
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(format!(
                                    "{} is not currently bound. Try `let {} = something`.",
                                    variable.0, variable.0
                                )));
                            }

                            let expr_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            stack_frame.bindings.insert(variable, expr_value.clone());
                            stack_frame.evalled_values.push(expr_value);
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Let(variable, expr) => {
                        if done_children {
                            if stack_frame.bindings.contains_key(&variable) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &[],
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(format!(
                                    "{} is already bound. Try `{} = something` instead.",
                                    variable.0, variable.0
                                )));
                            }

                            let expr_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            stack_frame.bindings.insert(variable, expr_value.clone());
                            stack_frame.evalled_values.push(expr_value);
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Stop(e) => {
                        stack_frame
                            .exprs_to_eval
                            .push((false, Expression(offset, Expression_::Stop(e))));
                        env.stack.push(stack_frame);
                        return Err(EvalError::Stop(e));
                    }
                    Expression_::IntLiteral(i) => {
                        stack_frame.evalled_values.push(Value::Integer(i));
                    }
                    Expression_::BoolLiteral(b) => {
                        stack_frame.evalled_values.push(Value::Boolean(b));
                    }
                    Expression_::StringLiteral(s) => {
                        stack_frame.evalled_values.push(Value::String(s));
                    }
                    Expression_::Variable(name) => {
                        if let Some(value) = get_var(&name, &stack_frame, &env) {
                            stack_frame.evalled_values.push(value);
                        } else {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(offset, expr_copy)),
                                &[],
                                Some(ErrorKind::MalformedExpression),
                            );
                            return Err(EvalError::ResumableError(format!(
                                "Undefined variable: {}. What value would you like to use instead?",
                                name.0
                            )));
                        }
                    }
                    Expression_::BinaryOperator(
                        lhs,
                        op @ (BinaryOperatorKind::Add
                        | BinaryOperatorKind::Subtract
                        | BinaryOperatorKind::Multiply
                        | BinaryOperatorKind::Divide
                        | BinaryOperatorKind::LessThan
                        | BinaryOperatorKind::GreaterThan),
                        rhs,
                    ) => {
                        if done_children {
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
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value.clone(), rhs_value],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected an integer, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_num = match rhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value, rhs_value.clone()],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected an integer, but got: {}",
                                        rhs_value
                                    )));
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
                                        restore_stack_frame(
                                            env,
                                            stack_frame,
                                            (done_children, Expression(offset, expr_copy)),
                                            &[lhs_value, rhs_value.clone()],
                                            Some(ErrorKind::BadValue),
                                        );
                                        return Err(EvalError::ResumableError(format!(
                                            "Tried to divide {} by zero.",
                                            rhs_value
                                        )));
                                    }

                                    stack_frame
                                        .evalled_values
                                        .push(Value::Integer(lhs_num / rhs_num));
                                }
                                BinaryOperatorKind::LessThan => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_num < rhs_num));
                                }
                                BinaryOperatorKind::GreaterThan => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_num > rhs_num));
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
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
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value.clone(), rhs_value],
                                        Some(ErrorKind::BadValue),
                                    );

                                    return Err(EvalError::ResumableError(format!(
                                        "Expected an integer, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_num = match rhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value, rhs_value.clone()],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        // TODO: use the term 'int' or 'integer' in error messages?
                                        // int: reflects code
                                        // integer: conventional maths
                                        "Expected an integer, but got: {}",
                                        rhs_value
                                    )));
                                }
                            };

                            match op {
                                BinaryOperatorKind::Equal => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_num == rhs_num));
                                }
                                BinaryOperatorKind::NotEqual => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_num != rhs_num));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
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
                            let rhs_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for RHS of binary operator");
                            let lhs_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for LHS of binary operator");

                            let lhs_bool = match lhs_value {
                                Value::Boolean(b) => b,
                                _ => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value.clone(), rhs_value],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected a boolean, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_bool = match rhs_value {
                                Value::Boolean(b) => b,
                                _ => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &[lhs_value, rhs_value.clone()],
                                        Some(ErrorKind::BadValue),
                                    );
                                    return Err(EvalError::ResumableError(format!(
                                        "Expected a bool, but got: {}",
                                        rhs_value
                                    )));
                                }
                            };

                            match op {
                                BinaryOperatorKind::And => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_bool && rhs_bool));
                                }
                                BinaryOperatorKind::Or => {
                                    stack_frame
                                        .evalled_values
                                        .push(Value::Boolean(lhs_bool || rhs_bool));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            // TODO: do short-circuit evaluation of && and ||.
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                            stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                        }
                    }
                    Expression_::Call(receiver, ref args) => {
                        if done_children {
                            let mut arg_values = vec![];
                            for _ in 0..args.len() {
                                arg_values.push(
                                    stack_frame.evalled_values.pop().expect(
                                        "Popped an empty value for stack for call arguments",
                                    ),
                                );
                            }
                            let receiver_value = stack_frame
                                .evalled_values
                                .pop()
                                .expect("Popped an empty value stack for call receiver");

                            match &receiver_value {
                                Value::Fun(_, name, params, body) => {
                                    if params.len() != arg_values.len() {
                                        let mut saved_values = vec![receiver_value.clone()];
                                        for value in arg_values.iter().rev() {
                                            saved_values.push(value.clone());
                                        }
                                        restore_stack_frame(
                                            env,
                                            stack_frame,
                                            (done_children, Expression(offset, expr_copy)),
                                            &saved_values,
                                            Some(ErrorKind::MalformedExpression),
                                        );

                                        return Err(EvalError::ResumableError(format!(
                                            "Function {} expects {} arguments, but got {}",
                                            name.0.clone(),
                                            params.len(),
                                            arg_values.len()
                                        )));
                                    }

                                    env.stack.push(stack_frame);

                                    let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
                                    for expr in body.iter().rev() {
                                        fun_subexprs.push((false, expr.clone()));
                                    }

                                    let mut fun_bindings = HashMap::new();
                                    for (param, value) in params.iter().zip(arg_values.iter()) {
                                        fun_bindings.insert(param.clone(), value.clone());
                                    }

                                    env.stack.push(StackFrame {
                                        fun_name: name.clone(),
                                        bindings: fun_bindings,
                                        exprs_to_eval: fun_subexprs,
                                        evalled_values: vec![Value::Void],
                                    });

                                    continue;
                                }
                                Value::BuiltinFunction(kind) => match kind {
                                    BuiltinFunctionKind::Print => {
                                        if args.len() != 1 {
                                            let mut saved_values = vec![receiver_value.clone()];
                                            for value in arg_values.iter().rev() {
                                                saved_values.push(value.clone());
                                            }
                                            restore_stack_frame(
                                                env,
                                                stack_frame,
                                                (done_children, Expression(offset, expr_copy)),
                                                &saved_values,
                                                Some(ErrorKind::MalformedExpression),
                                            );

                                            return Err(EvalError::ResumableError(format!(
                                                "Function print requires 1 argument, but got: {}",
                                                args.len()
                                            )));
                                        }
                                        match &arg_values[0] {
                                            Value::String(s) => {
                                                if session.has_attached_stdout {
                                                    println!("{}", s);
                                                } else {
                                                    let response = Response {
                                                        kind: ResponseKind::Printed,
                                                        value: Ok(format!("{}\n", s)),
                                                    };
                                                    let serialized =
                                                        serde_json::to_string(&response).unwrap();
                                                    println!("{}", serialized);
                                                }
                                            }
                                            v => {
                                                let mut saved_values = vec![];
                                                for value in arg_values.iter().rev() {
                                                    saved_values.push(value.clone());
                                                }
                                                saved_values.push(receiver_value.clone());
                                                restore_stack_frame(
                                                    env,
                                                    stack_frame,
                                                    (done_children, Expression(offset, expr_copy)),
                                                    &saved_values,
                                                    Some(ErrorKind::BadValue),
                                                );

                                                return Err(EvalError::ResumableError(format!(
                                                    "Expected a string, but got: {}",
                                                    v
                                                )));
                                            }
                                        }
                                        stack_frame.evalled_values.push(Value::Void);
                                    }
                                    BuiltinFunctionKind::IntToString => {
                                        if args.len() != 1 {
                                            let mut saved_values = vec![];
                                            for value in arg_values.iter().rev() {
                                                saved_values.push(value.clone());
                                            }
                                            saved_values.push(receiver_value.clone());
                                            restore_stack_frame(
                                                env,
                                                stack_frame,
                                                (done_children, Expression(offset, expr_copy)),
                                                &saved_values,
                                                Some(ErrorKind::MalformedExpression),
                                            );

                                            return Err(EvalError::ResumableError(format!(
                                                "Function int_to_string requires 1 argument, but got: {}",
                                                args.len()
                                            )));
                                        }
                                        match &arg_values[0] {
                                            Value::Integer(i) => {
                                                stack_frame
                                                    .evalled_values
                                                    .push(Value::String(format!("{}", i)));
                                            }
                                            v => {
                                                let mut saved_values = vec![];
                                                for value in arg_values.iter().rev() {
                                                    saved_values.push(value.clone());
                                                }
                                                saved_values.push(receiver_value.clone());
                                                restore_stack_frame(
                                                    env,
                                                    stack_frame,
                                                    (done_children, Expression(offset, expr_copy)),
                                                    &saved_values,
                                                    Some(ErrorKind::BadValue),
                                                );

                                                return Err(EvalError::ResumableError(format!(
                                                    "Expected an integer, but got: {}",
                                                    v
                                                )));
                                            }
                                        }
                                    }
                                },
                                v => {
                                    let mut saved_values = vec![];
                                    for value in arg_values.iter().rev() {
                                        saved_values.push(value.clone());
                                    }
                                    saved_values.push(receiver_value.clone());
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &saved_values,
                                        Some(ErrorKind::BadValue),
                                    );

                                    return Err(EvalError::ResumableError(format!(
                                        "Expected a function, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));

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
                }
            }

            if stack_frame.exprs_to_eval.is_empty() {
                // No more statements in this stack frame.
                if env.stack.is_empty() {
                    // Don't pop the outer scope: that's for the top level environment.
                    env.stack.push(stack_frame);
                    break;
                } else {
                    // The final evaluation result of the function
                    // call should be used in the previous stack
                    // frame.
                    let result = stack_frame
                        .evalled_values
                        .pop()
                        .expect("Should have a value");
                    env.stack.last_mut().unwrap().evalled_values.push(result);
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
        .expect("Should have a value from the last expression"))
}

pub fn eval_stmts(
    stmts: &[Statement],
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    let mut stmts_to_eval = vec![];
    for stmt in stmts.iter().rev() {
        stmts_to_eval.push((false, stmt.clone()));
    }

    let top_stack = env.stack.last_mut().unwrap();
    // TODO: do this setup outside of this function.
    top_stack.stmts_to_eval = stmts_to_eval;

    eval_env(env, session)
}

// fn read_replacement(msg: &str) -> Result<Expression, String> {
//     println!("{}: {}", "Unexpected error".bright_red(), msg);
//     println!("What value should be used instead?\n");

//     let mut rl: Editor<()> = Editor::new().unwrap();
//     let input = rl
//         .readline(&prompt_symbol(1))
//         .expect("error: unable to read user input");

//     let tokens = lex(input.trim())?;
//     let mut token_ptr = &tokens[..];
//     parse_expression(&mut token_ptr)
// }

#[cfg(test)]
mod tests {
    use crate::parse::{parse_def_or_expr_from_str, parse_exprs_from_str};

    use super::*;

    fn eval_stmts(stmts: &[Statement], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut session = Session {
            history: String::new(),
            interrupted: &interrupted,
            has_attached_stdout: false,
        };

        super::eval_stmts(stmts, env, &mut session)
    }

    #[test]
    fn test_eval_bool_literal() {
        let stmts = vec![Statement(
            Position(0),
            Statement_::Expr(Expression(Position(0), Expression_::BoolLiteral(true))),
        )];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let stmts = vec![Statement(
            Position(0),
            Statement_::Let(
                VariableName("foo".into()),
                Expression(Position(0), Expression_::BoolLiteral(true)),
            ),
        )];
        eval_stmts(&stmts, &mut env).unwrap();

        let stmts = vec![Statement(
            Position(0),
            Statement_::Expr(Expression(
                Position(0),
                Expression_::Variable(VariableName("foo".into())),
            )),
        )];
        eval_stmts(&stmts, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_stmts() {
        let stmts = parse_exprs_from_str("true; false;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_add() {
        let stmts = parse_exprs_from_str("1 + 2;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_let() {
        let stmts = parse_exprs_from_str("let foo = true; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let stmts = parse_exprs_from_str("let foo = true; let foo = false;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env);
        assert!(value.is_err());
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_stmts(&[], &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_call() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str("fun f() { true; }").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f();").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str("fun f(x) {x; }").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f(123);").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str("fun f(x, y) { y; }").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f(1, 2);").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_while() {
        let stmts = parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_env_after_call() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str("fun id(x) { x; }").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("let i = 0; id(i); i;").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(0));
    }

    #[test]
    fn test_eval_return() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str("fun f() { return 1; 2; }").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f();").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let value = Value::String("foo \\ \" bar".into());
        assert_eq!(format!("{}", value), "\"foo \\\\ \\\" bar\"");
    }
}
