use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{collections::HashMap, fmt::Display};

use crate::commands::{run_command, Command, CommandError};
use crate::json_session::{Response, ResponseKind};
use crate::parse::{
    parse_def_or_expr_from_str, Definition, Definition_, DefinitionsOrExpression, Expression,
    Expression_, ParseError, Statement_, VariableName,
};
use crate::parse::{BinaryOperatorKind, Statement};
use crate::prompt::prompt_symbol;

use owo_colors::OwoColorize;
use rustyline::Editor;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(
        Option<String>,
        VariableName,
        Vec<VariableName>,
        Vec<Statement>,
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
            // TODO: escape inner double quotes.
            Value::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug)]
pub struct StackFrame {
    pub fun_name: VariableName,
    pub bindings: HashMap<VariableName, Value>,
}

#[derive(Debug)]
pub struct Env {
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
            file_scope,
            stack: vec![StackFrame {
                fun_name: VariableName("toplevel".into()),
                bindings: HashMap::new(),
            }],
        }
    }
}

impl Env {
    pub fn get(&self, name: &VariableName) -> Option<Value> {
        if let Some(stack_frame) = self.stack.last() {
            if let Some(value) = stack_frame.bindings.get(name) {
                return Some(value.clone());
            }
        }

        if let Some(value) = self.file_scope.get(name) {
            return Some(value.clone());
        }

        None
    }

    pub fn push_new_fun_scope(&mut self, fun_name: &VariableName) {
        self.stack.push(StackFrame {
            fun_name: fun_name.clone(),
            bindings: HashMap::new(),
        });
    }

    pub fn pop_fun_scope(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn set_with_file_scope(&mut self, name: &VariableName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }

    pub fn set_with_fun_scope(&mut self, name: &VariableName, value: Value) {
        let stack_frame = &mut self.stack.last_mut().unwrap();
        stack_frame.bindings.insert(name.clone(), value);
    }

    pub fn fun_scope_has_var(&self, name: &VariableName) -> bool {
        let stack_frame = self.stack.last().unwrap();
        stack_frame.bindings.contains_key(name)
    }
}

#[derive(Debug)]
pub struct Session<'a> {
    pub history: String,
    pub interrupted: &'a Arc<AtomicBool>,
    pub has_attached_stdout: bool,
}

#[derive(Debug)]
pub enum EvalError {
    UserError(String),
    Aborted,
}

fn error_prompt(message: &str, env: &mut Env, session: &Session) -> Result<Statement, EvalError> {
    println!("{}: {}", "Error".bright_red(), message);
    println!("What value would you like to use instead?\n");

    let mut rl: Editor<()> = Editor::new().unwrap();
    loop {
        match rl.readline(&prompt_symbol(1)) {
            Ok(input) => {
                match Command::from_string(&input) {
                    Some(cmd) => {
                        match run_command(&mut std::io::stdout(), &cmd, &env, session) {
                            Ok(()) => {
                                println!();
                                continue;
                            }
                            Err(CommandError::Abort) => {
                                // Pop to toplevel.
                                while env.stack.len() > 1 {
                                    env.pop_fun_scope();
                                }

                                return Err(EvalError::Aborted);
                            }
                        }
                    }
                    None => {}
                }

                let asts = parse_def_or_expr_from_str(&input).map_err(|e| match e {
                    ParseError::OtherError(e) | ParseError::Incomplete(e) => {
                        EvalError::UserError(e)
                    }
                })?;
                match asts {
                    DefinitionsOrExpression::Defs(defs) => {
                        return Err(EvalError::UserError(format!(
                            "Expected to read a single statement, got {} definitoins",
                            defs.len()
                        )));
                    }
                    DefinitionsOrExpression::Expr(e) => {
                        return Ok(Statement(e.0, Statement_::Expr(e)));
                    }
                }
            }
            Err(e) => {
                return Err(EvalError::UserError(format!("Input failed: {}", e)));
            }
        }
    }
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
            let stmts = vec![Statement(0, Statement_::Expr(e.clone()))];
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

pub fn eval_stmts(
    stmts: &[Statement],
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    let mut stmts_to_eval = vec![];
    for stmt in stmts.iter().rev() {
        stmts_to_eval.push((false, stmt.clone()));
    }

    let mut stmts_to_eval_per_fun: Vec<Vec<(bool, Statement)>> = vec![stmts_to_eval];
    let mut evalled_values: Vec<Value> = vec![Value::Void];

    loop {
        if let Some(stmts_to_eval) = stmts_to_eval_per_fun.last_mut() {
            if let Some((done_children, Statement(offset, stmt_))) = stmts_to_eval.pop() {
                if session.interrupted.load(Ordering::SeqCst) {
                    // TODO: prompt for what to do next.
                    println!("Got ctrl-c");
                    break;
                }

                let stmt_copy = stmt_.clone();
                match stmt_ {
                    Statement_::If(condition, ref then_body, ref else_body) => {
                        if done_children {
                            let condition_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for if condition");
                            match condition_value {
                                Value::Boolean(b) => {
                                    if b {
                                        for stmt in then_body.iter().rev() {
                                            stmts_to_eval.push((false, stmt.clone()));
                                        }
                                    } else {
                                        for stmt in else_body.iter().rev() {
                                            stmts_to_eval.push((false, stmt.clone()));
                                        }
                                    }
                                }
                                v => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected a boolean, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval.push((
                                false,
                                Statement(condition.0, Statement_::Expr(condition.clone())),
                            ));
                        }
                    }
                    Statement_::While(condition, ref body) => {
                        if done_children {
                            let condition_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for if condition");
                            match condition_value {
                                Value::Boolean(b) => {
                                    if b {
                                        // Start loop evaluation again.
                                        stmts_to_eval.push((false, Statement(offset, stmt_copy)));

                                        // Evaluate the body.
                                        for stmt in body.iter().rev() {
                                            stmts_to_eval.push((false, stmt.clone()));
                                        }
                                    } else {
                                        evalled_values.push(Value::Void);
                                    }
                                }
                                v => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected a boolean, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval.push((
                                false,
                                Statement(condition.0, Statement_::Expr(condition.clone())),
                            ));
                        }
                    }
                    Statement_::Let(variable, expr) => {
                        if done_children {
                            if env.fun_scope_has_var(&variable) {
                                return Err(EvalError::UserError(format!(
                                    "{} is already bound. Try `{} = something` instead.",
                                    variable.0, variable.0
                                )));
                            }

                            let expr_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            env.set_with_fun_scope(&variable, expr_value.clone());
                            evalled_values.push(expr_value);
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(expr.0, Statement_::Expr(expr.clone()))));
                        }
                    }
                    Statement_::Return(expr) => {
                        if done_children {
                            // No more statements to evaluate in this function.
                            stmts_to_eval.clear();
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(expr.0, Statement_::Expr(expr.clone()))));
                        }
                    }
                    Statement_::Assign(variable, expr) => {
                        if done_children {
                            if !env.fun_scope_has_var(&variable) {
                                return Err(EvalError::UserError(format!(
                                    "{} is not currently bound. Try `let {} = something`.",
                                    variable.0, variable.0
                                )));
                            }

                            let expr_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            env.set_with_fun_scope(&variable, expr_value.clone());
                            evalled_values.push(expr_value);
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(expr.0, Statement_::Expr(expr.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(_, Expression_::IntLiteral(i))) => {
                        evalled_values.push(Value::Integer(i));
                    }
                    Statement_::Expr(Expression(_, Expression_::BoolLiteral(b))) => {
                        evalled_values.push(Value::Boolean(b));
                    }
                    Statement_::Expr(Expression(_, Expression_::StringLiteral(s))) => {
                        evalled_values.push(Value::String(s));
                    }
                    Statement_::Expr(Expression(_, Expression_::Variable(name))) => {
                        if let Some(value) = env.get(&name) {
                            evalled_values.push(value);
                        } else {
                            if session.has_attached_stdout {
                                let stmt = error_prompt(
                                    &format!("Undefined variable: {}", name.0),
                                    env,
                                    &session,
                                )?;
                                stmts_to_eval.push((false, stmt));
                            } else {
                                // TODO: add equivalent to error_prompt in JSON sessions.
                                return Err(EvalError::UserError(format!(
                                    "Undefined variable: {}",
                                    name.0
                                )));
                            }
                        }
                    }
                    Statement_::Expr(Expression(
                        _,
                        Expression_::BinaryOperator(
                            lhs,
                            op @ (BinaryOperatorKind::Add
                            | BinaryOperatorKind::Subtract
                            | BinaryOperatorKind::Multiply
                            | BinaryOperatorKind::Divide
                            | BinaryOperatorKind::LessThan
                            | BinaryOperatorKind::GreaterThan),
                            rhs,
                        ),
                    )) => {
                        if done_children {
                            let rhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for RHS of binary operator");
                            let lhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for LHS of binary operator");

                            let lhs_num = match lhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected an integer, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_num = match rhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected an integer, but got: {}",
                                        rhs_value
                                    )));
                                }
                            };

                            match op {
                                BinaryOperatorKind::Add => {
                                    evalled_values
                                        .push(Value::Integer(lhs_num.wrapping_add(rhs_num)));
                                }
                                BinaryOperatorKind::Subtract => {
                                    evalled_values
                                        .push(Value::Integer(lhs_num.wrapping_sub(rhs_num)));
                                }
                                BinaryOperatorKind::Multiply => {
                                    evalled_values
                                        .push(Value::Integer(lhs_num.wrapping_mul(rhs_num)));
                                }
                                BinaryOperatorKind::Divide => {
                                    if rhs_num == 0 {
                                        return Err(EvalError::UserError(format!(
                                            "Tried to divide {} by zero.",
                                            rhs_value
                                        )));
                                    }

                                    evalled_values.push(Value::Integer(lhs_num / rhs_num));
                                }
                                BinaryOperatorKind::LessThan => {
                                    evalled_values.push(Value::Boolean(lhs_num < rhs_num));
                                }
                                BinaryOperatorKind::GreaterThan => {
                                    evalled_values.push(Value::Boolean(lhs_num > rhs_num));
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(rhs.0, Statement_::Expr(*rhs.clone()))));
                            stmts_to_eval
                                .push((false, Statement(lhs.0, Statement_::Expr(*lhs.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(
                        _,
                        Expression_::BinaryOperator(
                            lhs,
                            op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
                            rhs,
                        ),
                    )) => {
                        if done_children {
                            let rhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for RHS of binary operator");
                            let lhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for LHS of binary operator");

                            let lhs_num = match lhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected an integer, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_num = match rhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(EvalError::UserError(format!(
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
                                    evalled_values.push(Value::Boolean(lhs_num == rhs_num));
                                }
                                BinaryOperatorKind::NotEqual => {
                                    evalled_values.push(Value::Boolean(lhs_num != rhs_num));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(rhs.0, Statement_::Expr(*rhs.clone()))));
                            stmts_to_eval
                                .push((false, Statement(lhs.0, Statement_::Expr(*lhs.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(
                        _,
                        Expression_::BinaryOperator(
                            lhs,
                            op @ (BinaryOperatorKind::And | BinaryOperatorKind::Or),
                            rhs,
                        ),
                    )) => {
                        if done_children {
                            let rhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for RHS of binary operator");
                            let lhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for LHS of binary operator");

                            let lhs_bool = match lhs_value {
                                Value::Boolean(b) => b,
                                _ => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected a bool, but got: {}",
                                        lhs_value
                                    )));
                                }
                            };
                            let rhs_bool = match rhs_value {
                                Value::Boolean(b) => b,
                                _ => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected a bool, but got: {}",
                                        rhs_value
                                    )));
                                }
                            };

                            match op {
                                BinaryOperatorKind::And => {
                                    evalled_values.push(Value::Boolean(lhs_bool && rhs_bool));
                                }
                                BinaryOperatorKind::Or => {
                                    evalled_values.push(Value::Boolean(lhs_bool || rhs_bool));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            // TODO: do short-circuit evaluation of && and ||.
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(rhs.0, Statement_::Expr(*rhs.clone()))));
                            stmts_to_eval
                                .push((false, Statement(lhs.0, Statement_::Expr(*lhs.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(_, Expression_::Call(receiver, ref args))) => {
                        if done_children {
                            let receiver_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for call receiver");
                            let mut arg_values = vec![];
                            for _ in 0..args.len() {
                                arg_values.push(
                                    evalled_values.pop().expect(
                                        "Popped an empty value for stack for call arguments",
                                    ),
                                );
                            }

                            match receiver_value {
                                Value::Fun(_, name, params, body) => {
                                    if params.len() != arg_values.len() {
                                        return Err(EvalError::UserError(format!(
                                            "Expected {} arguments to function {}, but got {}",
                                            params.len(),
                                            name.0,
                                            arg_values.len()
                                        )));
                                    }

                                    env.push_new_fun_scope(&name);
                                    for (param, value) in params.iter().zip(arg_values.iter()) {
                                        env.set_with_fun_scope(param, value.clone());
                                    }

                                    let mut fun_subexprs = vec![];
                                    for stmt in body.iter().rev() {
                                        fun_subexprs.push((false, stmt.clone()));
                                    }
                                    stmts_to_eval_per_fun.push(fun_subexprs);
                                    continue;
                                }
                                Value::BuiltinFunction(kind) => match kind {
                                    BuiltinFunctionKind::Print => {
                                        if args.len() != 1 {
                                            return Err(EvalError::UserError(format!(
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
                                                return Err(EvalError::UserError(format!(
                                                    "Expected a string, but got: {}",
                                                    v
                                                )));
                                            }
                                        }
                                        evalled_values.push(Value::Void);
                                    }
                                    BuiltinFunctionKind::IntToString => {
                                        if args.len() != 1 {
                                            return Err(EvalError::UserError(format!(
                                                "Function print requires 1 argument, but got: {}",
                                                args.len()
                                            )));
                                        }
                                        match &arg_values[0] {
                                            Value::Integer(i) => {
                                                evalled_values
                                                    .push(Value::String(format!("{}", i)));
                                            }
                                            v => {
                                                return Err(EvalError::UserError(format!(
                                                    "Expected an integer, but got: {}",
                                                    v
                                                )));
                                            }
                                        }
                                    }
                                },
                                v => {
                                    return Err(EvalError::UserError(format!(
                                        "Expected a function, but got: {}",
                                        v
                                    )));
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));

                            stmts_to_eval.push((
                                false,
                                Statement(receiver.0, Statement_::Expr(*receiver.clone())),
                            ));
                            for arg in args {
                                stmts_to_eval
                                    .push((false, Statement(arg.0, Statement_::Expr(arg.clone()))));
                            }
                        }
                    }
                }
            } else {
                assert!(stmts_to_eval.is_empty());
                stmts_to_eval_per_fun.pop();

                // Reached end of this block. Pop to the parent.
                if env.stack.len() > 1 {
                    // Don't pop the outer scope: that's for the top level environment.
                    env.pop_fun_scope();
                }
            }
        } else {
            break;
        }
    }

    Ok(evalled_values
        .pop()
        .expect("Should have a value from the last expression"))
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
    use crate::parse::parse_stmts_from_str;

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
            0,
            Statement_::Expr(Expression(0, Expression_::BoolLiteral(true))),
        )];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let stmts = vec![Statement(
            0,
            Statement_::Let(
                VariableName("foo".into()),
                Expression(0, Expression_::BoolLiteral(true)),
            ),
        )];
        eval_stmts(&stmts, &mut env).unwrap();

        let stmts = vec![Statement(
            0,
            Statement_::Expr(Expression(
                0,
                Expression_::Variable(VariableName("foo".into())),
            )),
        )];
        eval_stmts(&stmts, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_stmts() {
        let stmts = parse_stmts_from_str("true; false;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_add() {
        let stmts = parse_stmts_from_str("1 + 2;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_let() {
        let stmts = parse_stmts_from_str("let foo = true; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let stmts = parse_stmts_from_str("let foo = true; let foo = false;").unwrap();

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

        let stmts = parse_stmts_from_str("f();").unwrap();
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

        let stmts = parse_stmts_from_str("f(123);").unwrap();
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

        let stmts = parse_stmts_from_str("f(1, 2);").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_while() {
        let stmts = parse_stmts_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

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

        let stmts = parse_stmts_from_str("let i = 0; id(i); i;").unwrap();
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

        let stmts = parse_stmts_from_str("f();").unwrap();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }
}
