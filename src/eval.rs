use std::{collections::HashMap, fmt::Display};

use crate::parse::Statement;
use crate::parse::{lex, parse_expression, Expression};
use crate::prompt::prompt_symbol;

use owo_colors::OwoColorize;
use rustyline::Editor;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(String, Vec<String>, Vec<Statement>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(name, _, _) => write!(f, "(function: {})", name),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    pub file_scope: HashMap<String, Value>,
    pub fun_scopes: Vec<HashMap<String, Value>>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            file_scope: HashMap::new(),
            // TODO: having a toplevel function scope is hard to reason about.
            fun_scopes: vec![HashMap::new()],
        }
    }
}

impl Env {
    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(fun_scope) = self.fun_scopes.last() {
            if let Some(value) = fun_scope.get(name) {
                return Some(value.clone());
            }
        }

        if let Some(value) = self.file_scope.get(name) {
            return Some(value.clone());
        }

        None
    }

    pub fn push_new_fun_scope(&mut self) {
        self.fun_scopes.push(HashMap::new());
    }

    pub fn pop_fun_scope(&mut self) {
        self.fun_scopes.pop().unwrap();
    }

    pub fn set_with_file_scope(&mut self, name: &str, value: Value) {
        self.file_scope.insert(name.to_string(), value);
    }

    pub fn set_with_fun_scope(&mut self, name: &str, value: Value) {
        let fun_scope = &mut self.fun_scopes.last_mut().unwrap();
        fun_scope.insert(name.to_string(), value);
    }
}

pub fn evaluate_stmt(stmt: &Statement, env: &mut Env) -> Result<Value, String> {
    match stmt {
        Statement::Expr(e) => evaluate_expr(e, env),
        Statement::Let(variable, expr) => {
            // TODO: error if the variable is already defined.
            let value = evaluate_expr(expr, env)?;

            // TODO: does this make sense for scope for let outside a function?
            env.set_with_fun_scope(variable, value.clone());
            Ok(value)
        }
        Statement::Fun(name, params, body) => {
            let value = Value::Fun(name.clone(), params.clone(), body.clone());
            env.set_with_file_scope(name, value.clone());
            Ok(value)
        }
    }
}

fn evaluate_expr(expr: &Expression, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Boolean(b) => Ok(Value::Boolean(*b)),
        Expression::BinaryOperator(lhs, _, rhs) => {
            let lhs_value = evaluate_expr(lhs, env)?;
            let rhs_value = evaluate_expr(rhs, env)?;

            let lhs_num = match lhs_value {
                Value::Integer(i) => i,
                _ => {
                    let lhs_new =
                        read_replacement(&format!("Expected an integer, but got: {}", lhs_value))?;
                    match evaluate_expr(&lhs_new, env)? {
                        Value::Integer(i) => i,
                        v => {
                            return Err(format!("Expected an integer, but got: {}", v));
                        }
                    }
                }
            };

            let rhs_num = match rhs_value {
                Value::Integer(i) => i,
                _ => {
                    let rhs_new =
                        read_replacement(&format!("Expected an integer, but got: {}", rhs_value))?;
                    match evaluate_expr(&rhs_new, env)? {
                        Value::Integer(i) => i,
                        v => {
                            return Err(format!("Expected an integer, but got: {}", v));
                        }
                    }
                }
            };

            Ok(Value::Integer(lhs_num + rhs_num))
        }
        Expression::Variable(s) => match env.get(s) {
            Some(v) => Ok(v.clone()),
            None => {
                let expr_new = read_replacement(&format!("Unbound variable: {}", s))?;
                evaluate_expr(&expr_new, env)
            }
        },
        Expression::Call(receiver, args) => {
            let mut args_values = vec![];
            for arg in args {
                args_values.push(evaluate_expr(arg, env)?);
            }

            match evaluate_expr(receiver, env)? {
                Value::Fun(name, params, body) => {
                    if args_values.len() != params.len() {
                        // TODO: prompt user for extra arguments.
                        return Err(format!(
                            "Function {} requires {} arguments, but got: {}",
                            name,
                            params.len(),
                            args_values.len()
                        ));
                    }

                    env.push_new_fun_scope();
                    for (var_name, value) in params.iter().zip(args_values) {
                        env.set_with_fun_scope(&var_name, value);
                    }

                    // TODO: define a void type.
                    let mut res = Value::Boolean(false);
                    for stmt in body {
                        res = evaluate_stmt(&stmt, env)?;
                    }

                    env.pop_fun_scope();
                    Ok(res)
                }
                v => {
                    return Err(format!("Expected a function, but got: {}", v));
                }
            }
        }
    }
}

#[derive(Debug)]
enum NextStep {
    NextStmt { idx: usize },
    EvalSubexpressions(usize),
    EvalLet(String),
    EvalCall { num_args: usize },
    EvalAdd,
}

fn eval_iter(stmts: &[Statement], env: &mut Env) -> Result<Value, String> {
    let mut subexprs_to_eval: Vec<Expression> = vec![];
    let mut subexprs_values: Vec<Value> = vec![Value::Integer(1234)];
    let mut next_steps: Vec<NextStep> = vec![NextStep::NextStmt { idx: 0 }];
    let mut fun_bodies: Vec<Vec<Statement>> = vec![stmts.to_vec()];

    loop {
        if let Some(step) = dbg!(next_steps.pop()) {
            dbg!(&subexprs_values);
            match step {
                NextStep::NextStmt { idx } => {
                    let stmts = fun_bodies
                        .last()
                        .expect("Function stack should never be empty");
                    if let Some(stmt) = stmts.get(idx) {
                        if idx > 0 {
                            // Discard value from previous statement.
                            subexprs_values.pop().expect("Popped an empty value stack");
                        }
                        match stmt {
                            Statement::Fun(name, params, body) => {
                                let value = Value::Fun(name.clone(), params.clone(), body.clone());
                                env.set_with_file_scope(name, value.clone());
                                subexprs_values.push(value);
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                            }
                            Statement::Let(v, e) => {
                                subexprs_to_eval.push(e.clone());
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                                next_steps.push(NextStep::EvalLet(v.clone()));
                                next_steps.push(NextStep::EvalSubexpressions(1));
                            }
                            Statement::Expr(e) => {
                                subexprs_to_eval.push(e.clone());
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                                next_steps.push(NextStep::EvalSubexpressions(1));
                            }
                        }
                    } else {
                        // Reached end of this block. Pop to the parent.
                        // TODO: function return value.
                        env.pop_fun_scope();
                        fun_bodies.pop();
                    }
                }
                NextStep::EvalSubexpressions(n) => {
                    assert!(n > 0);
                    if n > 1 {
                        next_steps.push(NextStep::EvalSubexpressions(n - 1));
                    }

                    let expr = subexprs_to_eval
                        .pop()
                        .expect("Expected a non-empty subexpression stack");
                    match expr {
                        Expression::Integer(i) => {
                            subexprs_values.push(Value::Integer(i));
                        }
                        Expression::Boolean(b) => {
                            subexprs_values.push(Value::Boolean(b));
                        }
                        Expression::BinaryOperator(lhs, _, rhs) => {
                            subexprs_to_eval.push(*rhs.clone());
                            subexprs_to_eval.push(*lhs.clone());

                            next_steps.push(NextStep::EvalSubexpressions(2));
                        }
                        Expression::Variable(s) => match env.get(&s) {
                            Some(v) => subexprs_values.push(v),
                            None => {
                                // TODO: read_replacement
                                return Err(format!("Unbound variable: {}", s));
                            }
                        },
                        Expression::Call(receiver, args) => {
                            for arg in &args {
                                subexprs_to_eval.push(arg.clone());
                            }
                            subexprs_to_eval.push(*receiver.clone());

                            next_steps.push(NextStep::EvalCall {
                                num_args: args.len(),
                            });
                            next_steps.push(NextStep::EvalSubexpressions(args.len() + 1));
                        }
                    }
                }
                NextStep::EvalLet(variable) => {
                    let value = subexprs_values
                        .pop()
                        .expect("Got an empty value stack when evaluating let");

                    // TODO: does this make sense for scope for let outside a function?
                    env.set_with_fun_scope(&variable, value.clone());
                }
                NextStep::EvalAdd => {
                    let rhs_value = subexprs_values
                        .pop()
                        .expect("Got an empty value stack when evaluating RHS of +");
                    let lhs_value = subexprs_values
                        .pop()
                        .expect("Got an empty value stack when evaluating LHS of +");

                    let lhs_num = match lhs_value {
                        Value::Integer(i) => i,
                        _ => {
                            let lhs_new = read_replacement(&format!(
                                "Expected an integer, but got: {}",
                                lhs_value
                            ))?;
                            match evaluate_expr(&lhs_new, env)? {
                                Value::Integer(i) => i,
                                v => {
                                    return Err(format!("Expected an integer, but got: {}", v));
                                }
                            }
                        }
                    };

                    let rhs_num = match rhs_value {
                        Value::Integer(i) => i,
                        _ => {
                            let rhs_new = read_replacement(&format!(
                                "Expected an integer, but got: {}",
                                rhs_value
                            ))?;
                            match evaluate_expr(&rhs_new, env)? {
                                Value::Integer(i) => i,
                                v => {
                                    return Err(format!("Expected an integer, but got: {}", v));
                                }
                            }
                        }
                    };

                    subexprs_values.push(Value::Integer(lhs_num + rhs_num))
                }
                NextStep::EvalCall { num_args } => {
                    let receiver = subexprs_values
                        .pop()
                        .expect("Popped an empty value stack for call receiver");

                    let mut args = vec![];
                    for _ in 0..num_args {
                        args.push(
                            subexprs_values
                                .pop()
                                .expect("Popped an empty value for stack for call arguments"),
                        );
                    }

                    match receiver {
                        Value::Fun(name, params, body) => {
                            if args.len() != params.len() {
                                // TODO: prompt user for extra arguments.
                                return Err(format!(
                                    "Function {} requires {} arguments, but got: {}",
                                    name,
                                    params.len(),
                                    args.len()
                                ));
                            }

                            env.push_new_fun_scope();
                            for (var_name, value) in params.iter().zip(args) {
                                env.set_with_fun_scope(&var_name, value);
                            }

                            fun_bodies.push(body);
                            next_steps.push(NextStep::NextStmt { idx: 0 });

                            // Push block.
                            // Eval block.
                            // Pop fun scope.
                            // env.pop_fun_scope();
                        }
                        v => {
                            return Err(format!("Expected a function, but got: {}", v));
                        }
                    }
                }
            }
        } else {
            break;
        }
    }

    Ok(subexprs_values
        .pop()
        .expect("Should have a value from the last expression"))
}

fn read_replacement(msg: &str) -> Result<Expression, String> {
    println!("{}: {}", "Unexpected error".bright_red(), msg);
    println!("What value should be used instead?\n");

    let mut rl: Editor<()> = Editor::new().unwrap();
    let input = rl
        .readline(&prompt_symbol(1))
        .expect("error: unable to read user input");

    let tokens = lex(input.trim())?;
    let mut token_ptr = &tokens[..];
    parse_expression(&mut token_ptr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_bool_literal() {
        let mut env = Env::default();
        let value = evaluate_stmt(&Statement::Expr(Expression::Boolean(true)), &mut env).unwrap();
        assert!(matches!(value, Value::Boolean(true)));
    }

    #[test]
    fn test_eval_iter_bool_literal() {
        let stmts = vec![Statement::Expr(Expression::Boolean(true))];

        let mut env = Env::default();
        let value = eval_iter(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }
}
