use std::{collections::HashMap, fmt::Display};

use crate::parse::Statement;
use crate::parse::{lex, parse_expression, Expression};
use crate::prompt::prompt_symbol;

use owo_colors::OwoColorize;

#[derive(Debug, Clone)]
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

pub fn evaluate_stmt(stmt: &Statement, env: &mut HashMap<String, Value>) -> Result<Value, String> {
    match stmt {
        Statement::Expr(e) => evaluate_expr(e, env),
        Statement::Let(variable, expr) => {
            let value = evaluate_expr(expr, env)?;
            env.insert(variable.to_string(), value.clone());
            Ok(value)
        }
        Statement::Fun(name, params, body) => {
            let value = Value::Fun(name.clone(), params.clone(), body.clone());
            env.insert(name.to_string(), value.clone());
            Ok(value)
        }
    }
}

fn evaluate_expr(expr: &Expression, env: &HashMap<String, Value>) -> Result<Value, String> {
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
                Value::Fun(name, params, _body) => {
                    if args_values.len() != params.len() {
                        // TODO: prompt user for extra arguments.
                        return Err(format!(
                            "Function {} requires {} arguments, but got: {}",
                            name,
                            params.len(),
                            args_values.len()
                        ));
                    }

                    todo!();
                }
                v => {
                    return Err(format!("Expected a function, but got: {}", v));
                }
            }
        }
    }
}

fn read_replacement(msg: &str) -> Result<Expression, String> {
    println!("{}: {}", "Unexpected error".bright_red(), msg);
    println!("What value should be used instead?\n");
    prompt_symbol(1);

    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("error: unable to read user input");

    let tokens = lex(input.trim())?;
    let mut token_ptr = &tokens[..];
    parse_expression(&mut token_ptr)
}
