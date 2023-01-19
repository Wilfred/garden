mod parse;

use std::{collections::HashMap, fmt::Display, io::Write};

use crate::parse::{lex, parse_toplevel, Expression};
use owo_colors::OwoColorize;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
        }
    }
}

fn evaluate(expr: &Expression, env: &HashMap<String, Value>) -> Result<Value, String> {
    match expr {
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Boolean(b) => Ok(Value::Boolean(*b)),
        Expression::BinaryOperator(lhs, _, rhs) => {
            let lhs_value = evaluate(lhs, env)?;
            let rhs_value = evaluate(rhs, env)?;

            let lhs_num = match lhs_value {
                Value::Integer(i) => i,
                _ => {
                    let lhs_new =
                        read_replacement(&format!("Expected an integer, but got: {}", lhs_value))?;
                    match evaluate(&lhs_new, env)? {
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
                    match evaluate(&rhs_new, env)? {
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
                evaluate(&expr_new, env)
            }
        },
    }
}

fn prompt_symbol(depth: usize) {
    if depth > 0 {
        print!("[{}]", depth);
    }
    print!("{} ", ">".green().bold());
    std::io::stdout().flush().unwrap();
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
    parse_toplevel(&mut token_ptr)
}

fn main() {
    println!(
        "{} {}{}",
        "Welcome to the".bold(),
        "garden".bold().green(),
        "!".bold()
    );

    let mut env: HashMap<String, Value> = HashMap::new();
    env.insert("x".into(), Value::Boolean(true));

    loop {
        println!();
        prompt_symbol(0);

        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                input = input.trim().to_string();

                if let Some(input) = input.strip_prefix(":parse ") {
                    let tokens = match lex(&input) {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            println!("{}: {}", "Error".bright_red(), e);
                            continue;
                        }
                    };
                    let mut token_ptr = &tokens[..];
                    println!("{:?}", parse_toplevel(&mut token_ptr));
                    continue;
                }

                let tokens = match lex(&input) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        println!("{}: {}", "Error".bright_red(), e);
                        continue;
                    }
                };
                let mut token_ptr = &tokens[..];

                match parse_toplevel(&mut token_ptr) {
                    Ok(expr) => match evaluate(&expr, &env) {
                        Ok(result) => {
                            println!("{}", result)
                        }
                        Err(e) => {
                            println!("{}: {}", "Error".bright_red(), e);
                        }
                    },
                    Err(e) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
