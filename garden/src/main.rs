mod parse;

use std::{fmt::Display, io::Write};

use crate::parse::{lex, parse_toplevel, Expression};
use owo_colors::OwoColorize;

#[derive(Debug)]
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

fn evaluate(expr: &Expression) -> Result<Value, String> {
    match expr {
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Boolean(b) => Ok(Value::Boolean(*b)),
        Expression::BinaryOperator(lhs, _, rhs) => {
            let lhs_value = evaluate(lhs)?;
            let rhs_value = evaluate(rhs)?;

            let lhs_num = match lhs_value {
                Value::Integer(i) => i,
                _ => {
                    let lhs_new =
                        read_replacement(&format!("Expected an integer, but got: {}", lhs_value))?;
                    match evaluate(&lhs_new)? {
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
                    match evaluate(&rhs_new)? {
                        Value::Integer(i) => i,
                        v => {
                            return Err(format!("Expected an integer, but got: {}", v));
                        }
                    }
                }
            };

            Ok(Value::Integer(lhs_num + rhs_num))
        }
        Expression::Variable(s) => Err(format!("Unbound variable: {}", s)),
    }
}

fn read_replacement(msg: &str) -> Result<Expression, String> {
    println!("{}", msg);
    println!("Oh no! What value should be used instead?\n");
    print!("[1]> ");
    std::io::stdout().flush().unwrap();

    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("error: unable to read user input");

    let tokens = lex(input.trim());
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

    loop {
        print!("\n{}", "> ".bold());
        let mut input = String::new();
        std::io::stdout().flush().unwrap();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                let tokens = lex(input.trim());
                let mut token_ptr = &tokens[..];

                match parse_toplevel(&mut token_ptr) {
                    Ok(expr) => match evaluate(&expr) {
                        Ok(result) => {
                            println!("{}", result)
                        }
                        Err(e) => {
                            println!("parsed: {:?}", expr);
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
