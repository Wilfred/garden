use std::{fmt::Display, io::Write};

use owo_colors::OwoColorize;
use regex::Regex;

#[derive(Debug)]
enum Expression {
    Integer(i64),
    Boolean(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
}

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

fn pop_token<'a, 'b>(tokens: &'a mut &[&'b str]) -> Option<&'b str> {
    if tokens.is_empty() {
        return None;
    }

    let token = tokens[0];
    *tokens = &tokens[1..];
    Some(token)
}

fn parse_integer(tokens: &mut &[&str]) -> Result<Expression, String> {
    let re = Regex::new(r"^[0-9]+$").unwrap();

    match pop_token(tokens) {
        Some(token) => {
            if re.is_match(token) {
                let i: i64 = token.parse().unwrap();
                Ok(Expression::Integer(i))
            } else {
                Err(format!("Not a valid integer literal: {}", token))
            }
        }
        None => Err("Expected integer, got EOF".into()),
    }
}

fn parse_variable(tokens: &mut &[&str]) -> Result<Expression, String> {
    match pop_token(tokens) {
        Some(token) => Ok(Expression::Variable(token.into())),
        None => Err("Expected variable, got EOF".into()),
    }
}

fn parse_simple_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    if let Some(token) = tokens.first() {
        if *token == "true" {
            return Ok(Expression::Boolean(true));
        }
        if *token == "false" {
            return Ok(Expression::Boolean(false));
        }

        let re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();
        if re.is_match(token) {
            return parse_variable(tokens);
        }
    }

    parse_integer(tokens)
}

fn parse_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    let mut expr = parse_simple_expression(tokens)?;

    if let Some(token) = tokens.first() {
        if *token == "+" {
            let operator = pop_token(tokens).unwrap();

            let rhs_expr = parse_expression(tokens)?;

            expr = Expression::BinaryOperator(
                Box::new(expr),
                operator.to_string(),
                Box::new(rhs_expr),
            );
        }
    }

    Ok(expr)
}

fn parse_toplevel(tokens: &mut &[&str]) -> Result<Expression, String> {
    let expr = parse_expression(tokens)?;

    if !tokens.is_empty() {
        return Err(format!("Tokens left after parsing: {:?}", tokens));
    }

    Ok(expr)
}

fn lex(s: &str) -> Vec<&str> {
    if s.is_empty() {
        return vec![];
    }

    s.split(' ').collect()
}

fn evaluate(expr: &Expression) -> Result<Value, String> {
    match expr {
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Boolean(b) => Ok(Value::Boolean(*b)),
        Expression::BinaryOperator(lhs, _, rhs) => {
            let lhs_value = evaluate(lhs)?;
            let rhs_value = evaluate(rhs)?;

            match (lhs_value, rhs_value) {
                (Value::Integer(lhs_i), Value::Integer(rhs_i)) => Ok(Value::Integer(lhs_i + rhs_i)),
                _ => Err("Addition requires integers.".into()),
            }
        }
        Expression::Variable(s) => Err(format!("Unbound variable: {}", s)),
    }
}

fn main() {
    println!("{}", "Welcome to the garden!".bold());

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
