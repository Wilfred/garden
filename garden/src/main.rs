use std::io::Write;

use regex::Regex;

#[derive(Debug)]
enum Expression {
    Integer(i64),
    Variable(String),
}

#[derive(Debug)]
enum Value {
    Integer(i64),
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

fn parse_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    if let Some(token) = tokens.first() {
        let re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();
        if re.is_match(token) {
            return parse_variable(tokens);
        }
    }

    parse_integer(tokens)
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
        Expression::Variable(s) => Err(format!("Unbound variable: {}", s)),
    }
}

fn main() {
    println!("Welcome to the garden!");

    loop {
        print!("> ");
        let mut input = String::new();
        std::io::stdout().flush().unwrap();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                let tokens = lex(input.trim());
                let mut token_ptr = &tokens[..];

                match parse_expression(&mut token_ptr) {
                    Ok(expr) => {
                        println!("parsed: {:?}", expr);
                        let result = evaluate(&expr);
                        println!("value: {:?}", result);
                    }
                    Err(e) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
