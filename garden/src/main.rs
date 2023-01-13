use std::io::Write;

use regex::Regex;

#[derive(Debug)]
enum Expression {
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

fn lex(s: &str) -> Vec<&str> {
    if s.is_empty() {
        return vec![];
    }

    s.split(' ').collect()
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
                let ast = parse_integer(&mut token_ptr);
                println!("{:?}", ast)
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
