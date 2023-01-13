use std::io::Write;

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
    match pop_token(tokens) {
        Some(_token) => Ok(Expression::Integer(1)),
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
