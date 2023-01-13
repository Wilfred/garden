use std::io::Write;

#[derive(Debug)]
enum Expression {
    Integer(i64),
}

fn parse_integer(tokens: &[&str]) -> Result<Expression, String> {
    if tokens.first().is_some() {
        return Ok(Expression::Integer(1));
    }

    Ok(Expression::Integer(0))
}

fn lex(s: &str) -> Vec<&str> {
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
                let tokens = lex(&input);
                let ast = parse_integer(&tokens);
                println!("{:?}", ast)
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
