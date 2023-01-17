use regex::Regex;

#[derive(Debug)]
pub enum Expression {
    Integer(i64),
    Boolean(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
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
            pop_token(tokens);
            return Ok(Expression::Boolean(true));
        }
        if *token == "false" {
            pop_token(tokens);
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

pub fn parse_toplevel(tokens: &mut &[&str]) -> Result<Expression, String> {
    let expr = parse_expression(tokens)?;

    if !tokens.is_empty() {
        return Err(format!("Tokens left after parsing: {:?}", tokens));
    }

    Ok(expr)
}

pub fn lex(s: &str) -> Vec<&str> {
    if s.is_empty() {
        return vec![];
    }

    s.split(' ').collect()
}
