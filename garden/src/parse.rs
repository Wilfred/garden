use regex::Regex;

#[derive(Debug)]
pub enum Expression {
    Integer(i64),
    Boolean(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
}

fn pop_token<'a>(tokens: &mut &[&'a str]) -> Option<&'a str> {
    if tokens.is_empty() {
        return None;
    }

    let token = tokens[0];
    *tokens = &tokens[1..];
    Some(token)
}

fn peek_token<'a>(tokens: &[&'a str]) -> Option<&'a str> {
    tokens.first().copied()
}

fn require_a_token<'a>(
    tokens: &mut &[&'a str],
    token_description: &str,
) -> Result<&'a str, String> {
    match pop_token(tokens) {
        Some(token) => Ok(token),
        None => Err(format!("Expected {}, got EOF", token_description)),
    }
}

fn require_token<'a>(tokens: &mut &[&'a str], expected: &str) -> Result<(), String> {
    match pop_token(tokens) {
        Some(token) => {
            if token == expected {
                Ok(())
            } else {
                Err(format!("Expected `{}`, got `{}`", expected, token))
            }
        }
        None => Err(format!("Expected `{}`, got EOF", expected)),
    }
}

fn parse_integer(tokens: &mut &[&str]) -> Result<Expression, String> {
    let re = Regex::new(r"^[0-9]+$").unwrap();

    let token = require_a_token(tokens, "integer literal")?;
    if re.is_match(token) {
        let i: i64 = token.parse().unwrap();
        Ok(Expression::Integer(i))
    } else {
        Err(format!("Not a valid integer literal: {}", token))
    }
}

fn parse_variable(tokens: &mut &[&str]) -> Result<Expression, String> {
    let token = require_a_token(tokens, "variable name")?;
    Ok(Expression::Variable(token.into()))
}

fn parse_parenthesis_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    require_token(tokens, "(")?;
    let expr = parse_expression(tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_simple_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    if let Some(token) = peek_token(tokens) {
        if token == "(" {
            return parse_parenthesis_expression(tokens);
        }

        if token == "true" {
            pop_token(tokens);
            return Ok(Expression::Boolean(true));
        }
        if token == "false" {
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

    if let Some(token) = peek_token(tokens) {
        if token == "+" {
            let operator = token;
            pop_token(tokens);

            let rhs_expr = parse_simple_expression(tokens)?;

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
    let integer_re = Regex::new(r"^[0-9]+").unwrap();
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();

    let mut res = vec![];

    let mut s = s;
    while !s.is_empty() {
        if let Some(new_s) = s.strip_prefix('+') {
            res.push(&s[0..1]);
            s = new_s;
        } else if let Some(new_s) = s.strip_prefix('(') {
            res.push(&s[0..1]);
            s = new_s;
        } else if let Some(new_s) = s.strip_prefix(')') {
            res.push(&s[0..1]);
            s = new_s;
        } else if let Some(integer_match) = integer_re.find(s) {
            res.push(integer_match.as_str());
            s = &s[integer_match.end()..];
        } else if let Some(variable_match) = variable_re.find(s) {
            res.push(variable_match.as_str());
            s = &s[variable_match.end()..];
        } else {
            break;
        }
    }

    res
}
