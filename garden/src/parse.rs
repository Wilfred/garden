use regex::Regex;

#[derive(Debug)]
pub enum Expression {
    Integer(i64),
    Boolean(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
}

#[derive(Debug)]
pub enum Statement {
    // TODO: is Statement the best place for Fun?
    Fun(String, Vec<String>, Vec<Expression>),
    Let(String, Expression),
    Expr(Expression),
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

fn parse_variable_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    let variable = parse_variable_name(tokens)?;
    Ok(Expression::Variable(variable))
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
            return parse_variable_expression(tokens);
        }
    }

    parse_integer(tokens)
}

pub fn parse_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
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

fn parse_statement(tokens: &mut &[&str]) -> Result<Statement, String> {
    if let Some(token) = peek_token(tokens) {
        if token == "let" {
            return parse_let_statement(tokens);
        }

        if token == "fun" {
            return parse_function(tokens);
        }
    }

    let expr = parse_expression(tokens)?;
    require_token(tokens, ";")?;
    Ok(Statement::Expr(expr))
}

fn parse_function(tokens: &mut &[&str]) -> Result<Statement, String> {
    require_token(tokens, "fun")?;
    let name = parse_variable_name(tokens)?;

    require_token(tokens, "(")?;
    let params = vec!["todo".to_string()];
    require_token(tokens, ")")?;

    require_token(tokens, "{")?;
    let body = vec![]; // TODO
    require_token(tokens, "}")?;

    Ok(Statement::Fun(name, params, body))
}

fn parse_variable_name(tokens: &mut &[&str]) -> Result<String, String> {
    // TODO: this is duplicated with lex().
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();

    let variable = require_a_token(tokens, "variable name")?;
    if !variable_re.is_match(variable) {
        return Err(format!("Invalid variable name: '{}'", variable));
    }

    for reserved in ["let", "fun"] {
        if variable == reserved {
            return Err(format!(
                "'{}' is a reserved word that cannot be used as a variable",
                variable
            ));
        }
    }

    Ok(variable.to_string())
}

fn parse_let_statement(tokens: &mut &[&str]) -> Result<Statement, String> {
    require_token(tokens, "let")?;
    let variable = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_expression(tokens)?;
    require_token(tokens, ";")?;

    Ok(Statement::Let(variable, expr))
}

pub fn parse_toplevel(tokens: &mut &[&str]) -> Result<Statement, String> {
    let expr = parse_statement(tokens)?;

    if !tokens.is_empty() {
        return Err(format!("Tokens left after parsing: {:?}", tokens));
    }

    Ok(expr)
}

pub fn lex(s: &str) -> Result<Vec<&str>, String> {
    let integer_re = Regex::new(r"^[0-9]+").unwrap();
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();

    let mut res = vec![];

    let mut s = s;
    'outer: while !s.is_empty() {
        s = s.trim();
        for token_char in ['+', '(', ')', '{', '}', ';', '='] {
            if let Some(new_s) = s.strip_prefix(token_char) {
                res.push(&s[0..1]);
                s = new_s;
                continue 'outer;
            }
        }
        if let Some(integer_match) = integer_re.find(s) {
            res.push(integer_match.as_str());
            s = &s[integer_match.end()..];
        } else if let Some(variable_match) = variable_re.find(s) {
            res.push(variable_match.as_str());
            s = &s[variable_match.end()..];
        } else {
            break;
        }
    }

    if !s.is_empty() {
        return Err(format!("Unrecognized syntax: '{}'", s));
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_spaces() {
        assert_eq!(lex("1 + 2").unwrap(), vec!["1", "+", "2"]);
    }

    #[test]
    fn test_lex_no_spaces() {
        assert_eq!(lex("1+2").unwrap(), vec!["1", "+", "2"]);
    }
}
