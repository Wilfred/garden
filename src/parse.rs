use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
    Call(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // TODO: is Statement the best place for Fun?
    Fun(String, Vec<String>, Vec<Statement>),
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
        Ok(Expression::IntLiteral(i))
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
            return Ok(Expression::BoolLiteral(true));
        }
        if token == "false" {
            pop_token(tokens);
            return Ok(Expression::BoolLiteral(false));
        }

        let re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();
        if re.is_match(token) {
            return parse_variable_expression(tokens);
        }

        if token.starts_with("\"") {
            pop_token(tokens);
            return Ok(Expression::StringLiteral(
                token[1..token.len() - 1].to_owned(),
            ));
        }
    }

    parse_integer(tokens)
}

fn parse_call_arguments(tokens: &mut &[&str]) -> Result<Vec<Expression>, String> {
    require_token(tokens, "(")?;

    let mut args = vec![];
    loop {
        if let Some(token) = peek_token(tokens) {
            if token == ")" {
                break;
            }
        }

        let arg = parse_expression(tokens)?;
        args.push(arg);

        if let Some(token) = peek_token(tokens) {
            if token == "," {
                pop_token(tokens);
            } else if token == ")" {
                break;
            } else {
                return Err(format!(
                    "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                    token
                ));
            }
        } else {
            return Err("Invalid syntax: Expected `,` or `)` here, but got EOF".to_string());
        }
    }

    require_token(tokens, ")")?;
    Ok(args)
}

fn parse_simple_expression_or_call(tokens: &mut &[&str]) -> Result<Expression, String> {
    let expr = parse_simple_expression(tokens)?;

    if let Some(token) = peek_token(tokens) {
        if token == "(" {
            let arguments = parse_call_arguments(tokens)?;
            return Ok(Expression::Call(Box::new(expr), arguments));
        }
    }

    Ok(expr)
}

pub fn parse_expression(tokens: &mut &[&str]) -> Result<Expression, String> {
    let mut expr = parse_simple_expression_or_call(tokens)?;

    if let Some(token) = peek_token(tokens) {
        if token == "+" {
            let operator = token;
            pop_token(tokens);

            let rhs_expr = parse_simple_expression_or_call(tokens)?;

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

fn parse_function_params(tokens: &mut &[&str]) -> Result<Vec<String>, String> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if let Some(token) = peek_token(tokens) {
            if token == ")" {
                break;
            }
        }

        let param = parse_variable_name(tokens)?;
        params.push(param);

        if let Some(token) = peek_token(tokens) {
            if token == "," {
                pop_token(tokens);
            } else if token == ")" {
                break;
            } else {
                return Err(format!(
                    "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                    token
                ));
            }
        } else {
            return Err("Invalid syntax: Expected `,` or `)` here, but got EOF".to_string());
        }
    }

    require_token(tokens, ")")?;
    Ok(params)
}

fn parse_function_body(tokens: &mut &[&str]) -> Result<Vec<Statement>, String> {
    require_token(tokens, "{")?;

    let mut stmts = vec![];
    loop {
        if let Some(token) = peek_token(tokens) {
            if token == "}" {
                break;
            }
        } else {
            return Err("Invalid syntax: Expected `}}` here, but got EOF".to_string());
        }

        let stmt = parse_statement(tokens)?;
        stmts.push(stmt);
    }

    require_token(tokens, "}")?;
    Ok(stmts)
}

fn parse_function(tokens: &mut &[&str]) -> Result<Statement, String> {
    require_token(tokens, "fun")?;
    let name = parse_variable_name(tokens)?;
    let params = parse_function_params(tokens)?;
    let body = parse_function_body(tokens)?;

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

pub fn parse_toplevel(tokens: &mut &[&str]) -> Result<Vec<Statement>, String> {
    let mut res = vec![];

    while !tokens.is_empty() {
        res.push(parse_statement(tokens)?);
    }

    Ok(res)
}

pub fn lex(s: &str) -> Result<Vec<&str>, String> {
    let integer_re = Regex::new(r"^[0-9]+").unwrap();
    let string_re = Regex::new(r#"^"[^"]*""#).unwrap();
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();

    let mut res = vec![];

    let mut s = s;
    'outer: while !s.is_empty() {
        s = s.trim();
        for token_char in ['+', '(', ')', '{', '}', ';', '=', ','] {
            if let Some(new_s) = s.strip_prefix(token_char) {
                res.push(&s[0..1]);
                s = new_s;
                continue 'outer;
            }
        }
        if let Some(integer_match) = integer_re.find(s) {
            res.push(integer_match.as_str());
            s = &s[integer_match.end()..];
        } else if let Some(string_match) = string_re.find(s) {
            res.push(string_match.as_str());
            s = &s[string_match.end()..];
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

    #[test]
    fn test_parse_bool_literal() {
        let src = "true;";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(ast, vec![Statement::Expr(Expression::BoolLiteral(true))]);
    }

    #[test]
    fn test_parse_variable() {
        let src = "abc_def;";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement::Expr(Expression::Variable("abc_def".to_string()))]
        );
    }
}
