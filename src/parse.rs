use regex::Regex;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Position {
//     line: usize,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Expression_ {
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    BinaryOperator(Box<Expression>, String, Box<Expression>),
    Variable(String),
    Call(Box<Expression>, Vec<Expression>),
    If(Box<Expression>, Vec<Statement>, Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub usize, pub Expression_);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement_ {
    // TODO: is Statement the best place for Fun?
    Fun(String, Vec<String>, Vec<Statement>),
    Let(String, Expression),
    Expr(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement(pub usize, pub Statement_);

type Token<'a> = (usize, &'a str);

fn pop_token<'a>(tokens: &mut &[Token<'a>]) -> Option<Token<'a>> {
    if tokens.is_empty() {
        return None;
    }

    let token = tokens[0];
    *tokens = &tokens[1..];
    Some(token)
}

fn peek_token<'a>(tokens: &[Token<'a>]) -> Option<Token<'a>> {
    tokens.first().copied()
}

fn require_a_token<'a>(
    tokens: &mut &[Token<'a>],
    token_description: &str,
) -> Result<Token<'a>, String> {
    match pop_token(tokens) {
        Some(token) => Ok(token),
        None => Err(format!("Expected {}, got EOF", token_description)),
    }
}

fn require_token<'a>(tokens: &mut &[Token<'a>], expected: &str) -> Result<usize, String> {
    match pop_token(tokens) {
        Some((offset, token)) => {
            if token == expected {
                Ok(offset)
            } else {
                Err(format!("Expected `{}`, got `{}`", expected, token))
            }
        }
        None => Err(format!("Expected `{}`, got EOF", expected)),
    }
}

fn parse_integer(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let re = Regex::new(r"^[0-9]+$").unwrap();

    let (offset, token) = require_a_token(tokens, "integer literal")?;
    if re.is_match(token) {
        let i: i64 = token.parse().unwrap();
        Ok(Expression(offset, Expression_::IntLiteral(i)))
    } else {
        Err(format!("Not a valid integer literal: {}", token))
    }
}

fn parse_variable_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let (offset, variable) = parse_variable_name(tokens)?;
    Ok(Expression(offset, Expression_::Variable(variable)))
}

fn parse_parenthesis_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    require_token(tokens, "(")?;
    let expr = parse_expression(tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_if_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let offset = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_expression(tokens)?;
    require_token(tokens, ")")?;

    require_token(tokens, "{")?;
    require_token(tokens, "}")?;
    require_token(tokens, "else")?;
    require_token(tokens, "{")?;
    require_token(tokens, "}")?;

    Ok(Expression(
        offset,
        Expression_::If(Box::new(condition), vec![], vec![]),
    ))
}

fn parse_simple_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    if let Some((offset, token)) = peek_token(tokens) {
        if token == "(" {
            return parse_parenthesis_expression(tokens);
        }

        if token == "if" {
            return parse_if_expression(tokens);
        }

        if token == "true" {
            pop_token(tokens);
            return Ok(Expression(offset, Expression_::BoolLiteral(true)));
        }
        if token == "false" {
            pop_token(tokens);
            return Ok(Expression(offset, Expression_::BoolLiteral(false)));
        }

        let re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();
        if re.is_match(token) {
            return parse_variable_expression(tokens);
        }

        if token.starts_with("\"") {
            pop_token(tokens);
            return Ok(Expression(
                offset,
                Expression_::StringLiteral(token[1..token.len() - 1].to_owned()),
            ));
        }
    }

    parse_integer(tokens)
}

fn parse_call_arguments(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, String> {
    require_token(tokens, "(")?;

    let mut args = vec![];
    loop {
        if let Some((_, token)) = peek_token(tokens) {
            if token == ")" {
                break;
            }
        }

        let arg = parse_expression(tokens)?;
        args.push(arg);

        if let Some((_, token)) = peek_token(tokens) {
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

fn parse_simple_expression_or_call(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let expr = parse_simple_expression(tokens)?;

    if let Some((_, token)) = peek_token(tokens) {
        if token == "(" {
            let arguments = parse_call_arguments(tokens)?;
            return Ok(Expression(
                expr.0,
                Expression_::Call(Box::new(expr), arguments),
            ));
        }
    }

    Ok(expr)
}

pub fn parse_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let mut expr = parse_simple_expression_or_call(tokens)?;

    if let Some((_, token)) = peek_token(tokens) {
        if token == "+" {
            let operator = token;
            pop_token(tokens);

            let rhs_expr = parse_simple_expression_or_call(tokens)?;

            expr = Expression(
                expr.0,
                Expression_::BinaryOperator(
                    Box::new(expr),
                    operator.to_string(),
                    Box::new(rhs_expr),
                ),
            );
        }
    }

    Ok(expr)
}

fn parse_statement(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    if let Some((_, token)) = peek_token(tokens) {
        if token == "let" {
            return parse_let_statement(tokens);
        }

        if token == "fun" {
            return parse_function(tokens);
        }
    }

    let expr = parse_expression(tokens)?;
    require_token(tokens, ";")?;
    Ok(Statement(expr.0, Statement_::Expr(expr)))
}

fn parse_function_params(tokens: &mut &[Token<'_>]) -> Result<Vec<String>, String> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if let Some((_, token)) = peek_token(tokens) {
            if token == ")" {
                break;
            }
        }

        let (_, param) = parse_variable_name(tokens)?;
        params.push(param);

        if let Some((_, token)) = peek_token(tokens) {
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

fn parse_function_body(tokens: &mut &[Token<'_>]) -> Result<Vec<Statement>, String> {
    require_token(tokens, "{")?;

    let mut stmts = vec![];
    loop {
        if let Some((_, token)) = peek_token(tokens) {
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

fn parse_function(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let offset = require_token(tokens, "fun")?;
    let (_, name) = parse_variable_name(tokens)?;
    let params = parse_function_params(tokens)?;
    let body = parse_function_body(tokens)?;

    Ok(Statement(offset, Statement_::Fun(name, params, body)))
}

const RESERVED_WORDS: &[&str] = &["let", "fun", "true", "false", "if", "else"];

fn parse_variable_name(tokens: &mut &[Token<'_>]) -> Result<(usize, String), String> {
    // TODO: this is duplicated with lex().
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();

    let (offset, variable) = require_a_token(tokens, "variable name")?;
    if !variable_re.is_match(variable) {
        return Err(format!("Invalid variable name: '{}'", variable));
    }

    for reserved in RESERVED_WORDS {
        if variable == *reserved {
            return Err(format!(
                "'{}' is a reserved word that cannot be used as a variable",
                variable
            ));
        }
    }

    Ok((offset, variable.to_string()))
}

fn parse_let_statement(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let offset = require_token(tokens, "let")?;
    let (_, variable) = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_expression(tokens)?;
    require_token(tokens, ";")?;

    Ok(Statement(offset, Statement_::Let(variable, expr)))
}

pub fn parse_toplevel(tokens: &mut &[Token<'_>]) -> Result<Vec<Statement>, String> {
    let mut res = vec![];

    while !tokens.is_empty() {
        res.push(parse_statement(tokens)?);
    }

    Ok(res)
}

fn lex_from<'a>(s: &'a str, offset: usize) -> Result<Vec<Token<'a>>, String> {
    let integer_re = Regex::new(r"^[0-9]+").unwrap();
    let string_re = Regex::new(r##"^"[^"]*""##).unwrap();
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();

    let mut res: Vec<(usize, &str)> = vec![];

    let mut offset = offset;
    'outer: while offset < s.len() {
        let s = &s[offset..];

        // Skip over whitespace.
        if let Some(first_char) = s.chars().next() {
            if first_char.is_whitespace() {
                offset += 1;
                continue;
            }
        } else {
            break;
        }

        for token_char in ['+', '(', ')', '{', '}', ';', '=', ','] {
            if s.starts_with(token_char) {
                res.push((offset, &s[0..1]));
                offset += 1;
                continue 'outer;
            }
        }
        if let Some(integer_match) = integer_re.find(s) {
            res.push((offset, integer_match.as_str()));
            offset += integer_match.end();
        } else if let Some(string_match) = string_re.find(s) {
            res.push((offset, string_match.as_str()));
            offset += string_match.end();
        } else if let Some(variable_match) = variable_re.find(s) {
            res.push((offset, variable_match.as_str()));
            offset += variable_match.end();
        } else {
            break;
        }
    }

    if offset != s.len() {
        return Err(format!("Unrecognized syntax: '{}'", &s[offset..]));
    }

    Ok(res)
}

pub fn lex<'a>(s: &'a str) -> Result<Vec<Token<'a>>, String> {
    lex_from(s, 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_no_offset() {
        assert_eq!(lex("1").unwrap(), vec![(0, "1")]);
    }

    #[test]
    fn test_lex_with_offset() {
        assert_eq!(lex(" a").unwrap(), vec![(1, "a")]);
    }

    // #[test]
    // fn test_lex_spaces() {
    //     assert_eq!(lex("1 + 2").unwrap(), vec!["1", "+", "2"]);
    // }

    // #[test]
    // fn test_lex_no_spaces() {
    //     assert_eq!(lex("1+2").unwrap(), vec!["1", "+", "2"]);
    // }

    #[test]
    fn test_parse_bool_literal() {
        let src = "true;";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::Expr(Expression(0, Expression_::BoolLiteral(true)))
            )]
        );
    }

    #[test]
    fn test_parse_variable() {
        let src = "abc_def;";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::Expr(Expression(0, Expression_::Variable("abc_def".to_string())))
            )]
        );
    }
}
