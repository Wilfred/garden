use lazy_static::lazy_static;
use regex::Regex;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Position {
//     line: usize,
// }

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableName(pub String);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression_ {
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    Variable(VariableName),
    Call(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub usize, pub Expression_);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement_ {
    // TODO: is Statement the best place for Fun?
    Fun(VariableName, Vec<VariableName>, Vec<Statement>),
    If(Box<Expression>, Vec<Statement>, Vec<Statement>),
    While(Box<Expression>, Vec<Statement>),
    Assign(VariableName, Box<Expression>),
    Let(VariableName, Box<Expression>),
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

fn next_token_is(tokens: &[Token<'_>], token: &str) -> bool {
    tokens.first().map(|t| t.1 == token).unwrap_or(false)
}

fn peek_two_tokens<'a>(tokens: &[Token<'a>]) -> Option<(Token<'a>, Token<'a>)> {
    if tokens.len() > 1 {
        Some((tokens[0], tokens[1]))
    } else {
        None
    }
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

fn parse_block(tokens: &mut &[Token<'_>]) -> Result<Vec<Statement>, String> {
    let mut res = vec![];

    require_token(tokens, "{")?;

    while !tokens.is_empty() {
        if next_token_is(tokens, "}") {
            break;
        }

        res.push(parse_statement(tokens)?);
    }

    require_token(tokens, "}")?;

    Ok(res)
}

fn parse_if_stmt(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let offset = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_expression(tokens)?;
    require_token(tokens, ")")?;

    let then_body = parse_block(tokens)?;

    let else_body = if next_token_is(tokens, "else") {
        pop_token(tokens);

        if next_token_is(tokens, "if") {
            vec![parse_if_stmt(tokens)?]
        } else {
            parse_block(tokens)?
        }
    } else {
        vec![]
    };

    Ok(Statement(
        offset,
        Statement_::If(Box::new(condition), then_body, else_body),
    ))
}

fn parse_while_stmt(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let offset = require_token(tokens, "while")?;

    require_token(tokens, "(")?;
    let condition = parse_expression(tokens)?;
    require_token(tokens, ")")?;

    let body = parse_block(tokens)?;

    Ok(Statement(
        offset,
        Statement_::While(Box::new(condition), body),
    ))
}

fn parse_simple_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    if let Some((offset, token)) = peek_token(tokens) {
        if token == "(" {
            return parse_parenthesis_expression(tokens);
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

        if INTEGER_RE.is_match(token) {
            return parse_integer(tokens);
        }

        return Err(format!("Expected an expression, got: {}", token));
    }

    Err("Expected an expression".to_owned())
}

fn parse_call_arguments(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, String> {
    require_token(tokens, "(")?;

    let mut args = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
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

    if next_token_is(tokens, "(") {
        let arguments = parse_call_arguments(tokens)?;
        return Ok(Expression(
            expr.0,
            Expression_::Call(Box::new(expr), arguments),
        ));
    }

    Ok(expr)
}

fn token_as_binary_op(token: &str) -> Option<BinaryOperatorKind> {
    match token {
        "+" => Some(BinaryOperatorKind::Add),
        "-" => Some(BinaryOperatorKind::Subtract),
        "*" => Some(BinaryOperatorKind::Multiply),
        "/" => Some(BinaryOperatorKind::Divide),
        "==" => Some(BinaryOperatorKind::Equal),
        "!=" => Some(BinaryOperatorKind::NotEqual),
        "&&" => Some(BinaryOperatorKind::And),
        "||" => Some(BinaryOperatorKind::Or),
        "<" => Some(BinaryOperatorKind::LessThan),
        ">" => Some(BinaryOperatorKind::GreaterThan),
        _ => None,
    }
}

fn parse_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, String> {
    let mut expr = parse_simple_expression_or_call(tokens)?;

    if let Some((_, token)) = peek_token(tokens) {
        if let Some(op) = token_as_binary_op(token) {
            pop_token(tokens);

            let rhs_expr = parse_simple_expression_or_call(tokens)?;
            expr = Expression(
                expr.0,
                Expression_::BinaryOperator(Box::new(expr), op, Box::new(rhs_expr)),
            );
        }
    }

    Ok(expr)
}

fn parse_statement(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    if let Some((_, (_, token))) = peek_two_tokens(tokens) {
        if token == "=" {
            return parse_assign_stmt(tokens);
        }
    }

    if let Some((_, token)) = peek_token(tokens) {
        if token == "fun" {
            return parse_function(tokens);
        }
        if token == "let" {
            return parse_let_stmt(tokens);
        }
        if token == "if" {
            return parse_if_stmt(tokens);
        }
        if token == "while" {
            return parse_while_stmt(tokens);
        }
    }

    let expr = parse_expression(tokens)?;
    require_token(tokens, ";")?;
    Ok(Statement(expr.0, Statement_::Expr(expr)))
}

fn parse_function_params(tokens: &mut &[Token<'_>]) -> Result<Vec<VariableName>, String> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
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

const RESERVED_WORDS: &[&str] = &["let", "fun", "true", "false", "if", "else", "while"];

fn parse_variable_name(tokens: &mut &[Token<'_>]) -> Result<(usize, VariableName), String> {
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

    Ok((offset, VariableName(variable.to_string())))
}

fn parse_let_stmt(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let offset = require_token(tokens, "let")?;
    let (_, variable) = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Statement(offset, Statement_::Let(variable, Box::new(expr))))
}

fn parse_assign_stmt(tokens: &mut &[Token<'_>]) -> Result<Statement, String> {
    let (offset, variable) = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Statement(
        offset,
        Statement_::Assign(variable, Box::new(expr)),
    ))
}

pub fn parse_toplevel_from_str(s: &str) -> Result<Vec<Statement>, String> {
    let tokens = lex(s)?;
    let mut token_ptr = &tokens[..];
    parse_toplevel(&mut token_ptr)
}

fn parse_toplevel(tokens: &mut &[Token<'_>]) -> Result<Vec<Statement>, String> {
    let mut res = vec![];

    while !tokens.is_empty() {
        res.push(parse_statement(tokens)?);
    }

    Ok(res)
}

lazy_static! {
    static ref INTEGER_RE: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref STRING_RE: Regex = Regex::new(r##"^"[^"]*""##).unwrap();
    static ref VARIABLE_RE: Regex = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();
}

fn lex_from<'a>(s: &'a str, offset: usize) -> Result<Vec<Token<'a>>, String> {
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

        for token_str in ["==", "!=", "&&", "||"] {
            if s.starts_with(token_str) {
                res.push((offset, &s[0..token_str.len()]));
                offset += token_str.len();
                continue 'outer;
            }
        }
        for token_char in [
            '+', '-', '*', '/', '(', ')', '{', '}', ';', '=', ',', '<', '>',
        ] {
            if s.starts_with(token_char) {
                res.push((offset, &s[0..1]));
                offset += 1;
                continue 'outer;
            }
        }
        if let Some(integer_match) = INTEGER_RE.find(s) {
            res.push((offset, integer_match.as_str()));
            offset += integer_match.end();
        } else if let Some(string_match) = STRING_RE.find(s) {
            res.push((offset, string_match.as_str()));
            offset += string_match.end();
        } else if let Some(variable_match) = VARIABLE_RE.find(s) {
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

fn lex<'a>(s: &'a str) -> Result<Vec<Token<'a>>, String> {
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

    #[test]
    fn test_lex_spaces() {
        assert_eq!(
            lex("1 + 2")
                .unwrap()
                .iter()
                .map(|token| token.1)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

    #[test]
    fn test_lex_no_spaces() {
        assert_eq!(
            lex("1+2")
                .unwrap()
                .iter()
                .map(|token| token.1)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

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
                Statement_::Expr(Expression(
                    0,
                    Expression_::Variable(VariableName("abc_def".to_string()))
                ))
            )]
        );
    }

    #[test]
    fn test_parse_let() {
        let src = "let x = 1;";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::Let(
                    VariableName("x".into()),
                    Box::new(Expression(8, Expression_::IntLiteral(1)))
                )
            )]
        );
    }

    #[test]
    fn test_parse_if_else() {
        let src = "if (true) {} else {}";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::If(
                    Box::new(Expression(4, Expression_::BoolLiteral(true))),
                    vec![],
                    vec![],
                )
            )]
        );
    }

    #[test]
    fn test_parse_else_if() {
        let src = "if (x) {} else if (y) {}";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::If(
                    Box::new(Expression(
                        4,
                        Expression_::Variable(VariableName("x".into()))
                    )),
                    vec![],
                    vec![Statement(
                        15,
                        Statement_::If(
                            Box::new(Expression(
                                19,
                                Expression_::Variable(VariableName("y".into()))
                            )),
                            vec![],
                            vec![],
                        )
                    )],
                )
            )]
        );
    }

    #[test]
    fn test_parse_if() {
        let src = "if (true) {}";
        let tokens = lex(src).unwrap();
        let mut token_ptr = &tokens[..];
        let ast = parse_toplevel(&mut token_ptr).unwrap();

        assert_eq!(
            ast,
            vec![Statement(
                0,
                Statement_::If(
                    Box::new(Expression(4, Expression_::BoolLiteral(true))),
                    vec![],
                    vec![],
                )
            )]
        );
    }
}
