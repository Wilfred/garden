use std::path::PathBuf;

use lazy_static::lazy_static;
use regex::Regex;

use crate::eval::ErrorKind;

/// A position is an offset into source code.
#[derive(Debug, Clone, PartialEq)]
// TODO: consider just storing a pointer to the path.
pub struct Position(pub usize, pub PathBuf);

#[derive(Debug)]
pub enum ParseError {
    OtherError(Position, String),
    Incomplete(String),
}

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
    If(Box<Expression>, Vec<Expression>, Vec<Expression>),
    While(Box<Expression>, Vec<Expression>),
    Assign(VariableName, Box<Expression>),
    Let(VariableName, Box<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    ListLiteral(Vec<Expression>),
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    Variable(VariableName),
    Call(Box<Expression>, Vec<Expression>),
    Stop(Option<ErrorKind>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub Position, pub Expression_);

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    // TODO: define a FunDetails struct.
    Fun(
        Option<String>,
        VariableName,
        Vec<VariableName>,
        Vec<Expression>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition(pub Position, pub Definition_);

#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionsOrExpression {
    Defs(Vec<Definition>),
    Expr(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    offset: Position,
    text: &'a str,
    preceding_comments: Vec<&'a str>,
}

fn pop_token<'a>(tokens: &mut &[Token<'a>]) -> Option<Token<'a>> {
    if tokens.is_empty() {
        return None;
    }

    let token = tokens[0].clone();
    *tokens = &tokens[1..];
    Some(token)
}

fn peek_token<'a>(tokens: &[Token<'a>]) -> Option<Token<'a>> {
    tokens.first().map(|t| t.clone())
}

fn next_token_is(tokens: &[Token<'_>], token: &str) -> bool {
    tokens.first().map(|t| t.text == token).unwrap_or(false)
}

fn peek_two_tokens<'a>(tokens: &[Token<'a>]) -> Option<(Token<'a>, Token<'a>)> {
    if tokens.len() > 1 {
        Some((tokens[0].clone(), tokens[1].clone()))
    } else {
        None
    }
}

fn require_a_token<'a>(
    tokens: &mut &[Token<'a>],
    token_description: &str,
) -> Result<Token<'a>, ParseError> {
    match pop_token(tokens) {
        Some(token) => Ok(token),
        None => Err(ParseError::Incomplete(format!(
            "Expected {}, got EOF",
            token_description
        ))),
    }
}

fn require_token<'a>(tokens: &mut &[Token<'a>], expected: &str) -> Result<Token<'a>, ParseError> {
    match pop_token(tokens) {
        Some(token) => {
            if token.text == expected {
                Ok(token)
            } else {
                Err(ParseError::OtherError(
                    token.offset,
                    format!("Expected `{}`, got `{}`", expected, token.text),
                ))
            }
        }
        None => Err(ParseError::Incomplete(format!(
            "Expected `{}`, got EOF",
            expected
        ))),
    }
}

fn parse_integer(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let re = Regex::new(r"^[0-9]+$").unwrap();

    let token = require_a_token(tokens, "integer literal")?;
    if re.is_match(token.text) {
        let i: i64 = token.text.parse().unwrap();
        Ok(Expression(token.offset, Expression_::IntLiteral(i)))
    } else {
        Err(ParseError::OtherError(
            token.offset,
            format!("Not a valid integer literal: {}", token.text),
        ))
    }
}

fn parse_variable_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let (offset, variable) = parse_variable_name(tokens)?;
    Ok(Expression(offset, Expression_::Variable(variable)))
}

fn parse_parenthesis_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    require_token(tokens, "(")?;
    let expr = parse_inline_expression(tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_list_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let open_brace = require_token(tokens, "[")?;

    let mut items = vec![];
    loop {
        if next_token_is(tokens, "]") {
            break;
        }

        let item = parse_inline_expression(tokens)?;
        items.push(item);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == "]" {
                break;
            } else {
                return Err(ParseError::OtherError(
                    token.offset,
                    format!(
                        "Invalid syntax: Expected `,` or `]` here, but got `{}`",
                        token.text
                    ),
                ));
            }
        } else {
            return Err(ParseError::Incomplete(
                "Invalid syntax: Expected `,` or `]` here, but got EOF".to_string(),
            ));
        }
    }

    require_token(tokens, "]")?;

    Ok(Expression(
        open_brace.offset,
        Expression_::ListLiteral(items),
    ))
}

fn parse_block_expressions(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
    let mut res = vec![];

    require_token(tokens, "{")?;

    while !tokens.is_empty() {
        if next_token_is(tokens, "}") {
            break;
        }

        res.push(parse_block_expression(tokens)?);
    }

    require_token(tokens, "}")?;

    Ok(res)
}

fn parse_if_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let if_token = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(tokens)?;
    require_token(tokens, ")")?;

    let then_body = parse_block_expressions(tokens)?;

    let else_body = if next_token_is(tokens, "else") {
        pop_token(tokens);

        if next_token_is(tokens, "if") {
            vec![parse_if_expression(tokens)?]
        } else {
            parse_block_expressions(tokens)?
        }
    } else {
        vec![]
    };

    Ok(Expression(
        if_token.offset,
        Expression_::If(Box::new(condition), then_body, else_body),
    ))
}

fn parse_while_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let while_token = require_token(tokens, "while")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(tokens)?;
    require_token(tokens, ")")?;

    let body = parse_block_expressions(tokens)?;

    Ok(Expression(
        while_token.offset,
        Expression_::While(Box::new(condition), body),
    ))
}

fn parse_return_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let return_token = require_token(tokens, "return")?;

    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;
    Ok(Expression(
        return_token.offset,
        Expression_::Return(Box::new(expr)),
    ))
}

fn unescape_string(s: &str) -> String {
    // Trim doublequotes.
    let s = &s[1..s.len() - 1];

    let mut res = String::with_capacity(s.len());

    let mut i = 0;
    let chars: Vec<_> = s.chars().collect();
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' {
            match chars.get(i + 1) {
                Some('n') => {
                    res.push('\n');
                    i += 2;
                }
                Some('\\') => {
                    res.push('\\');
                    i += 2;
                }
                Some('"') => {
                    res.push('"');
                    i += 2;
                }
                _ => {
                    // TODO: an invalid escape sequence such as \z
                    // should be a parse error.
                    res.push(c);

                    i += 1;
                }
            }
        } else {
            res.push(c);
            i += 1;
        }
    }

    res
}

fn parse_simple_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "(" {
            return parse_parenthesis_expression(tokens);
        }

        if token.text == "[" {
            return parse_list_expression(tokens);
        }

        if token.text == "true" {
            pop_token(tokens);
            return Ok(Expression(token.offset, Expression_::BoolLiteral(true)));
        }
        if token.text == "false" {
            pop_token(tokens);
            return Ok(Expression(token.offset, Expression_::BoolLiteral(false)));
        }

        let re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();
        if re.is_match(token.text) {
            return parse_variable_expression(tokens);
        }

        if token.text.starts_with("\"") {
            pop_token(tokens);
            return Ok(Expression(
                token.offset,
                Expression_::StringLiteral(unescape_string(token.text)),
            ));
        }

        if INTEGER_RE.is_match(token.text) {
            return parse_integer(tokens);
        }

        return Err(ParseError::OtherError(
            token.offset.clone(),
            format!(
                "Expected an expression, got: {} (offset {})",
                token.text, token.offset.0
            ),
        ));
    }

    Err(ParseError::Incomplete("Expected an expression".to_owned()))
}

fn parse_call_arguments(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
    require_token(tokens, "(")?;

    let mut args = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
        }

        let arg = parse_inline_expression(tokens)?;
        args.push(arg);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == ")" {
                break;
            } else {
                return Err(ParseError::OtherError(
                    token.offset,
                    format!(
                        "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                        token.text
                    ),
                ));
            }
        } else {
            return Err(ParseError::Incomplete(
                "Invalid syntax: Expected `,` or `)` here, but got EOF".to_string(),
            ));
        }
    }

    require_token(tokens, ")")?;
    Ok(args)
}

fn parse_simple_expression_or_call(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let expr = parse_simple_expression(tokens)?;

    if next_token_is(tokens, "(") {
        let arguments = parse_call_arguments(tokens)?;
        return Ok(Expression(
            expr.0.clone(),
            Expression_::Call(Box::new(expr), arguments),
        ));
    }

    Ok(expr)
}

fn token_as_binary_op(token: Token<'_>) -> Option<BinaryOperatorKind> {
    match token.text {
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

fn parse_inline_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    parse_general_expression(tokens, true)
}

fn parse_block_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    parse_general_expression(tokens, false)
}

fn parse_general_expression(
    tokens: &mut &[Token<'_>],
    is_inline: bool,
) -> Result<Expression, ParseError> {
    if !is_inline {
        // TODO: Matching on tokens will prevent us from doing more
        // complex assignments like `foo.bar = 1;`.
        if let Some((_, token)) = peek_two_tokens(tokens) {
            if token.text == "=" {
                return parse_assign_expression(tokens);
            }
        }

        if let Some(token) = peek_token(tokens) {
            if token.text == "let" {
                return parse_let_expression(tokens);
            }
            if token.text == "return" {
                return parse_return_expression(tokens);
            }
            if token.text == "while" {
                return parse_while_expression(tokens);
            }
        }
    }

    if let Some(token) = peek_token(tokens) {
        if token.text == "if" {
            return parse_if_expression(tokens);
        }
    }

    let expr = parse_simple_expression_or_binop(tokens)?;
    if !is_inline {
        let _ = require_token(tokens, ";")?;
    }

    Ok(expr)
}

fn parse_simple_expression_or_binop(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression_or_call(tokens)?;

    if let Some(token) = peek_token(tokens) {
        if let Some(op) = token_as_binary_op(token) {
            pop_token(tokens);

            let rhs_expr = parse_simple_expression_or_call(tokens)?;
            expr = Expression(
                expr.0.clone(),
                Expression_::BinaryOperator(Box::new(expr), op, Box::new(rhs_expr)),
            );
        }
    }

    Ok(expr)
}

fn parse_definition(path: &PathBuf, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "fun" {
            return parse_function(tokens);
        }

        // TODO: Include the token in the error message.
        return Err(ParseError::OtherError(
            token.offset,
            "Expected a definition".to_string(),
        ));
    }

    // TODO: return a more meaningful position (e.g. EOF)
    Err(ParseError::OtherError(
        Position(0, path.clone()),
        "Expected a definition, got EOF".to_string(),
    ))
}

fn parse_function_params(tokens: &mut &[Token<'_>]) -> Result<Vec<VariableName>, ParseError> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
        }

        let (_, param) = parse_variable_name(tokens)?;
        params.push(param);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == ")" {
                break;
            } else {
                return Err(ParseError::OtherError(
                    token.offset,
                    format!(
                        "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                        token.text
                    ),
                ));
            }
        } else {
            return Err(ParseError::Incomplete(
                "Invalid syntax: Expected `,` or `)` here, but got EOF".to_string(),
            ));
        }
    }

    require_token(tokens, ")")?;
    Ok(params)
}

fn parse_function_body(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
    require_token(tokens, "{")?;

    let mut exprs = vec![];
    loop {
        if let Some(token) = peek_token(tokens) {
            if token.text == "}" {
                break;
            }
        } else {
            return Err(ParseError::Incomplete(
                "Invalid syntax: Expected `}}` here, but got EOF".to_string(),
            ));
        }

        let expr = parse_block_expression(tokens)?;
        exprs.push(expr);
    }

    require_token(tokens, "}")?;
    Ok(exprs)
}

fn join_comments(comments: &[&str]) -> String {
    let mut comment_texts = comments
        .iter()
        .map(|comment| comment.strip_prefix(" ").unwrap_or(comment))
        .collect::<Vec<_>>();

    if let Some(comment_text) = comment_texts.last_mut() {
        *comment_text = comment_text.strip_suffix("\n").unwrap_or(&comment_text)
    }

    comment_texts.join("")
}

fn parse_function(tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    let fun_token = require_token(tokens, "fun")?;
    let mut doc_comment = None;
    if !fun_token.preceding_comments.is_empty() {
        doc_comment = Some(join_comments(&fun_token.preceding_comments));
    }

    let (_, name) = parse_variable_name(tokens)?;
    let params = parse_function_params(tokens)?;
    let body = parse_function_body(tokens)?;

    Ok(Definition(
        fun_token.offset,
        Definition_::Fun(doc_comment, name, params, body),
    ))
}

const RESERVED_WORDS: &[&str] = &[
    "let", "fun", "true", "false", "if", "else", "while", "return",
];

fn parse_variable_name(tokens: &mut &[Token<'_>]) -> Result<(Position, VariableName), ParseError> {
    // TODO: this is duplicated with lex().
    let variable_re = Regex::new(r"^[a-z_][a-z0-9_]*$").unwrap();

    let variable_token = require_a_token(tokens, "variable name")?;
    if !variable_re.is_match(variable_token.text) {
        return Err(ParseError::OtherError(
            variable_token.offset,
            format!("Invalid variable name: '{}'", variable_token.text),
        ));
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            return Err(ParseError::OtherError(
                variable_token.offset,
                format!(
                    "'{}' is a reserved word that cannot be used as a variable",
                    variable_token.text
                ),
            ));
        }
    }

    Ok((
        variable_token.offset,
        VariableName(variable_token.text.to_string()),
    ))
}

fn parse_let_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let let_token = require_token(tokens, "let")?;
    let (_, variable) = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        let_token.offset,
        Expression_::Let(variable, Box::new(expr)),
    ))
}

fn parse_assign_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let (offset, variable) = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        offset,
        Expression_::Assign(variable, Box::new(expr)),
    ))
}

fn parse_def_or_expr(
    path: &PathBuf,
    tokens: &mut &[Token<'_>],
) -> Result<DefinitionsOrExpression, ParseError> {
    // Parsing advances the tokens pointer, so create a copy for
    // trying an expression parse.
    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_inline_expression(&mut tokens_copy) {
        return Ok(DefinitionsOrExpression::Expr(expr));
    }

    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_block_expression(&mut tokens_copy) {
        return Ok(DefinitionsOrExpression::Expr(expr));
    }

    let mut defs = vec![];
    while !tokens.is_empty() {
        defs.push(parse_definition(path, tokens)?);
    }

    Ok(DefinitionsOrExpression::Defs(defs))
}

pub fn parse_def_or_expr_from_str(
    path: &PathBuf,
    s: &str,
) -> Result<DefinitionsOrExpression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_def_or_expr(path, &mut token_ptr)
}

pub fn parse_inline_expr_from_str(path: &PathBuf, s: &str) -> Result<Expression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_inline_expression(&mut token_ptr)
}

lazy_static! {
    static ref INTEGER_RE: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref STRING_RE: Regex = Regex::new(r##"^"(\\"|[^"])*""##).unwrap();
    static ref VARIABLE_RE: Regex = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();
}

fn lex_from<'a>(path: &PathBuf, s: &'a str, offset: usize) -> Result<Vec<Token<'a>>, ParseError> {
    let mut res: Vec<Token<'a>> = vec![];

    let mut preceding_comments = vec![];
    let mut offset = offset;
    'outer: while offset < s.len() {
        let s = &s[offset..];

        // Skip over comments.
        if s.starts_with("//") {
            if let Some(i) = s.find("\n") {
                preceding_comments.push(&s["//".len()..i + 1]);
                offset += i + 1;
                continue;
            } else {
                offset += s.len();
                break;
            }
        }

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
                res.push(Token {
                    offset: Position(offset, path.clone()),
                    text: &s[0..token_str.len()],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += token_str.len();
                continue 'outer;
            }
        }
        for token_char in [
            '+', '-', '*', '/', '(', ')', '{', '}', ';', '=', ',', '<', '>', '[', ']',
        ] {
            if s.starts_with(token_char) {
                res.push(Token {
                    offset: Position(offset, path.clone()),
                    text: &s[0..1],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += 1;
                continue 'outer;
            }
        }
        if let Some(integer_match) = INTEGER_RE.find(s) {
            res.push(Token {
                offset: Position(offset, path.clone()),
                text: integer_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += integer_match.end();
        } else if let Some(string_match) = STRING_RE.find(s) {
            res.push(Token {
                offset: Position(offset, path.clone()),
                text: string_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += string_match.end();
        } else if let Some(variable_match) = VARIABLE_RE.find(s) {
            res.push(Token {
                offset: Position(offset, path.clone()),
                text: variable_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += variable_match.end();
        } else {
            break;
        }
    }

    if offset != s.len() {
        return Err(ParseError::OtherError(
            Position(offset, path.clone()),
            format!("Unrecognized syntax: '{}'", &s[offset..]),
        ));
    }

    Ok(res)
}

fn lex<'a>(path: &PathBuf, s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
    lex_from(path, s, 0)
}

#[cfg(test)]
pub fn parse_exprs_from_str(s: &str) -> Result<Vec<Expression>, ParseError> {
    let tokens = lex(&PathBuf::from("__test.gdn"), s)?;
    let mut token_ptr = &tokens[..];

    let mut res = vec![];
    while !token_ptr.is_empty() {
        res.push(parse_block_expression(&mut token_ptr)?);
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_no_offset() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "1").unwrap(),
            vec![Token {
                offset: Position(0, PathBuf::from("__test.gdn")),
                text: "1",
                preceding_comments: vec![],
            }]
        );
    }

    #[test]
    fn test_lex_with_offset() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), " a").unwrap(),
            vec![Token {
                offset: Position(1, PathBuf::from("__test.gdn")),
                text: "a",
                preceding_comments: vec![],
            }]
        );
    }

    #[test]
    fn test_lex_spaces() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "1 + 2")
                .unwrap()
                .iter()
                .map(|token| token.text)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

    #[test]
    fn test_lex_no_spaces() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "1+2")
                .unwrap()
                .iter()
                .map(|token| token.text)
                .collect::<Vec<_>>(),
            vec!["1", "+", "2"]
        );
    }

    #[test]
    fn test_parse_bool_literal() {
        let ast = parse_exprs_from_str("true;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::BoolLiteral(true)
            )]
        );
    }

    #[test]
    fn test_parse_string_literal() {
        let ast = parse_exprs_from_str("\"a\\nb\\\\c\\\"d\";").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::StringLiteral("a\nb\\c\"d".into())
            )]
        );
    }

    #[test]
    fn test_lex_comment() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "// 2\n1").unwrap(),
            vec![Token {
                offset: Position(5, PathBuf::from("__test.gdn"),),
                text: "1",
                preceding_comments: vec![" 2\n"],
            }]
        );
    }

    #[test]
    fn test_lex_comment_leading_newline() {
        assert_eq!(lex(&PathBuf::from("__test.gdn"), "\n// 2").unwrap(), vec![]);
    }

    #[test]
    fn test_lex_standalone_comment() {
        assert_eq!(lex(&PathBuf::from("__test.gdn"), "// foo").unwrap(), vec![]);
    }

    #[test]
    fn test_parse_variable() {
        let ast = parse_exprs_from_str("abc_def;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::Variable(VariableName("abc_def".to_string()))
            )]
        );
    }

    #[test]
    fn test_parse_let() {
        let ast = parse_exprs_from_str("let x = 1;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::Let(
                    VariableName("x".into()),
                    Box::new(Expression(
                        Position(8, PathBuf::from("__test.gdn")),
                        Expression_::IntLiteral(1)
                    ))
                )
            )]
        );
    }

    #[test]
    fn test_parse_if_else() {
        let ast = parse_exprs_from_str("if (true) {} else {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::If(
                    Box::new(Expression(
                        Position(4, PathBuf::from("__test.gdn")),
                        Expression_::BoolLiteral(true)
                    )),
                    vec![],
                    vec![],
                )
            )]
        );
    }

    #[test]
    fn test_parse_else_if() {
        let ast = parse_exprs_from_str("if (x) {} else if (y) {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::If(
                    Box::new(Expression(
                        Position(4, PathBuf::from("__test.gdn")),
                        Expression_::Variable(VariableName("x".into()))
                    )),
                    vec![],
                    vec![Expression(
                        Position(15, PathBuf::from("__test.gdn")),
                        Expression_::If(
                            Box::new(Expression(
                                Position(19, PathBuf::from("__test.gdn")),
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
        let ast = parse_exprs_from_str("if (true) {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::If(
                    Box::new(Expression(
                        Position(4, PathBuf::from("__test.gdn")),
                        Expression_::BoolLiteral(true)
                    )),
                    vec![],
                    vec![],
                )
            )]
        );
    }

    #[test]
    fn test_parse_return() {
        let ast = parse_exprs_from_str("return true;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::Return(Box::new(Expression(
                    Position(7, PathBuf::from("__test.gdn")),
                    Expression_::BoolLiteral(true)
                )))
            )]
        );
    }

    #[test]
    fn test_parse_function() {
        let ast = match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "// Hello\n// World\nfun foo() {}").unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };

        assert_eq!(
            ast,
            vec![Definition(
                Position(18, PathBuf::from("__test.gdn")),
                Definition_::Fun(
                    Some("Hello\nWorld".into()),
                    VariableName("foo".into()),
                    vec![],
                    vec![]
                )
            )]
        );
    }

    #[test]
    fn test_parse_block_expression() {
        let ast = match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "let x = 1;").unwrap() {
            DefinitionsOrExpression::Defs(_) => unreachable!(),
            DefinitionsOrExpression::Expr(e) => e,
        };

        assert_eq!(
            ast,
            Expression(
                Position(0, PathBuf::from("__test.gdn")),
                Expression_::Let(
                    VariableName("x".into()),
                    Box::new(Expression(
                        Position(8, PathBuf::from("__test.gdn")),
                        Expression_::IntLiteral(1)
                    ))
                )
            )
        );
    }
}
