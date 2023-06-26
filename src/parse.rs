use std::path::{Path, PathBuf};

use ariadne::{Config, Label, Report, ReportKind, Source};
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::eval::ErrorKind;

/// A position is an offset into source code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub offset: usize,
    pub end_offset: usize,
    // TODO: consider storing a &Path to reduce memory usage.
    pub path: PathBuf,
}

pub fn format_error(message: &str, position: &Position, src: &str) -> String {
    let mut res = Vec::new();

    let path_str = position.path.display().to_string();
    let r = Report::build(ReportKind::Error, &path_str, position.offset)
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((&path_str, position.offset..position.end_offset)).with_message(message),
        )
        .finish();

    r.write((&path_str, Source::from(src)), &mut res).unwrap();
    String::from_utf8_lossy(&res).to_string()
}

#[derive(Debug)]
pub enum ParseError {
    Invalid(Position, String),
    Incomplete(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableName(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct Variable(pub Position, pub VariableName);

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
    Assign(Variable, Box<Expression>),
    Let(Variable, Box<Expression>),
    Return(Box<Expression>),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    ListLiteral(Vec<Expression>),
    BinaryOperator(Box<Expression>, BinaryOperatorKind, Box<Expression>),
    Variable(Variable),
    Call(Box<Expression>, Vec<Expression>),
    Lambda(Vec<Variable>, Vec<Expression>),
    Stop(Option<ErrorKind>),
    Block(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub Position, pub Expression_);

#[derive(Debug, Clone, PartialEq)]
pub struct FunInfo {
    pub doc_comment: Option<String>,
    pub name: Variable,
    pub params: Vec<Variable>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition_ {
    Fun(FunInfo),
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
    position: Position,
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
    tokens.first().cloned()
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
                Err(ParseError::Invalid(
                    token.position,
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
    let token = require_a_token(tokens, "integer literal")?;
    if INTEGER_RE.is_match(token.text) {
        let i: i64 = token.text.parse().unwrap();
        Ok(Expression(token.position, Expression_::IntLiteral(i)))
    } else {
        Err(ParseError::Invalid(
            token.position,
            format!("Not a valid integer literal: {}", token.text),
        ))
    }
}

fn parse_variable_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let variable = parse_variable_name(tokens)?;
    Ok(Expression(
        variable.0.clone(),
        Expression_::Variable(variable),
    ))
}

fn parse_parenthesis_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    require_token(tokens, "(")?;
    let expr = parse_inline_expression(tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_list_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let open_brace = require_token(tokens, "[")?;
    let items = parse_comma_separated_exprs(tokens, "]")?;
    require_token(tokens, "]")?;

    Ok(Expression(
        open_brace.position,
        Expression_::ListLiteral(items),
    ))
}

fn parse_lambda_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let fun_keyword = require_token(tokens, "fun")?;

    let params = parse_function_params(tokens)?;
    let body = parse_block_expressions(tokens)?;

    Ok(Expression(
        fun_keyword.position,
        Expression_::Lambda(params, body),
    ))
}

fn parse_block_expressions(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
    let mut res = vec![];

    require_token(tokens, "{")?;

    while !tokens.is_empty() {
        if next_token_is(tokens, "}") {
            break;
        }

        res.push(parse_block_member_expression(tokens)?);
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
        if_token.position,
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
        while_token.position,
        Expression_::While(Box::new(condition), body),
    ))
}

fn parse_return_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let return_token = require_token(tokens, "return")?;

    // TODO: allow `return;`
    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;
    Ok(Expression(
        return_token.position,
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
        if token.text == "{" {
            let exprs = parse_block(tokens)?;
            return Ok(Expression(token.position, Expression_::Block(exprs)));
        }

        if token.text == "(" {
            return parse_parenthesis_expression(tokens);
        }

        if token.text == "[" {
            return parse_list_expression(tokens);
        }

        if token.text == "fun" {
            return parse_lambda_expression(tokens);
        }

        if token.text == "true" {
            pop_token(tokens);
            return Ok(Expression(token.position, Expression_::BoolLiteral(true)));
        }
        if token.text == "false" {
            pop_token(tokens);
            return Ok(Expression(token.position, Expression_::BoolLiteral(false)));
        }

        if VARIABLE_RE.is_match(token.text) {
            return parse_variable_expression(tokens);
        }

        if token.text.starts_with("\"") {
            pop_token(tokens);
            return Ok(Expression(
                token.position,
                Expression_::StringLiteral(unescape_string(token.text)),
            ));
        }

        if INTEGER_RE.is_match(token.text) {
            return parse_integer(tokens);
        }

        return Err(ParseError::Invalid(
            token.position.clone(),
            format!(
                "Expected an expression, got: {} (offset {})",
                token.text, token.position.offset
            ),
        ));
    }

    Err(ParseError::Incomplete("Expected an expression".to_owned()))
}

fn parse_comma_separated_exprs(
    tokens: &mut &[Token<'_>],
    terminator: &str,
) -> Result<Vec<Expression>, ParseError> {
    let mut items = vec![];
    loop {
        if next_token_is(tokens, terminator) {
            break;
        }

        let arg = parse_inline_expression(tokens)?;
        items.push(arg);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == terminator {
                break;
            } else {
                return Err(ParseError::Invalid(
                    token.position,
                    format!(
                        "Invalid syntax: Expected `,` or `{}` here, but got `{}`",
                        terminator, token.text
                    ),
                ));
            }
        } else {
            return Err(ParseError::Incomplete(format!(
                "Invalid syntax: Expected `,` or `{}` here, but got EOF",
                terminator
            )));
        }
    }

    Ok(items)
}

fn parse_call_arguments(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
    require_token(tokens, "(")?;
    let args = parse_comma_separated_exprs(tokens, ")")?;
    require_token(tokens, ")")?;
    Ok(args)
}

/// Parse an expression, and handle trailing parentheses if present.
///
/// We handle trailing syntax separately from
/// `parse_simple_expression`, to avoid infinit recursion. This is
/// essentially left-recursion from a grammar perspective.
fn parse_simple_expression_or_call(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression(tokens)?;

    // here
    while next_token_is(tokens, "(") {
        let arguments = parse_call_arguments(tokens)?;
        expr = Expression(expr.0.clone(), Expression_::Call(Box::new(expr), arguments));
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

/// Parse an inline expression. An inline expression can occur
/// anywhere, and does not end with a semicolon.
///
/// Examples:
///
/// ```
/// foo()
/// x + 1
/// if (a) { b } else { c }
/// while (z) { foo(); }
/// ```
fn parse_inline_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    parse_general_expression(tokens, true)
}

/// Parse a block member expression. This is an expression that can
/// occur at the top level of braces, such as a let expression.
///
/// Examples:
///
/// ```
/// foo();
/// let x = y + 1;
/// if (a) { b; } else { c; }
/// while (z) { foo(); }
/// ```
fn parse_block_member_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    parse_general_expression(tokens, false)
}

/// Parse an inline or block member expression.
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

    // `if` can occur as both an inline expression and a standalone
    // expression.
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

/// In Garden, an expression can only contain a single binary
/// operation, so `x + y + z` isn't legal. Users must use parentheses,
/// e.g. `(x + y) + z`.
///
/// This ensures that every subexpression has trailing syntax that we
/// can use to show intermediate values computed during evaluation.
///
/// To ensure binary operations aren't combined, we have a separate
/// parser function that allows exactly one binary operation. This
/// also has the nice side effect of not requiring precedence logic in
/// the parser.
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

fn parse_definition(path: &Path, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "fun" {
            return parse_function(tokens);
        }

        // TODO: Include the token in the error message.
        return Err(ParseError::Invalid(
            token.position,
            "Expected a definition".to_string(),
        ));
    }

    // TODO: return a more meaningful position (e.g. EOF)
    Err(ParseError::Invalid(
        Position {
            offset: 0,
            end_offset: 0,
            path: path.into(),
        },
        "Expected a definition, got EOF".to_string(),
    ))
}

fn parse_function_params(tokens: &mut &[Token<'_>]) -> Result<Vec<Variable>, ParseError> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
        }

        let param = parse_variable_name(tokens)?;
        params.push(param);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == ")" {
                break;
            } else {
                return Err(ParseError::Invalid(
                    token.position,
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

fn parse_block(tokens: &mut &[Token<'_>]) -> Result<Vec<Expression>, ParseError> {
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

        let expr = parse_block_member_expression(tokens)?;
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

    let name = parse_variable_name(tokens)?;
    let params = parse_function_params(tokens)?;
    let body = parse_block(tokens)?;

    Ok(Definition(
        fun_token.position,
        Definition_::Fun(FunInfo {
            doc_comment,
            name,
            params,
            body,
        }),
    ))
}

const RESERVED_WORDS: &[&str] = &[
    "let", "fun", "true", "false", "if", "else", "while", "return",
];

fn parse_variable_name(tokens: &mut &[Token<'_>]) -> Result<Variable, ParseError> {
    let variable_token = require_a_token(tokens, "variable name")?;
    if !VARIABLE_RE.is_match(variable_token.text) {
        return Err(ParseError::Invalid(
            variable_token.position,
            format!("Invalid variable name: '{}'", variable_token.text),
        ));
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            return Err(ParseError::Invalid(
                variable_token.position,
                format!(
                    "'{}' is a reserved word that cannot be used as a variable",
                    variable_token.text
                ),
            ));
        }
    }

    Ok(Variable(
        variable_token.position,
        VariableName(variable_token.text.to_string()),
    ))
}

fn parse_let_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let let_token = require_token(tokens, "let")?;
    let variable = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        let_token.position, // TODO: this should be a larger span.
        Expression_::Let(variable, Box::new(expr)),
    ))
}

fn parse_assign_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let variable = parse_variable_name(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        variable.0.clone(),
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
    if let Ok(expr) = parse_block_member_expression(&mut tokens_copy) {
        if tokens_copy.is_empty() {
            return Ok(DefinitionsOrExpression::Expr(expr));
        }
    }

    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_inline_expression(&mut tokens_copy) {
        if tokens_copy.is_empty() {
            return Ok(DefinitionsOrExpression::Expr(expr));
        }
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

pub fn parse_def_or_expr_from_span(
    path: &PathBuf,
    s: &str,
    offset: usize,
    end_offset: usize,
) -> Result<DefinitionsOrExpression, ParseError> {
    let tokens = lex_between(path, s, offset, end_offset)?;
    let mut token_ptr = &tokens[..];
    parse_def_or_expr(path, &mut token_ptr)
}

pub fn parse_inline_expr_from_str(path: &PathBuf, s: &str) -> Result<Expression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_inline_expression(&mut token_ptr)
}

lazy_static! {
    static ref INTEGER_RE: Regex = Regex::new(r"^-?[0-9]+").unwrap();
    static ref STRING_RE: Regex = Regex::new(r##"^"(\\"|[^"])*""##).unwrap();
    static ref VARIABLE_RE: Regex = Regex::new(r"^[a-z_][a-z0-9_]*").unwrap();
}

fn lex_between<'a>(
    path: &PathBuf,
    s: &'a str,
    offset: usize,
    end_offset: usize,
) -> Result<Vec<Token<'a>>, ParseError> {
    assert!(end_offset <= s.len());

    let mut res: Vec<Token<'a>> = vec![];

    let mut preceding_comments = vec![];
    let mut offset = offset;

    // Skip shebang if present at the beginning of the file.
    if offset == 0 && s.starts_with("#") {
        offset = s.find('\n').unwrap_or(s.len());
    }

    'outer: while offset < end_offset {
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
                    position: Position {
                        offset,
                        end_offset: offset + token_str.len(),
                        path: path.clone(),
                    },
                    text: &s[0..token_str.len()],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += token_str.len();
                continue 'outer;
            }
        }

        // Match integers before binary operators, so -1 is treated as
        // a single integer literal, not the tokens - followed by 1.
        if let Some(integer_match) = INTEGER_RE.find(s) {
            res.push(Token {
                position: Position {
                    offset,
                    end_offset: offset + integer_match.end(),
                    path: path.clone(),
                },
                text: integer_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += integer_match.end();
            continue;
        }

        for token_char in [
            '+', '-', '*', '/', '(', ')', '{', '}', ';', '=', ',', '<', '>', '[', ']',
        ] {
            if s.starts_with(token_char) {
                res.push(Token {
                    position: Position {
                        offset,
                        end_offset: offset + 1,
                        path: path.clone(),
                    },
                    text: &s[0..1],
                    preceding_comments,
                });
                preceding_comments = vec![];

                offset += 1;
                continue 'outer;
            }
        }
        if let Some(string_match) = STRING_RE.find(s) {
            res.push(Token {
                position: Position {
                    offset,
                    end_offset: offset + string_match.end(),
                    path: path.clone(),
                },
                text: string_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += string_match.end();
        } else if let Some(variable_match) = VARIABLE_RE.find(s) {
            res.push(Token {
                position: Position {
                    offset,
                    end_offset: offset + variable_match.end(),
                    path: path.clone(),
                },
                text: variable_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += variable_match.end();
        } else {
            break;
        }
    }

    if offset != end_offset {
        return Err(ParseError::Invalid(
            Position {
                offset,
                end_offset: s.len(),
                path: path.clone(),
            },
            format!("Unrecognized syntax: '{}'", &s[offset..]),
        ));
    }

    Ok(res)
}

fn lex<'a>(path: &PathBuf, s: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
    lex_between(path, s, 0, s.len())
}

#[cfg(test)]
pub fn parse_exprs_from_str(s: &str) -> Result<Vec<Expression>, ParseError> {
    let tokens = lex(&PathBuf::from("__test.gdn"), s)?;
    let mut token_ptr = &tokens[..];

    let mut res = vec![];
    while !token_ptr.is_empty() {
        res.push(parse_block_member_expression(&mut token_ptr)?);
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
                position: Position {
                    offset: 0,
                    end_offset: 1,
                    path: PathBuf::from("__test.gdn")
                },
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
                position: Position {
                    offset: 1,
                    end_offset: 2,
                    path: PathBuf::from("__test.gdn")
                },
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
                Position {
                    offset: 0,
                    end_offset: 4,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::BoolLiteral(true)
            )]
        );
    }

    #[test]
    fn test_parse_int_literal() {
        let ast = parse_exprs_from_str("-123;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    offset: 0,
                    end_offset: 4,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::IntLiteral(-123)
            )]
        );
    }

    #[test]
    fn test_parse_string_literal() {
        let ast = parse_exprs_from_str("\"a\\nb\\\\c\\\"d\";").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    offset: 0,
                    end_offset: 12,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::StringLiteral("a\nb\\c\"d".into())
            )]
        );
    }

    #[test]
    fn test_lex_comment() {
        assert_eq!(
            lex(&PathBuf::from("__test.gdn"), "// 2\n1").unwrap(),
            vec![Token {
                position: Position {
                    offset: 5,
                    end_offset: 6,
                    path: PathBuf::from("__test.gdn")
                },
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
                Position {
                    offset: 0,
                    end_offset: 7,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Variable(Variable(
                    Position {
                        offset: 0,
                        end_offset: 7,
                        path: PathBuf::from("__test.gdn")
                    },
                    VariableName("abc_def".to_string())
                ))
            )]
        );
    }

    #[test]
    fn test_parse_let() {
        let ast = parse_exprs_from_str("let x = 1;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    offset: 0,
                    end_offset: 3,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Let(
                    Variable(
                        Position {
                            offset: 4,
                            end_offset: 5,
                            path: PathBuf::from("__test.gdn")
                        },
                        VariableName("x".into())
                    ),
                    Box::new(Expression(
                        Position {
                            offset: 8,
                            end_offset: 9,
                            path: PathBuf::from("__test.gdn")
                        },
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
                Position {
                    offset: 0,
                    end_offset: 2,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            offset: 4,
                            end_offset: 8,
                            path: PathBuf::from("__test.gdn")
                        },
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
                Position {
                    offset: 0,
                    end_offset: 2,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            offset: 4,
                            end_offset: 5,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::Variable(Variable(
                            Position {
                                offset: 4,
                                end_offset: 5,
                                path: PathBuf::from("__test.gdn")
                            },
                            VariableName("x".into())
                        ))
                    )),
                    vec![],
                    vec![Expression(
                        Position {
                            offset: 15,
                            end_offset: 17,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::If(
                            Box::new(Expression(
                                Position {
                                    offset: 19,
                                    end_offset: 20,
                                    path: PathBuf::from("__test.gdn")
                                },
                                Expression_::Variable(Variable(
                                    Position {
                                        offset: 19,
                                        end_offset: 20,
                                        path: PathBuf::from("__test.gdn")
                                    },
                                    VariableName("y".into())
                                ))
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
                Position {
                    offset: 0,
                    end_offset: 2,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            offset: 4,
                            end_offset: 8,
                            path: PathBuf::from("__test.gdn")
                        },
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
                Position {
                    offset: 0,
                    end_offset: 6,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Return(Box::new(Expression(
                    Position {
                        offset: 7,
                        end_offset: 11,
                        path: PathBuf::from("__test.gdn")
                    },
                    Expression_::BoolLiteral(true)
                )))
            )]
        );
    }

    #[test]
    fn test_parse_call_repeated() {
        let ast = parse_exprs_from_str("foo()();").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    offset: 0,
                    end_offset: 3,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Call(
                    Box::new(Expression(
                        Position {
                            offset: 0,
                            end_offset: 3,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::Call(
                            Box::new(Expression(
                                Position {
                                    offset: 0,
                                    end_offset: 3,
                                    path: PathBuf::from("__test.gdn")
                                },
                                Expression_::Variable(Variable(
                                    Position {
                                        offset: 0,
                                        end_offset: 3,
                                        path: PathBuf::from("__test.gdn")
                                    },
                                    VariableName("foo".into())
                                ))
                            )),
                            vec![]
                        )
                    )),
                    vec![]
                )
            )]
        );
    }

    #[test]
    fn test_parse_function() {
        let ast = match parse_def_or_expr_from_str(
            &PathBuf::from("__test.gdn"),
            "// Hello\n// World\nfun foo() {}",
        )
        .unwrap()
        {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };

        assert_eq!(
            ast,
            vec![Definition(
                Position {
                    offset: 18,
                    end_offset: 21,
                    path: PathBuf::from("__test.gdn")
                },
                Definition_::Fun(FunInfo {
                    doc_comment: Some("Hello\nWorld".into()),
                    name: Variable(
                        Position {
                            offset: 22,
                            end_offset: 25,
                            path: PathBuf::from("__test.gdn"),
                        },
                        VariableName("foo".into())
                    ),
                    params: vec![],
                    body: vec![]
                })
            )]
        );
    }

    #[test]
    fn test_incomplete_expression() {
        assert!(matches!(
            parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "1; 2"),
            Err(_)
        ));
    }

    #[test]
    fn test_parse_block_expression() {
        let ast =
            match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "let x = 1;").unwrap() {
                DefinitionsOrExpression::Defs(_) => unreachable!(),
                DefinitionsOrExpression::Expr(e) => e,
            };

        assert_eq!(
            ast,
            Expression(
                Position {
                    offset: 0,
                    end_offset: 3,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Let(
                    Variable(
                        Position {
                            offset: 4,
                            end_offset: 5,
                            path: PathBuf::from("__test.gdn")
                        },
                        VariableName("x".into())
                    ),
                    Box::new(Expression(
                        Position {
                            offset: 8,
                            end_offset: 9,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::IntLiteral(1)
                    ))
                )
            )
        );
    }
}
