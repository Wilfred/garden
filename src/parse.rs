use std::path::{Path, PathBuf};

use ariadne::{Label, Report, ReportKind, Source};
use lazy_static::lazy_static;
use line_numbers::LinePositions;
use regex::Regex;

use crate::ast::BinaryOperatorKind;
use crate::ast::Block;
use crate::ast::Definition;
use crate::ast::Definition_;
use crate::ast::DefinitionsOrExpression;
use crate::ast::Expression;
use crate::ast::Expression_;
use crate::ast::FunInfo;
use crate::ast::Position;
use crate::ast::ToplevelExpression;
use crate::ast::Variable;
use crate::ast::VariableName;

pub fn simple_format_pos(position: &Position, src: &str) -> String {
    let line_positions = LinePositions::from(src);
    let line_num = line_positions.from_offset(position.start_offset);

    let mut res = String::new();
    res.push_str(&format!(
        "--> {}:{}\n",
        position.path.display(),
        line_num.display()
    ));

    let s_lines: Vec<_> = src.lines().collect();
    res.push_str(s_lines[line_num.as_usize()]);

    res
}

pub fn format_error(message: &str, position: &Position, src: &str) -> String {
    let mut res = Vec::new();

    let path_str = position.path.display().to_string();
    let r = Report::build(ReportKind::Error, &path_str, position.start_offset)
        .with_label(
            Label::new((&path_str, position.start_offset..position.end_offset))
                .with_message(message),
        )
        .finish();

    r.write((&path_str, Source::from(src)), &mut res).unwrap();
    String::from_utf8_lossy(&res).to_string()
}

#[derive(Debug)]
pub enum ParseError {
    Invalid {
        position: Position,
        message: String,
        additional: Vec<(Position, String)>,
    },
    Incomplete(String),
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
                Err(ParseError::Invalid {
                    position: token.position,
                    message: format!("Expected `{}`, got `{}`", expected, token.text),
                    additional: vec![],
                })
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
        Err(ParseError::Invalid {
            position: token.position,
            message: format!("Not a valid integer literal: {}", token.text),
            additional: vec![],
        })
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
    let body = parse_block(tokens)?;

    Ok(Expression(
        fun_keyword.position,
        Expression_::Lambda(params, body),
    ))
}

fn parse_if_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let if_token = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(tokens)?;
    require_token(tokens, ")")?;

    let then_body = parse_block(tokens)?;

    let else_body: Option<Block> = if next_token_is(tokens, "else") {
        pop_token(tokens);

        if next_token_is(tokens, "if") {
            let if_expr = parse_if_expression(tokens)?;
            Some(Block {
                // TODO: when there is a chain of if/else if
                // expressions, the open brace isn't meaningful. This
                // is an ugly hack.
                open_brace: if_expr.0.clone(),
                close_brace: if_expr.0.clone(),
                exprs: vec![if_expr],
            })
        } else {
            Some(parse_block(tokens)?)
        }
    } else {
        None
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

    let body = parse_block(tokens)?;

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

        if token.text.starts_with('\"') {
            pop_token(tokens);
            return Ok(Expression(
                token.position,
                Expression_::StringLiteral(unescape_string(token.text)),
            ));
        }

        if INTEGER_RE.is_match(token.text) {
            return parse_integer(tokens);
        }

        return Err(ParseError::Invalid {
            position: token.position.clone(),
            message: format!(
                "Expected an expression, got: {} (offset {})",
                token.text, token.position.start_offset
            ),
            additional: vec![],
        });
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
                return Err(ParseError::Invalid {
                    position: token.position,
                    message: format!(
                        "Invalid syntax: Expected `,` or `{}` here, but got `{}`",
                        terminator, token.text
                    ),
                    additional: vec![],
                });
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

fn parse_definition(
    path: &Path,
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Definition, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "fun" {
            return parse_function(src, tokens);
        }

        // TODO: Include the token in the error message.
        return Err(ParseError::Invalid {
            position: token.position,
            message: "Expected a definition".to_string(),
            additional: vec![],
        });
    }

    // TODO: return a more meaningful position (e.g. EOF)
    Err(ParseError::Invalid {
        position: Position {
            start_offset: 0,
            end_offset: 0,
            path: path.into(),
        },
        message: "Expected a definition, got EOF".to_string(),
        additional: vec![],
    })
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
                return Err(ParseError::Invalid {
                    position: token.position,
                    message: format!(
                        "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                        token.text
                    ),
                    additional: vec![],
                });
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

fn parse_block(tokens: &mut &[Token<'_>]) -> Result<Block, ParseError> {
    let open_brace = require_token(tokens, "{")?;

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

    let close_brace = require_token(tokens, "}")?;
    Ok(Block {
        open_brace: open_brace.position,
        exprs,
        close_brace: close_brace.position,
    })
}

fn join_comments(comments: &[&str]) -> String {
    let mut comment_texts = comments
        .iter()
        .map(|comment| comment.strip_prefix(' ').unwrap_or(comment))
        .collect::<Vec<_>>();

    if let Some(comment_text) = comment_texts.last_mut() {
        *comment_text = comment_text.strip_suffix('\n').unwrap_or(comment_text)
    }

    comment_texts.join("")
}

fn parse_function(src: &str, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    let fun_token = require_token(tokens, "fun")?;
    let mut doc_comment = None;
    if !fun_token.preceding_comments.is_empty() {
        doc_comment = Some(join_comments(&fun_token.preceding_comments));
    }

    let name = parse_variable_name(tokens)?;
    let params = parse_function_params(tokens)?;
    let body = parse_block(tokens)?;

    Ok(Definition(
        src.to_owned(),
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
        return Err(ParseError::Invalid {
            position: variable_token.position,
            message: format!("Invalid variable name: '{}'", variable_token.text),
            additional: vec![],
        });
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            return Err(ParseError::Invalid {
                position: variable_token.position,
                message: format!(
                    "'{}' is a reserved word that cannot be used as a variable",
                    variable_token.text
                ),
                additional: vec![],
            });
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
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<DefinitionsOrExpression, ParseError> {
    // Parsing advances the tokens pointer, so create a copy for
    // trying an expression parse.
    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_block_member_expression(&mut tokens_copy) {
        if tokens_copy.is_empty() {
            let pos = &expr.0;
            let toplevel_expr =
                ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
            return Ok(DefinitionsOrExpression::Expr(toplevel_expr));
        }
    }

    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_inline_expression(&mut tokens_copy) {
        if tokens_copy.is_empty() {
            let pos = &expr.0;
            let toplevel_expr =
                ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
            return Ok(DefinitionsOrExpression::Expr(toplevel_expr));
        }
    }

    let mut defs = vec![];
    while !tokens.is_empty() {
        defs.push(parse_definition(path, src, tokens)?);
    }

    Ok(DefinitionsOrExpression::Defs(defs))
}

pub fn parse_def_or_expr_from_str(
    path: &PathBuf,
    s: &str,
) -> Result<DefinitionsOrExpression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_def_or_expr(path, s, &mut token_ptr)
}

pub fn parse_def_or_expr_from_span(
    path: &PathBuf,
    s: &str,
    offset: usize,
    end_offset: usize,
) -> Result<DefinitionsOrExpression, ParseError> {
    let tokens = lex_between(path, s, offset, end_offset)?;
    let mut token_ptr = &tokens[..];
    parse_def_or_expr(path, s, &mut token_ptr)
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
    path: &Path,
    s: &'a str,
    offset: usize,
    end_offset: usize,
) -> Result<Vec<Token<'a>>, ParseError> {
    assert!(end_offset <= s.len());

    let mut res: Vec<Token<'a>> = vec![];

    let mut preceding_comments = vec![];
    let mut offset = offset;

    // Skip shebang if present at the beginning of the file.
    if offset == 0 && s.starts_with('#') {
        offset = s.find('\n').unwrap_or(s.len());
    }

    'outer: while offset < end_offset {
        let s = &s[offset..];

        // Skip over comments.
        if s.starts_with("//") {
            if let Some(i) = s.find('\n') {
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
                        start_offset: offset,
                        end_offset: offset + token_str.len(),
                        path: path.to_path_buf(),
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
                    start_offset: offset,
                    end_offset: offset + integer_match.end(),
                    path: path.to_path_buf(),
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
                        start_offset: offset,
                        end_offset: offset + 1,
                        path: path.to_path_buf(),
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
                    start_offset: offset,
                    end_offset: offset + string_match.end(),
                    path: path.to_path_buf(),
                },
                text: string_match.as_str(),
                preceding_comments,
            });
            preceding_comments = vec![];

            offset += string_match.end();
        } else if let Some(variable_match) = VARIABLE_RE.find(s) {
            res.push(Token {
                position: Position {
                    start_offset: offset,
                    end_offset: offset + variable_match.end(),
                    path: path.to_path_buf(),
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
        return Err(ParseError::Invalid {
            position: Position {
                start_offset: offset,
                end_offset: s.len(),
                path: path.to_path_buf(),
            },
            message: format!("Unrecognized syntax: '{}'", &s[offset..]),
            additional: vec![],
        });
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
                    start_offset: 0,
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
                    start_offset: 1,
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
                    start_offset: 0,
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
                    start_offset: 0,
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
                    start_offset: 0,
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
                    start_offset: 5,
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
                    start_offset: 0,
                    end_offset: 7,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Variable(Variable(
                    Position {
                        start_offset: 0,
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
                    start_offset: 0,
                    end_offset: 3,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Let(
                    Variable(
                        Position {
                            start_offset: 4,
                            end_offset: 5,
                            path: PathBuf::from("__test.gdn")
                        },
                        VariableName("x".into())
                    ),
                    Box::new(Expression(
                        Position {
                            start_offset: 8,
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
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("if (true) {} else {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 2,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 8,
                            path: path.clone()
                        },
                        Expression_::BoolLiteral(true)
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 10,
                            end_offset: 11,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 11,
                            end_offset: 12,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
                    Some(Block {
                        open_brace: Position {
                            start_offset: 18,
                            end_offset: 19,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 19,
                            end_offset: 20,
                            path: path.clone()
                        },
                        exprs: vec![]
                    }),
                )
            )]
        );
    }

    #[test]
    fn test_parse_else_if() {
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("if (x) {} else if (y) {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 2,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 5,
                            path: path.clone()
                        },
                        Expression_::Variable(Variable(
                            Position {
                                start_offset: 4,
                                end_offset: 5,
                                path: path.clone()
                            },
                            VariableName("x".into())
                        ))
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 7,
                            end_offset: 8,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 8,
                            end_offset: 9,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
                    Some(Block {
                        open_brace: Position {
                            start_offset: 15,
                            end_offset: 17,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 15,
                            end_offset: 17,
                            path: path.clone()
                        },
                        exprs: vec![Expression(
                            Position {
                                start_offset: 15,
                                end_offset: 17,
                                path: path.clone()
                            },
                            Expression_::If(
                                Box::new(Expression(
                                    Position {
                                        start_offset: 19,
                                        end_offset: 20,
                                        path: path.clone()
                                    },
                                    Expression_::Variable(Variable(
                                        Position {
                                            start_offset: 19,
                                            end_offset: 20,
                                            path: path.clone()
                                        },
                                        VariableName("y".into())
                                    ))
                                )),
                                Block {
                                    open_brace: Position {
                                        start_offset: 22,
                                        end_offset: 23,
                                        path: path.clone()
                                    },
                                    close_brace: Position {
                                        start_offset: 23,
                                        end_offset: 24,
                                        path: path.clone()
                                    },
                                    exprs: vec![]
                                },
                                None,
                            )
                        )]
                    }),
                )
            )]
        );
    }

    #[test]
    fn test_parse_if() {
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("if (true) {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 2,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 8,
                            path: path.clone()
                        },
                        Expression_::BoolLiteral(true)
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 10,
                            end_offset: 11,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 11,
                            end_offset: 12,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
                    None,
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
                    start_offset: 0,
                    end_offset: 6,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Return(Box::new(Expression(
                    Position {
                        start_offset: 7,
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
                    start_offset: 0,
                    end_offset: 3,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Call(
                    Box::new(Expression(
                        Position {
                            start_offset: 0,
                            end_offset: 3,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::Call(
                            Box::new(Expression(
                                Position {
                                    start_offset: 0,
                                    end_offset: 3,
                                    path: PathBuf::from("__test.gdn")
                                },
                                Expression_::Variable(Variable(
                                    Position {
                                        start_offset: 0,
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
        let path = PathBuf::from("__test.gdn");
        let src = "// Hello\n// World\nfun foo() {}";
        let ast = match parse_def_or_expr_from_str(&path, src).unwrap() {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };

        assert_eq!(
            ast,
            vec![Definition(
                src.to_owned(),
                Position {
                    start_offset: 18,
                    end_offset: 21,
                    path: path.clone()
                },
                Definition_::Fun(FunInfo {
                    doc_comment: Some("Hello\nWorld".into()),
                    name: Variable(
                        Position {
                            start_offset: 22,
                            end_offset: 25,
                            path: path.clone(),
                        },
                        VariableName("foo".into())
                    ),
                    params: vec![],
                    body: Block {
                        open_brace: Position {
                            start_offset: 28,
                            end_offset: 29,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 29,
                            end_offset: 30,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
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
            ToplevelExpression(
                // TODO: this is wrong, it should be the whole expression
                "let".into(),
                Expression(
                    Position {
                        start_offset: 0,
                        end_offset: 3,
                        path: PathBuf::from("__test.gdn")
                    },
                    Expression_::Let(
                        Variable(
                            Position {
                                start_offset: 4,
                                end_offset: 5,
                                path: PathBuf::from("__test.gdn")
                            },
                            VariableName("x".into())
                        ),
                        Box::new(Expression(
                            Position {
                                start_offset: 8,
                                end_offset: 9,
                                path: PathBuf::from("__test.gdn")
                            },
                            Expression_::IntLiteral(1)
                        ))
                    )
                )
            )
        );
    }
}
