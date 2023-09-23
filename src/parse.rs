use std::path::PathBuf;

use ariadne::{Label, Report, ReportKind, Source};
use line_numbers::LinePositions;

use crate::ast::BinaryOperatorKind;
use crate::ast::Block;
use crate::ast::Definition;
use crate::ast::Definition_;
use crate::ast::DefinitionsOrExpression;
use crate::ast::Expression;
use crate::ast::Expression_;
use crate::ast::FunInfo;
use crate::ast::MethodInfo;
use crate::ast::MethodKind;
use crate::ast::Position;
use crate::ast::SourceString;
use crate::ast::Symbol;
use crate::ast::SymbolName;
use crate::ast::SymbolWithType;
use crate::ast::TestInfo;
use crate::ast::ToplevelExpression;
use crate::ast::ToplevelItem;
use crate::ast::TypeName;
use crate::eval::ErrorMessage;
use crate::eval::StackFrame;
use crate::lex::lex;
use crate::lex::lex_between;
use crate::lex::Token;
use crate::lex::INTEGER_RE;
use crate::lex::SYMBOL_RE;

fn format_pos_in_fun(position: &Position, src_string: Option<&SourceString>) -> String {
    let mut res = String::new();

    res.push_str(&format!(
        "--> {}:{}\n",
        position.path.display(),
        position.line_number + 1,
    ));

    let relevant_line = match src_string {
        Some(src_string) => {
            let src = &src_string.src;
            let line_positions = LinePositions::from(src.as_str());

            // TODO: this is the line number relative to the start of
            // the SourceString, not the start of the file.
            let offset = std::cmp::max(
                position.start_offset as isize - src_string.offset as isize,
                0,
            ) as usize;
            let line_num = if offset >= src.len() {
                // TODO: this occurs when we are using the wrong
                // SourceString, such as the main function wrapper. We
                // should find the relevant SourceString instead.
                0.into()
            } else {
                line_positions.from_offset(offset)
            };

            let s_lines: Vec<_> = src.lines().collect();
            s_lines[line_num.as_usize()].to_owned()
        }
        None => "??? no source found (FunInfo is None) ???".to_owned(),
    };

    res.push_str(&relevant_line);
    res
}

pub fn format_error_with_stack(
    message: &ErrorMessage,
    position: &Position,
    stack: &[StackFrame],
) -> String {
    let mut res = String::new();

    res.push_str(&format!("Error: {}\n\n", message.0));

    let top_stack = stack.last().unwrap();
    let src_string = top_stack
        .enclosing_fun
        .as_ref()
        .map(|fi| fi.src_string.clone());
    res.push_str(&format_pos_in_fun(position, src_string.as_ref()));

    for stack_frame in stack.iter().rev() {
        if let Some((var, src_string)) = &stack_frame.call_site {
            res.push('\n');
            res.push_str(&format_pos_in_fun(&var.0, src_string.as_ref()));
        }
    }

    res
}

pub fn format_error(message: &ErrorMessage, position: &Position, src: &str) -> String {
    let mut res = Vec::new();

    let path_str = position.path.display().to_string();
    let r = Report::build(ReportKind::Error, &path_str, position.start_offset)
        .with_label(
            Label::new((&path_str, position.start_offset..position.end_offset))
                .with_message(&message.0),
        )
        .finish();

    r.write((&path_str, Source::from(src)), &mut res).unwrap();
    String::from_utf8_lossy(&res).to_string()
}

pub fn format_parse_error(message: &ErrorMessage, position: &Position, src: &str) -> String {
    format_error(message, position, src)
}

#[derive(Debug)]
pub enum ParseError {
    Invalid {
        position: Position,
        message: ErrorMessage,
        additional: Vec<(Position, String)>,
    },
    Incomplete(ErrorMessage),
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
        None => Err(ParseError::Incomplete(ErrorMessage(format!(
            "Expected {}, got EOF",
            token_description
        )))),
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
                    message: ErrorMessage(format!("Expected `{}`, got `{}`", expected, token.text)),
                    additional: vec![],
                })
            }
        }
        None => Err(ParseError::Incomplete(ErrorMessage(format!(
            "Expected `{}`, got EOF",
            expected
        )))),
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
            message: ErrorMessage(format!("Not a valid integer literal: {}", token.text)),
            additional: vec![],
        })
    }
}

fn parse_variable_expression(tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let variable = parse_symbol(tokens)?;
    Ok(Expression(
        variable.0.clone(),
        Expression_::Variable(variable),
    ))
}

fn parse_parenthesis_expression(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Expression, ParseError> {
    require_token(tokens, "(")?;
    let expr = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_list_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let open_brace = require_token(tokens, "[")?;
    let items = parse_comma_separated_exprs(src, tokens, "]")?;
    require_token(tokens, "]")?;

    Ok(Expression(
        open_brace.position,
        Expression_::ListLiteral(items),
    ))
}

fn parse_lambda_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let fun_keyword = require_token(tokens, "fun")?;

    let params = parse_parameters(tokens)?;
    let return_type = parse_type_annotation(tokens)?;

    let body = parse_block(src, tokens)?;

    let start_offset = fun_keyword.position.start_offset;
    let end_offset = body.close_brace.end_offset;
    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    Ok(Expression(
        fun_keyword.position,
        Expression_::FunLiteral(FunInfo {
            src_string,
            params,
            body,
            doc_comment: None,
            name: None,
            return_type,
        }),
    ))
}

fn parse_if_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let if_token = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    let then_body = parse_block(src, tokens)?;

    let else_body: Option<Block> = if next_token_is(tokens, "else") {
        pop_token(tokens);

        if next_token_is(tokens, "if") {
            let if_expr = parse_if_expression(src, tokens)?;
            Some(Block {
                // TODO: when there is a chain of if/else if
                // expressions, the open brace isn't meaningful. This
                // is an ugly hack.
                open_brace: if_expr.0.clone(),
                close_brace: if_expr.0.clone(),
                exprs: vec![if_expr],
            })
        } else {
            Some(parse_block(src, tokens)?)
        }
    } else {
        None
    };

    Ok(Expression(
        if_token.position,
        Expression_::If(Box::new(condition), then_body, else_body),
    ))
}

fn parse_while_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let while_token = require_token(tokens, "while")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    let body = parse_block(src, tokens)?;

    Ok(Expression(
        while_token.position,
        Expression_::While(Box::new(condition), body),
    ))
}

fn parse_return_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let return_token = require_token(tokens, "return")?;

    // TODO: allow `return;`
    let expr = parse_inline_expression(src, tokens)?;
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

fn parse_simple_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "{" {
            let exprs = parse_block(src, tokens)?;
            return Ok(Expression(token.position, Expression_::Block(exprs)));
        }

        if token.text == "(" {
            return parse_parenthesis_expression(src, tokens);
        }

        if token.text == "[" {
            return parse_list_expression(src, tokens);
        }

        if token.text == "fun" {
            return parse_lambda_expression(src, tokens);
        }

        if token.text == "true" {
            pop_token(tokens);
            return Ok(Expression(token.position, Expression_::BoolLiteral(true)));
        }
        if token.text == "false" {
            pop_token(tokens);
            return Ok(Expression(token.position, Expression_::BoolLiteral(false)));
        }

        if SYMBOL_RE.is_match(token.text) {
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
            message: ErrorMessage(format!(
                "Expected an expression, got: {} (offset {})",
                token.text, token.position.start_offset
            )),
            additional: vec![],
        });
    }

    Err(ParseError::Incomplete(ErrorMessage(
        "Expected an expression".to_owned(),
    )))
}

fn parse_comma_separated_exprs(
    src: &str,
    tokens: &mut &[Token<'_>],
    terminator: &str,
) -> Result<Vec<Expression>, ParseError> {
    let mut items = vec![];
    loop {
        if next_token_is(tokens, terminator) {
            break;
        }

        let arg = parse_inline_expression(src, tokens)?;
        items.push(arg);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == terminator {
                break;
            } else {
                return Err(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `{}` here, but got `{}`",
                        terminator, token.text
                    )),
                    additional: vec![],
                });
            }
        } else {
            return Err(ParseError::Incomplete(ErrorMessage(format!(
                "Invalid syntax: Expected `,` or `{}` here, but got EOF",
                terminator
            ))));
        }
    }

    Ok(items)
}

fn parse_call_arguments(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Vec<Expression>, ParseError> {
    require_token(tokens, "(")?;
    let args = parse_comma_separated_exprs(src, tokens, ")")?;
    require_token(tokens, ")")?;
    Ok(args)
}

/// Parse an expression, and handle trailing syntax (function calls,
/// method calls) if present.
///
/// We handle trailing syntax separately from
/// `parse_simple_expression`, to avoid infinite recursion. This is
/// essentially left-recursion from a grammar perspective.
fn parse_simple_expression_with_trailing(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression(src, tokens)?;

    loop {
        match peek_token(tokens) {
            Some(token) if token.text == "(" => {
                let arguments = parse_call_arguments(src, tokens)?;
                expr = Expression(expr.0.clone(), Expression_::Call(Box::new(expr), arguments));
            }
            Some(token) if token.text == "." => {
                pop_token(tokens);
                let variable = parse_symbol(tokens)?;
                let arguments = parse_call_arguments(src, tokens)?;
                expr = Expression(
                    expr.0.clone(),
                    Expression_::MethodCall(Box::new(expr), variable, arguments),
                );
            }
            _ => break,
        }
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
        "<=" => Some(BinaryOperatorKind::LessThanOrEqual),
        ">" => Some(BinaryOperatorKind::GreaterThan),
        ">=" => Some(BinaryOperatorKind::GreaterThanOrEqual),
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
fn parse_inline_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    parse_general_expression(src, tokens, true)
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
fn parse_block_member_expression(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Expression, ParseError> {
    parse_general_expression(src, tokens, false)
}

/// Parse an inline or block member expression.
fn parse_general_expression(
    src: &str,
    tokens: &mut &[Token<'_>],
    is_inline: bool,
) -> Result<Expression, ParseError> {
    if !is_inline {
        // TODO: Matching on tokens will prevent us from doing more
        // complex assignments like `foo.bar = 1;`.
        if let Some((_, token)) = peek_two_tokens(tokens) {
            if token.text == "=" {
                return parse_assign_expression(src, tokens);
            }
        }

        if let Some(token) = peek_token(tokens) {
            if token.text == "let" {
                return parse_let_expression(src, tokens);
            }
            if token.text == "return" {
                return parse_return_expression(src, tokens);
            }
            if token.text == "while" {
                return parse_while_expression(src, tokens);
            }
        }
    }

    // `if` can occur as both an inline expression and a standalone
    // expression.
    if let Some(token) = peek_token(tokens) {
        if token.text == "if" {
            return parse_if_expression(src, tokens);
        }
    }

    let expr = parse_simple_expression_or_binop(src, tokens)?;
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
fn parse_simple_expression_or_binop(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression_with_trailing(src, tokens)?;

    if let Some(token) = peek_token(tokens) {
        if let Some(op) = token_as_binary_op(token) {
            pop_token(tokens);

            let rhs_expr = parse_simple_expression_with_trailing(src, tokens)?;
            expr = Expression(
                expr.0.clone(),
                Expression_::BinaryOperator(Box::new(expr), op, Box::new(rhs_expr)),
            );
        }
    }

    Ok(expr)
}

fn parse_definition(src: &str, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == "fun" {
            return parse_function_or_method(src, tokens);
        }
        if token.text == "test" {
            return parse_test(src, tokens);
        }

        // TODO: Include the token in the error message.
        return Err(ParseError::Invalid {
            position: token.position,
            message: ErrorMessage("Expected a definition".to_string()),
            additional: vec![],
        });
    }

    Err(ParseError::Incomplete(ErrorMessage(
        "Unfinished definition".to_owned(),
    )))
}

fn parse_test(src: &str, tokens: &mut &[Token]) -> Result<Definition, ParseError> {
    let test_token = require_token(tokens, "test")?;
    let doc_comment = parse_doc_comment(&test_token);

    let name = if let Some(token) = peek_token(tokens) {
        if token.text == "{" {
            None
        } else {
            Some(parse_symbol(tokens)?)
        }
    } else {
        return Err(ParseError::Incomplete(ErrorMessage(
            "Unfinished test definition".to_owned(),
        )));
    };

    let body = parse_block(src, tokens)?;

    let mut start_offset = test_token.position.start_offset;
    if let Some((comment_pos, _)) = test_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    Ok(Definition(
        src_string.clone(),
        test_token.position,
        Definition_::TestDefinition(TestInfo {
            src_string,
            doc_comment,
            name,
            body,
        }),
    ))
}

fn parse_type_name(tokens: &mut &[Token<'_>]) -> Result<TypeName, ParseError> {
    let name = parse_symbol(tokens)?;
    Ok(TypeName(name.1 .0))
}

fn parse_type_annotation(tokens: &mut &[Token<'_>]) -> Result<Option<TypeName>, ParseError> {
    if let Some(token) = peek_token(tokens) {
        if token.text == ":" {
            pop_token(tokens);
            return Ok(Some(parse_type_name(tokens)?));
        }
    }

    Ok(None)
}

fn parse_parameter(tokens: &mut &[Token<'_>]) -> Result<SymbolWithType, ParseError> {
    let param = parse_symbol(tokens)?;
    let param_type = parse_type_annotation(tokens)?;
    Ok(SymbolWithType(param, param_type))
}

fn parse_parameters(tokens: &mut &[Token<'_>]) -> Result<Vec<SymbolWithType>, ParseError> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
        }

        let param = parse_parameter(tokens)?;
        params.push(param);

        if let Some(token) = peek_token(tokens) {
            if token.text == "," {
                pop_token(tokens);
            } else if token.text == ")" {
                break;
            } else {
                return Err(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
            }
        } else {
            return Err(ParseError::Incomplete(ErrorMessage(
                "Invalid syntax: Expected `,` or `)` here, but got EOF".to_string(),
            )));
        }
    }

    require_token(tokens, ")")?;
    Ok(params)
}

fn parse_block(src: &str, tokens: &mut &[Token<'_>]) -> Result<Block, ParseError> {
    let open_brace = require_token(tokens, "{")?;

    let mut exprs = vec![];
    loop {
        if let Some(token) = peek_token(tokens) {
            if token.text == "}" {
                break;
            }
        } else {
            return Err(ParseError::Incomplete(ErrorMessage(
                "Invalid syntax: Expected `}}` here, but got EOF".to_string(),
            )));
        }

        let expr = parse_block_member_expression(src, tokens)?;
        exprs.push(expr);
    }

    let close_brace = require_token(tokens, "}")?;
    Ok(Block {
        open_brace: open_brace.position,
        exprs,
        close_brace: close_brace.position,
    })
}

fn join_comments(comments: &[(Position, &str)]) -> String {
    let mut comment_texts = comments
        .iter()
        .map(|(_, comment)| comment.strip_prefix(' ').unwrap_or(comment))
        .collect::<Vec<_>>();

    if let Some(comment_text) = comment_texts.last_mut() {
        *comment_text = comment_text.strip_suffix('\n').unwrap_or(comment_text)
    }

    comment_texts.join("")
}

fn parse_doc_comment(token: &Token) -> Option<String> {
    if !token.preceding_comments.is_empty() {
        return Some(join_comments(&token.preceding_comments));
    }
    None
}

fn parse_function_or_method(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<Definition, ParseError> {
    match peek_two_tokens(tokens) {
        Some((_, second_token)) => {
            if second_token.text == "(" {
                parse_method(src, tokens)
            } else {
                parse_function(src, tokens)
            }
        }
        None => Err(ParseError::Incomplete(ErrorMessage(
            "Unfinished function or method definition.".to_owned(),
        ))),
    }
}

fn parse_method(src: &str, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    let fun_token = require_token(tokens, "fun")?;
    let doc_comment = parse_doc_comment(&fun_token);

    require_token(tokens, "(")?;
    let receiver_param = parse_parameter(tokens)?;
    let receiver_name = receiver_param.0 .1;
    let receiver_type = match receiver_param.1 {
        Some(type_name) => type_name,
        None => {
            return Err(ParseError::Incomplete(ErrorMessage(
                "A type name for this method is expected.".to_owned(),
            )));
        }
    };
    require_token(tokens, ")")?;

    let name = parse_symbol(tokens)?;

    let params = parse_parameters(tokens)?;
    let return_type = parse_type_annotation(tokens)?;

    let body = parse_block(src, tokens)?;

    let mut start_offset = fun_token.position.start_offset;
    if let Some((comment_pos, _)) = fun_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let fun_info = FunInfo {
        src_string: src_string.clone(),
        doc_comment,
        name: Some(name.clone()),
        params,
        body,
        return_type,
    };
    let meth_info = MethodInfo {
        receiver_type,
        receiver_name,
        name,
        kind: MethodKind::UserDefinedMethod(fun_info),
    };

    Ok(Definition(
        src_string.clone(),
        fun_token.position,
        Definition_::MethodDefinition(meth_info),
    ))
}

fn parse_function(src: &str, tokens: &mut &[Token<'_>]) -> Result<Definition, ParseError> {
    let fun_token = require_token(tokens, "fun")?;
    let doc_comment = parse_doc_comment(&fun_token);

    let name = parse_symbol(tokens)?;

    let params = parse_parameters(tokens)?;
    let return_type = parse_type_annotation(tokens)?;

    let body = parse_block(src, tokens)?;

    let mut start_offset = fun_token.position.start_offset;
    if let Some((comment_pos, _)) = fun_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    Ok(Definition(
        src_string.clone(),
        fun_token.position,
        Definition_::FunDefinition(
            name.clone(),
            FunInfo {
                src_string,
                doc_comment,
                name: Some(name),
                params,
                body,
                return_type,
            },
        ),
    ))
}

const RESERVED_WORDS: &[&str] = &[
    "let", "fun", "true", "false", "if", "else", "while", "return", "test",
];

fn parse_symbol(tokens: &mut &[Token<'_>]) -> Result<Symbol, ParseError> {
    let variable_token = require_a_token(tokens, "variable name")?;
    if !SYMBOL_RE.is_match(variable_token.text) {
        return Err(ParseError::Invalid {
            position: variable_token.position,
            message: ErrorMessage(format!("Invalid variable name: '{}'", variable_token.text)),
            additional: vec![],
        });
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            return Err(ParseError::Invalid {
                position: variable_token.position,
                message: ErrorMessage(format!(
                    "'{}' is a reserved word that cannot be used as a variable",
                    variable_token.text
                )),
                additional: vec![],
            });
        }
    }

    Ok(Symbol(
        variable_token.position,
        SymbolName(variable_token.text.to_string()),
    ))
}

fn parse_let_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let let_token = require_token(tokens, "let")?;
    let variable = parse_symbol(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(src, tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        let_token.position, // TODO: this should be a larger span.
        Expression_::Let(variable, Box::new(expr)),
    ))
}

fn parse_assign_expression(src: &str, tokens: &mut &[Token<'_>]) -> Result<Expression, ParseError> {
    let variable = parse_symbol(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(src, tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        variable.0.clone(),
        Expression_::Assign(variable, Box::new(expr)),
    ))
}

fn parse_def_or_expr(
    src: &str,
    tokens: &mut &[Token<'_>],
) -> Result<DefinitionsOrExpression, ParseError> {
    // Parsing advances the tokens pointer, so create a copy for
    // trying an expression parse.
    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_block_member_expression(src, &mut tokens_copy) {
        if tokens_copy.is_empty() {
            let pos = &expr.0;
            let toplevel_expr =
                ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
            return Ok(DefinitionsOrExpression::Expr(toplevel_expr));
        }
    }

    let mut tokens_copy = tokens.clone();
    if let Ok(expr) = parse_inline_expression(src, &mut tokens_copy) {
        if tokens_copy.is_empty() {
            let pos = &expr.0;
            let toplevel_expr =
                ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
            return Ok(DefinitionsOrExpression::Expr(toplevel_expr));
        }
    }

    let mut defs = vec![];
    while !tokens.is_empty() {
        defs.push(parse_definition(src, tokens)?);
    }

    Ok(DefinitionsOrExpression::Defs(defs))
}

pub fn parse_def_or_expr_from_str(
    path: &PathBuf,
    s: &str,
) -> Result<DefinitionsOrExpression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_def_or_expr(s, &mut token_ptr)
}

pub fn parse_inline_expr_from_str(path: &PathBuf, s: &str) -> Result<Expression, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];
    parse_inline_expression(s, &mut token_ptr)
}

pub fn parse_toplevel_item(path: &PathBuf, s: &str) -> Result<ToplevelItem, ParseError> {
    let items = parse_toplevel_items(path, s)?;
    Ok(items[0].clone())
}

pub fn parse_toplevel_items(path: &PathBuf, s: &str) -> Result<Vec<ToplevelItem>, ParseError> {
    let tokens = lex(path, s)?;
    let mut token_ptr = &tokens[..];

    let mut res = vec![];

    match parse_def_or_expr(s, &mut token_ptr)? {
        DefinitionsOrExpression::Defs(defs) => {
            for def in defs {
                res.push(ToplevelItem::Def(def.clone()));
            }
        }
        DefinitionsOrExpression::Expr(e) => {
            // TODO: parse all the toplevel expressions, not just the first.
            res.push(ToplevelItem::Expr(e));
        }
    };

    Ok(res)
}

pub fn parse_toplevel_items_from_span(
    path: &PathBuf,
    s: &str,
    offset: usize,
    end_offset: usize,
) -> Result<Vec<ToplevelItem>, ParseError> {
    let tokens = lex_between(path, s, offset, end_offset)?;
    let mut token_ptr = &tokens[..];

    let mut res = vec![];

    match parse_def_or_expr(s, &mut token_ptr)? {
        DefinitionsOrExpression::Defs(defs) => {
            for def in defs {
                res.push(ToplevelItem::Def(def.clone()));
            }
        }
        DefinitionsOrExpression::Expr(e) => {
            // TODO: parse all the toplevel expressions, not just the first.
            res.push(ToplevelItem::Expr(e));
        }
    };

    Ok(res)
}
#[cfg(test)]
pub fn parse_exprs_from_str(s: &str) -> Result<Vec<Expression>, ParseError> {
    let tokens = lex(&PathBuf::from("__test.gdn"), s)?;
    let mut token_ptr = &tokens[..];

    let mut res = vec![];
    while !token_ptr.is_empty() {
        res.push(parse_block_member_expression(s, &mut token_ptr)?);
    }

    Ok(res)
}

#[cfg(test)]
pub fn parse_defs_from_str(s: &str) -> Result<Vec<Definition>, ParseError> {
    let defs = match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), s)? {
        DefinitionsOrExpression::Defs(defs) => defs,
        DefinitionsOrExpression::Expr(_) => unreachable!(),
    };

    Ok(defs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_bool_literal() {
        let ast = parse_exprs_from_str("true;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 4,
                    line_number: 0,
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
                    line_number: 0,
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
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::StringLiteral("a\nb\\c\"d".into())
            )]
        );
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
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Variable(Symbol(
                    Position {
                        start_offset: 0,
                        end_offset: 7,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn")
                    },
                    SymbolName("abc_def".to_string())
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
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Let(
                    Symbol(
                        Position {
                            start_offset: 4,
                            end_offset: 5,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        SymbolName("x".into())
                    ),
                    Box::new(Expression(
                        Position {
                            start_offset: 8,
                            end_offset: 9,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::IntLiteral(1)
                    ))
                )
            )]
        );
    }

    #[test]
    fn test_parse_let_uppercase() {
        let ast = parse_exprs_from_str("let A = 1;").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 3,
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Let(
                    Symbol(
                        Position {
                            start_offset: 4,
                            end_offset: 5,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        SymbolName("A".into())
                    ),
                    Box::new(Expression(
                        Position {
                            start_offset: 8,
                            end_offset: 9,
                            line_number: 0,
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
                    line_number: 0,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 8,
                            line_number: 0,
                            path: path.clone()
                        },
                        Expression_::BoolLiteral(true)
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 10,
                            end_offset: 11,
                            line_number: 0,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 11,
                            end_offset: 12,
                            line_number: 0,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
                    Some(Block {
                        open_brace: Position {
                            start_offset: 18,
                            end_offset: 19,
                            line_number: 0,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 19,
                            end_offset: 20,
                            line_number: 0,
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
                    line_number: 0,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 5,
                            line_number: 0,
                            path: path.clone()
                        },
                        Expression_::Variable(Symbol(
                            Position {
                                start_offset: 4,
                                end_offset: 5,
                                line_number: 0,
                                path: path.clone()
                            },
                            SymbolName("x".into())
                        ))
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 7,
                            end_offset: 8,
                            line_number: 0,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 8,
                            end_offset: 9,
                            line_number: 0,
                            path: path.clone()
                        },
                        exprs: vec![]
                    },
                    Some(Block {
                        open_brace: Position {
                            start_offset: 15,
                            end_offset: 17,
                            line_number: 0,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 15,
                            end_offset: 17,
                            line_number: 0,
                            path: path.clone()
                        },
                        exprs: vec![Expression(
                            Position {
                                start_offset: 15,
                                end_offset: 17,
                                line_number: 0,
                                path: path.clone()
                            },
                            Expression_::If(
                                Box::new(Expression(
                                    Position {
                                        start_offset: 19,
                                        end_offset: 20,
                                        line_number: 0,
                                        path: path.clone()
                                    },
                                    Expression_::Variable(Symbol(
                                        Position {
                                            start_offset: 19,
                                            end_offset: 20,
                                            line_number: 0,
                                            path: path.clone()
                                        },
                                        SymbolName("y".into())
                                    ))
                                )),
                                Block {
                                    open_brace: Position {
                                        start_offset: 22,
                                        end_offset: 23,
                                        line_number: 0,
                                        path: path.clone()
                                    },
                                    close_brace: Position {
                                        start_offset: 23,
                                        end_offset: 24,
                                        line_number: 0,
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
                    line_number: 0,
                    path: path.clone()
                },
                Expression_::If(
                    Box::new(Expression(
                        Position {
                            start_offset: 4,
                            end_offset: 8,
                            line_number: 0,
                            path: path.clone()
                        },
                        Expression_::BoolLiteral(true)
                    )),
                    Block {
                        open_brace: Position {
                            start_offset: 10,
                            end_offset: 11,
                            line_number: 0,
                            path: path.clone()
                        },
                        close_brace: Position {
                            start_offset: 11,
                            end_offset: 12,
                            line_number: 0,
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
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Return(Box::new(Expression(
                    Position {
                        start_offset: 7,
                        end_offset: 11,
                        line_number: 0,
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
                    line_number: 0,
                    path: PathBuf::from("__test.gdn")
                },
                Expression_::Call(
                    Box::new(Expression(
                        Position {
                            start_offset: 0,
                            end_offset: 3,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        Expression_::Call(
                            Box::new(Expression(
                                Position {
                                    start_offset: 0,
                                    end_offset: 3,
                                    line_number: 0,
                                    path: PathBuf::from("__test.gdn")
                                },
                                Expression_::Variable(Symbol(
                                    Position {
                                        start_offset: 0,
                                        end_offset: 3,
                                        line_number: 0,
                                        path: PathBuf::from("__test.gdn")
                                    },
                                    SymbolName("foo".into())
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
    fn test_parse_function_with_doc_comment() {
        let path = PathBuf::from("__test.gdn");

        let src = "// Hello\n// World\nfun foo() {}";
        let ast = parse_defs_from_str(src).unwrap();

        assert_eq!(
            ast,
            vec![Definition(
                SourceString {
                    offset: 0,
                    src: "// Hello\n// World\nfun foo() {}".to_owned()
                },
                Position {
                    start_offset: 18,
                    end_offset: 21,
                    line_number: 2,
                    path: path.clone()
                },
                Definition_::FunDefinition(
                    Symbol(
                        Position {
                            start_offset: 22,
                            end_offset: 25,
                            line_number: 2,
                            path: path.clone(),
                        },
                        SymbolName("foo".into())
                    ),
                    FunInfo {
                        src_string: SourceString {
                            offset: 0,
                            src: "// Hello\n// World\nfun foo() {}".to_owned()
                        },
                        doc_comment: Some("Hello\nWorld".into()),
                        name: Some(Symbol(
                            Position {
                                start_offset: 22,
                                end_offset: 25,
                                line_number: 2,
                                path: path.clone(),
                            },
                            SymbolName("foo".into())
                        )),
                        params: vec![],
                        body: Block {
                            open_brace: Position {
                                start_offset: 28,
                                end_offset: 29,
                                line_number: 2,
                                path: path.clone()
                            },
                            close_brace: Position {
                                start_offset: 29,
                                end_offset: 30,
                                line_number: 2,
                                path: path.clone()
                            },
                            exprs: vec![]
                        },
                        return_type: None,
                    }
                )
            )]
        );
    }

    #[test]
    fn test_parse_method() {
        let src = "fun (self: List) foo() {}";
        let defs = parse_defs_from_str(src).unwrap();

        assert_eq!(defs.len(), 1);
        assert!(matches!(defs[0].2, Definition_::MethodDefinition(_)));
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
                        line_number: 0,
                        path: PathBuf::from("__test.gdn")
                    },
                    Expression_::Let(
                        Symbol(
                            Position {
                                start_offset: 4,
                                end_offset: 5,
                                line_number: 0,
                                path: PathBuf::from("__test.gdn")
                            },
                            SymbolName("x".into())
                        ),
                        Box::new(Expression(
                            Position {
                                start_offset: 8,
                                end_offset: 9,
                                line_number: 0,
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
