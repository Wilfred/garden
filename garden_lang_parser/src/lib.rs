pub mod ast;
pub mod diagnostics;
pub mod lex;

use std::collections::HashSet;
use std::path::Path;

use crate::ast::BinaryOperatorKind;
use crate::ast::Block;
use crate::ast::Definition;
use crate::ast::Definition_;
use crate::ast::EnumInfo;
use crate::ast::Expression;
use crate::ast::Expression_;
use crate::ast::FunInfo;
use crate::ast::MethodInfo;
use crate::ast::MethodKind;
use crate::ast::Pattern;
use crate::ast::Position;
use crate::ast::SourceString;
use crate::ast::Symbol;
use crate::ast::SymbolName;
use crate::ast::SymbolWithType;
use crate::ast::TestInfo;
use crate::ast::ToplevelExpression;
use crate::ast::ToplevelItem;
use crate::ast::TypeName;
use crate::ast::VariantInfo;
use crate::diagnostics::ErrorMessage;
use crate::lex::lex;
use crate::lex::lex_between;
use crate::lex::Token;
use crate::lex::TokenStream;
use crate::lex::INTEGER_RE;
use crate::lex::SYMBOL_RE;

#[derive(Debug)]
#[allow(dead_code)] // additional isn't used yet.
pub enum ParseError {
    Invalid {
        position: Position,
        message: ErrorMessage,
        additional: Vec<(Position, String)>,
    },
    Incomplete {
        message: ErrorMessage,
        position: Position,
    },
}

fn next_token_is(tokens: &TokenStream, token: &str) -> bool {
    tokens.peek().map(|t| t.text == token).unwrap_or(false)
}

fn require_a_token<'a>(
    tokens: &mut TokenStream<'a>,
    token_description: &str,
) -> Result<Token<'a>, ParseError> {
    match tokens.pop() {
        Some(token) => Ok(token),
        None => Err(ParseError::Incomplete {
            message: ErrorMessage(format!("Expected {}, got EOF", token_description)),
            position: Position::todo(),
        }),
    }
}

fn require_token<'a>(
    tokens: &mut TokenStream<'a>,
    expected: &str,
) -> Result<Token<'a>, ParseError> {
    // TODO: If we have an open delimiter, we want the incorrect
    // current token. If we've forgotten a terminator like ; we want
    // the previous token.
    let prev_token = tokens.prev();

    match tokens.pop() {
        Some(token) => {
            if token.text == expected {
                Ok(token)
            } else {
                let position = match prev_token {
                    Some(prev_token) => prev_token.position,
                    None => token.position,
                };

                Err(ParseError::Invalid {
                    position,
                    message: ErrorMessage(format!("Expected `{}`, got `{}`", expected, token.text)),
                    additional: vec![],
                })
            }
        }
        None => Err(ParseError::Incomplete {
            message: ErrorMessage(format!("Expected `{}`, got EOF", expected)),
            position: Position::todo(),
        }),
    }
}

fn parse_integer(tokens: &mut TokenStream) -> Result<Expression, ParseError> {
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

fn parse_variable_expression(tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let variable = parse_symbol(tokens)?;
    Ok(Expression(
        variable.pos.clone(),
        Expression_::Variable(variable),
    ))
}

fn parse_parenthesis_expression(
    src: &str,
    tokens: &mut TokenStream,
) -> Result<Expression, ParseError> {
    require_token(tokens, "(")?;
    let expr = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    Ok(expr)
}

fn parse_list_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let open_brace = require_token(tokens, "[")?;
    let items = parse_comma_separated_exprs(src, tokens, "]")?;
    let close_brace = require_token(tokens, "]")?;

    Ok(Expression(
        Position::merge(open_brace.position, close_brace.position),
        Expression_::ListLiteral(items),
    ))
}

fn parse_lambda_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
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

fn parse_if_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let if_token = require_token(tokens, "if")?;

    require_token(tokens, "(")?;
    let condition = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    let then_body = parse_block(src, tokens)?;

    let else_body: Option<Block> = if next_token_is(tokens, "else") {
        tokens.pop();

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

fn parse_while_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
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

fn parse_return_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let return_token = require_token(tokens, "return")?;

    // TODO: allow `return;`
    let expr = parse_inline_expression(src, tokens)?;
    let _ = require_token(tokens, ";")?;
    Ok(Expression(
        return_token.position,
        Expression_::Return(Box::new(expr)),
    ))
}

fn unescape_string(src: &str) -> String {
    // Trim doublequotes.
    let s = &src[1..src.len() - 1];

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

fn parse_simple_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    if let Some(token) = tokens.peek() {
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

        if SYMBOL_RE.is_match(token.text) {
            return parse_variable_expression(tokens);
        }

        if token.text.starts_with('\"') {
            tokens.pop();
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

    Err(ParseError::Incomplete {
        message: ErrorMessage("Expected an expression".to_owned()),
        position: Position::todo(),
    })
}

fn parse_match_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let match_keyword = require_token(tokens, "match")?;

    require_token(tokens, "(")?;
    let scrutinee = parse_inline_expression(src, tokens)?;
    require_token(tokens, ")")?;

    require_token(tokens, "{")?;

    let mut cases = vec![];
    loop {
        let Some(token) = tokens.peek() else {
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("Invalid syntax: Expected `}` here, but got EOF".to_string()),
            });
        };

        if token.text == "}" {
            break;
        }

        let pattern = parse_pattern(tokens)?;
        require_token(tokens, "=>")?;
        let case_expr = parse_inline_expression(src, tokens)?;
        cases.push((pattern, Box::new(case_expr)));
    }

    require_token(tokens, "}")?;

    Ok(Expression(
        match_keyword.position,
        Expression_::Match(Box::new(scrutinee), cases),
    ))
}

fn parse_pattern(tokens: &mut TokenStream) -> Result<Pattern, ParseError> {
    let symbol = parse_symbol(tokens)?;

    let argument = if next_token_is(tokens, "(") {
        require_token(tokens, "(")?;
        let arg = parse_symbol(tokens)?;
        require_token(tokens, ")")?;
        Some(arg)
    } else {
        None
    };

    Ok(Pattern { symbol, argument })
}

fn parse_comma_separated_exprs(
    src: &str,
    tokens: &mut TokenStream,
    terminator: &str,
) -> Result<Vec<Expression>, ParseError> {
    let mut items = vec![];
    loop {
        if next_token_is(tokens, terminator) {
            break;
        }

        let arg = parse_inline_expression(src, tokens)?;
        items.push(arg);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
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
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(format!(
                    "Invalid syntax: Expected `,` or `{}` here, but got EOF",
                    terminator
                )),
            });
        }
    }

    Ok(items)
}

fn parse_call_arguments(
    src: &str,
    tokens: &mut TokenStream,
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
    tokens: &mut TokenStream,
) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression(src, tokens)?;

    loop {
        match tokens.peek() {
            Some(token) if token.text == "(" => {
                let arguments = parse_call_arguments(src, tokens)?;
                expr = Expression(expr.0.clone(), Expression_::Call(Box::new(expr), arguments));
            }
            Some(token) if token.text == "." => {
                tokens.pop();
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
/// ```garden
/// foo()
/// x + 1
/// if (a) { b } else { c }
/// while (z) { foo(); }
/// ```
fn parse_inline_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    parse_general_expression(src, tokens, true)
}

/// Parse a block member expression. This is an expression that can
/// occur at the top level of braces, such as a let expression.
///
/// Examples:
///
/// ```garden
/// foo();
/// let x = y + 1;
/// if (a) { b; } else { c; }
/// while (z) { foo(); }
/// ```
fn parse_block_member_expression(
    src: &str,
    tokens: &mut TokenStream,
) -> Result<Expression, ParseError> {
    parse_general_expression(src, tokens, false)
}

/// Parse an inline or block member expression.
fn parse_general_expression(
    src: &str,
    tokens: &mut TokenStream,
    is_inline: bool,
) -> Result<Expression, ParseError> {
    if !is_inline {
        // TODO: Matching on tokens will prevent us from doing more
        // complex assignments like `foo.bar = 1;`.
        if let Some((_, token)) = tokens.peek_two() {
            if token.text == "=" {
                return parse_assign_expression(src, tokens);
            }
        }

        if let Some(token) = tokens.peek() {
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

    if let Some(token) = tokens.peek() {
        // `if` can occur as both an inline expression and a standalone
        // expression.
        if token.text == "if" {
            return parse_if_expression(src, tokens);
        }

        // Likewise match.
        if token.text == "match" {
            return parse_match_expression(src, tokens);
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
    tokens: &mut TokenStream,
) -> Result<Expression, ParseError> {
    let mut expr = parse_simple_expression_with_trailing(src, tokens)?;

    if let Some(token) = tokens.peek() {
        if let Some(op) = token_as_binary_op(token) {
            tokens.pop();

            let rhs_expr = parse_simple_expression_with_trailing(src, tokens)?;
            expr = Expression(
                expr.0.clone(),
                Expression_::BinaryOperator(Box::new(expr), op, Box::new(rhs_expr)),
            );
        }
    }

    Ok(expr)
}

fn parse_definition(src: &str, tokens: &mut TokenStream) -> Result<Definition, ParseError> {
    if let Some(token) = tokens.peek() {
        if token.text == "fun" {
            return parse_function_or_method(src, tokens);
        }
        if token.text == "test" {
            return parse_test(src, tokens);
        }
        if token.text == "enum" {
            return parse_enum(src, tokens);
        }

        // TODO: Include the token in the error message.
        return Err(ParseError::Invalid {
            position: token.position,
            message: ErrorMessage("Expected a definition".to_string()),
            additional: vec![],
        });
    }

    Err(ParseError::Incomplete {
        position: Position::todo(),
        message: ErrorMessage("Unfinished definition".to_owned()),
    })
}

fn parse_enum_body(tokens: &mut TokenStream<'_>) -> Result<Vec<VariantInfo>, ParseError> {
    let mut variants = vec![];
    loop {
        if next_token_is(tokens, "}") {
            break;
        }

        let variant = parse_variant(tokens)?;
        variants.push(variant);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == "}" {
                break;
            } else {
                return Err(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `}}` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
            }
        } else {
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `}` here, but got EOF".to_string(),
                ),
            });
        }
    }

    Ok(variants)
}

fn parse_variant(tokens: &mut TokenStream<'_>) -> Result<VariantInfo, ParseError> {
    let name = parse_symbol(tokens)?;

    // Parse the payload argument to this variant, if present.
    let mut has_payload = false;
    if let Some(next_token) = tokens.peek() {
        if next_token.text == "(" {
            tokens.pop();
            parse_symbol(tokens)?;
            require_token(tokens, ")")?;
            has_payload = true;
        }
    }
    let variant = VariantInfo {
        name_sym: name,
        has_payload,
    };
    Ok(variant)
}

fn parse_enum(src: &str, tokens: &mut TokenStream<'_>) -> Result<Definition, ParseError> {
    let enum_token = require_token(tokens, "enum")?;
    let doc_comment = parse_doc_comment(&enum_token);
    let name = parse_type_name(tokens)?;

    let _open_brace = require_token(tokens, "{")?;

    let variants = parse_enum_body(tokens)?;

    let close_brace = require_token(tokens, "}")?;

    let mut start_offset = enum_token.position.start_offset;
    if let Some((comment_pos, _)) = enum_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = close_brace.position.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    Ok(Definition(
        src_string.clone(),
        enum_token.position,
        Definition_::Enum(EnumInfo {
            src_string,
            doc_comment,
            name,
            variants,
        }),
    ))
}

fn parse_test(src: &str, tokens: &mut TokenStream) -> Result<Definition, ParseError> {
    let test_token = require_token(tokens, "test")?;
    let doc_comment = parse_doc_comment(&test_token);

    let name = if let Some(token) = tokens.peek() {
        if token.text == "{" {
            None
        } else {
            Some(parse_symbol(tokens)?)
        }
    } else {
        return Err(ParseError::Incomplete {
            position: Position::todo(),
            message: ErrorMessage("Unfinished test definition".to_owned()),
        });
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
        Definition_::Test(TestInfo {
            src_string,
            doc_comment,
            name,
            body,
        }),
    ))
}

fn parse_type_name(tokens: &mut TokenStream) -> Result<TypeName, ParseError> {
    let name = parse_symbol(tokens)?;
    Ok(TypeName(name.name.0))
}

fn parse_type_annotation(tokens: &mut TokenStream) -> Result<Option<TypeName>, ParseError> {
    if let Some(token) = tokens.peek() {
        if token.text == ":" {
            tokens.pop();
            return Ok(Some(parse_type_name(tokens)?));
        }
    }

    Ok(None)
}

fn parse_parameter(tokens: &mut TokenStream) -> Result<SymbolWithType, ParseError> {
    let param = parse_symbol(tokens)?;
    let param_type = parse_type_annotation(tokens)?;
    Ok(SymbolWithType {
        symbol: param,
        type_: param_type,
    })
}

fn parse_parameters(tokens: &mut TokenStream) -> Result<Vec<SymbolWithType>, ParseError> {
    require_token(tokens, "(")?;

    let mut params = vec![];
    loop {
        if next_token_is(tokens, ")") {
            break;
        }

        let param = parse_parameter(tokens)?;
        params.push(param);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
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
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `)` here, but got EOF".to_string(),
                ),
            });
        }
    }

    require_token(tokens, ")")?;

    // Emit error if there are duplicate parameters.
    // TODO: allow parsing to return an AST even if errors are present.
    let mut seen = HashSet::new();
    for param in &params {
        let param_name = &param.symbol.name.0;
        if param.symbol.name.is_underscore() {
            continue;
        }

        if seen.contains(param_name) {
            return Err(ParseError::Invalid {
                position: param.symbol.pos.clone(),
                message: ErrorMessage(format!("Duplicate parameter: `{}`", param_name)),
                // TODO: report the position of the previous parameter too.
                additional: vec![],
            });
        } else {
            seen.insert(param_name.clone());
        }
    }

    Ok(params)
}

fn parse_block(src: &str, tokens: &mut TokenStream) -> Result<Block, ParseError> {
    let open_brace = require_token(tokens, "{")?;

    let mut exprs = vec![];
    loop {
        if let Some(token) = tokens.peek() {
            if token.text == "}" {
                break;
            }
        } else {
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("Invalid syntax: Expected `}` here, but got EOF".to_string()),
            });
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

fn parse_function_or_method(src: &str, tokens: &mut TokenStream) -> Result<Definition, ParseError> {
    match tokens.peek_two() {
        Some((_, second_token)) => {
            if second_token.text == "(" {
                parse_method(src, tokens)
            } else {
                parse_function(src, tokens)
            }
        }
        None => Err(ParseError::Incomplete {
            position: Position::todo(),
            message: ErrorMessage("Unfinished function or method definition.".to_owned()),
        }),
    }
}

fn parse_method(src: &str, tokens: &mut TokenStream) -> Result<Definition, ParseError> {
    let fun_token = require_token(tokens, "fun")?;
    let doc_comment = parse_doc_comment(&fun_token);

    require_token(tokens, "(")?;
    let receiver_param = parse_parameter(tokens)?;
    let receiver_name = receiver_param.symbol.name;
    let receiver_type = match receiver_param.type_ {
        Some(type_name) => type_name,
        None => {
            return Err(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("A type name for this method is expected.".to_owned()),
            });
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
        name_sym: name,
        kind: MethodKind::UserDefinedMethod(fun_info),
    };

    Ok(Definition(
        src_string.clone(),
        fun_token.position,
        Definition_::Method(meth_info),
    ))
}

fn parse_function(src: &str, tokens: &mut TokenStream) -> Result<Definition, ParseError> {
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
        Definition_::Fun(
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
    "let", "fun", "enum", "if", "else", "while", "return", "test", "match",
];

fn parse_symbol(tokens: &mut TokenStream) -> Result<Symbol, ParseError> {
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

    Ok(Symbol {
        pos: variable_token.position,
        name: SymbolName(variable_token.text.to_string()),
    })
}

fn parse_let_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
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

fn parse_assign_expression(src: &str, tokens: &mut TokenStream) -> Result<Expression, ParseError> {
    let variable = parse_symbol(tokens)?;

    require_token(tokens, "=")?;
    let expr = parse_inline_expression(src, tokens)?;
    let _ = require_token(tokens, ";")?;

    Ok(Expression(
        variable.pos.clone(),
        Expression_::Assign(variable, Box::new(expr)),
    ))
}

fn parse_toplevel_expr(src: &str, tokens: &mut TokenStream) -> Result<ToplevelItem, ParseError> {
    let initial_token_idx = tokens.idx;

    if let Ok(expr) = parse_block_member_expression(src, tokens) {
        let pos = &expr.0.clone();
        let toplevel_expr =
            ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
        return Ok(ToplevelItem::Expr(toplevel_expr));
    }

    tokens.idx = initial_token_idx;
    let expr = parse_inline_expression(src, tokens)?;
    let pos = &expr.0.clone();
    let toplevel_expr = ToplevelExpression(src[pos.start_offset..pos.end_offset].to_owned(), expr);
    Ok(ToplevelItem::Expr(toplevel_expr))
}

fn parse_toplevel_items_from_tokens(
    src: &str,
    tokens: &mut TokenStream,
) -> Result<Vec<ToplevelItem>, ParseError> {
    let mut items: Vec<ToplevelItem> = vec![];

    while !tokens.is_empty() {
        let item = parse_toplevel_item_from_tokens(src, tokens)?;
        items.push(item);
    }
    Ok(items)
}

fn parse_toplevel_item_from_tokens(
    src: &str,
    tokens: &mut TokenStream,
) -> Result<ToplevelItem, ParseError> {
    if let Some(token) = tokens.peek() {
        if token.text == "fun" || token.text == "test" || token.text == "enum" {
            let def = parse_definition(src, tokens)?;
            return Ok(ToplevelItem::Def(def));
        }
    }

    parse_toplevel_expr(src, tokens)
}

pub fn parse_inline_expr_from_str(path: &Path, src: &str) -> Result<Expression, ParseError> {
    let mut tokens = lex(path, src)?;
    parse_inline_expression(src, &mut tokens)
}

pub fn parse_toplevel_item(path: &Path, src: &str) -> Result<ToplevelItem, ParseError> {
    let items = parse_toplevel_items(path, src)?;
    Ok(items[0].clone())
}

pub fn parse_toplevel_items(path: &Path, src: &str) -> Result<Vec<ToplevelItem>, ParseError> {
    let mut tokens = lex(path, src)?;
    parse_toplevel_items_from_tokens(src, &mut tokens)
}

pub fn parse_toplevel_items_from_span(
    path: &Path,
    src: &str,
    offset: usize,
    end_offset: usize,
) -> Result<Vec<ToplevelItem>, ParseError> {
    let mut tokens = lex_between(path, src, offset, end_offset)?;
    parse_toplevel_items_from_tokens(src, &mut tokens)
}

pub fn parse_exprs_from_str(src: &str) -> Result<Vec<Expression>, ParseError> {
    use std::path::PathBuf;

    let mut tokens = lex(&PathBuf::from("__test.gdn"), src)?;

    let mut res = vec![];
    while !tokens.is_empty() {
        res.push(parse_block_member_expression(src, &mut tokens)?);
    }

    Ok(res)
}

pub fn parse_defs_from_str(src: &str) -> Result<Vec<Definition>, ParseError> {
    use std::path::PathBuf;

    let items = parse_toplevel_items(&PathBuf::from("__test.gdn"), src)?;

    let mut defs = vec![];
    for item in items {
        match item {
            ToplevelItem::Def(def) => {
                defs.push(def.clone());
            }
            ToplevelItem::Expr(_) => unreachable!(),
        }
    }

    Ok(defs)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

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
                Expression_::Variable(Symbol {
                    pos: Position {
                        start_offset: 0,
                        end_offset: 7,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn")
                    },
                    name: SymbolName("abc_def".to_string())
                })
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
                    Symbol {
                        pos: Position {
                            start_offset: 4,
                            end_offset: 5,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        name: SymbolName("x".into())
                    },
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
                    Symbol {
                        pos: Position {
                            start_offset: 4,
                            end_offset: 5,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        name: SymbolName("A".into())
                    },
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
    fn test_parse_match() {
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("match (1) {}").unwrap();

        assert_eq!(
            ast,
            vec![Expression(
                Position {
                    start_offset: 0,
                    end_offset: 5,
                    line_number: 0,
                    path: path.clone(),
                },
                Expression_::Match(
                    Box::new(Expression(
                        Position {
                            start_offset: 7,
                            end_offset: 8,
                            line_number: 0,
                            path
                        },
                        Expression_::IntLiteral(1)
                    )),
                    vec![]
                )
            )]
        );
    }

    #[test]
    fn test_parse_if_else() {
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("if (True) {} else {}").unwrap();

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
                        Expression_::Variable(Symbol {
                            pos: Position {
                                start_offset: 4,
                                end_offset: 8,
                                line_number: 0,
                                path: path.clone()
                            },
                            name: SymbolName("True".into())
                        })
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
                        exprs: vec![],
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
                        exprs: vec![],
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
                        Expression_::Variable(Symbol {
                            pos: Position {
                                start_offset: 4,
                                end_offset: 5,
                                line_number: 0,
                                path: path.clone()
                            },
                            name: SymbolName("x".into())
                        })
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
                        exprs: vec![],
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
                                    Expression_::Variable(Symbol {
                                        pos: Position {
                                            start_offset: 19,
                                            end_offset: 20,
                                            line_number: 0,
                                            path: path.clone()
                                        },
                                        name: SymbolName("y".into())
                                    })
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
                                    exprs: vec![],
                                },
                                None,
                            )
                        )],
                    }),
                )
            )]
        );
    }

    #[test]
    fn test_parse_if() {
        let path = PathBuf::from("__test.gdn");
        let ast = parse_exprs_from_str("if (True) {}").unwrap();

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
                        Expression_::Variable(Symbol {
                            pos: Position {
                                start_offset: 4,
                                end_offset: 8,
                                line_number: 0,
                                path: path.clone()
                            },
                            name: SymbolName("True".into())
                        })
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
                        exprs: vec![],
                    },
                    None,
                )
            )]
        );
    }

    #[test]
    fn test_parse_return() {
        let ast = parse_exprs_from_str("return x;").unwrap();

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
                        end_offset: 8,
                        line_number: 0,
                        path: PathBuf::from("__test.gdn")
                    },
                    Expression_::Variable(Symbol {
                        pos: Position {
                            start_offset: 7,
                            end_offset: 8,
                            line_number: 0,
                            path: PathBuf::from("__test.gdn")
                        },
                        name: SymbolName("x".into())
                    })
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
                                Expression_::Variable(Symbol {
                                    pos: Position {
                                        start_offset: 0,
                                        end_offset: 3,
                                        line_number: 0,
                                        path: PathBuf::from("__test.gdn")
                                    },
                                    name: SymbolName("foo".into())
                                })
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
                Definition_::Fun(
                    Symbol {
                        pos: Position {
                            start_offset: 22,
                            end_offset: 25,
                            line_number: 2,
                            path: path.clone(),
                        },
                        name: SymbolName("foo".into())
                    },
                    FunInfo {
                        src_string: SourceString {
                            offset: 0,
                            src: "// Hello\n// World\nfun foo() {}".to_owned()
                        },
                        doc_comment: Some("Hello\nWorld".into()),
                        name: Some(Symbol {
                            pos: Position {
                                start_offset: 22,
                                end_offset: 25,
                                line_number: 2,
                                path: path.clone(),
                            },
                            name: SymbolName("foo".into())
                        }),
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
                            exprs: vec![],
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
        assert!(matches!(defs[0].2, Definition_::Method(_)));
    }

    #[test]
    fn test_parse_fun_def_and_block() {
        let src = "fun foo() {} {}";
        let defs = parse_toplevel_items(&PathBuf::new(), src).unwrap();

        assert_eq!(defs.len(), 2);
    }

    #[test]
    fn test_incomplete_expression() {
        assert!(parse_toplevel_items(&PathBuf::from("__test.gdn"), "1 + ").is_err());
    }

    #[test]
    fn test_parse_block_expression() {
        let ast = match parse_toplevel_item(&PathBuf::from("__test.gdn"), "let x = 1;").unwrap() {
            ToplevelItem::Def(_) => unreachable!(),
            ToplevelItem::Expr(e) => e,
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
                        Symbol {
                            pos: Position {
                                start_offset: 4,
                                end_offset: 5,
                                line_number: 0,
                                path: PathBuf::from("__test.gdn")
                            },
                            name: SymbolName("x".into())
                        },
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

    #[test]
    fn test_repeated_param() {
        assert!(parse_toplevel_items(&PathBuf::from("__test.gdn"), "fun f(x, x) {}").is_err());
    }

    #[test]
    fn test_repeated_param_underscore() {
        assert!(parse_toplevel_items(&PathBuf::from("__test.gdn"), "fun f(_, _) {}").is_ok());
    }
}
