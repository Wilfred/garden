#![warn(clippy::todo)]
#![warn(clippy::dbg_macro)]
#![allow(clippy::collapsible_else_if)]
// Common in unfinished code.
#![allow(clippy::if_same_then_else)]

pub mod ast;
pub mod diagnostics;
pub mod lex;
pub mod position;
pub mod visitor;

use std::collections::HashSet;
use std::path::Path;

use ast::FieldInfo;
use ast::StructInfo;
use position::Position;

use crate::ast::*;
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

fn peeked_symbol_is(tokens: &TokenStream, token: &str) -> bool {
    tokens.peek().map(|t| t.text == token).unwrap_or(false)
}

fn require_a_token<'a>(
    tokens: &mut TokenStream<'a>,
    diagnostics: &mut Vec<ParseError>,
    token_description: &str,
) -> Token<'a> {
    match tokens.pop() {
        Some(token) => token,
        None => {
            diagnostics.push(ParseError::Incomplete {
                message: ErrorMessage(format!("Expected {}, got EOF", token_description)),
                position: Position::todo(),
            });

            // TODO: this is arbitrarily choosing the previous token,
            // which is a horrible hack. It would be better to have a
            // way of generating placeholder tokens.
            tokens.prev().expect("TODO: handle empty token streams")
        }
    }
}

fn require_token<'a>(
    tokens: &mut TokenStream<'a>,
    diagnostics: &mut Vec<ParseError>,
    expected: &str,
) -> Token<'a> {
    match tokens.pop() {
        Some(token) => {
            if token.text != expected {
                let position = token.position.clone();

                diagnostics.push(ParseError::Invalid {
                    position,
                    message: ErrorMessage(format!("Expected `{}`, got `{}`", expected, token.text)),
                    additional: vec![],
                });

                // Undo the pop. We saw an unexpected token, so it
                // might be e.g. a close brace so we shouldn't just
                // discard it.
                tokens.unpop();
            }

            token
        }
        None => {
            diagnostics.push(ParseError::Incomplete {
                message: ErrorMessage(format!("Expected `{}`, got EOF", expected)),
                position: Position::todo(),
            });

            tokens.prev().expect("TODO: handle empty file properly")
        }
    }
}

fn parse_integer(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let token = require_a_token(tokens, diagnostics, "integer literal");

    if INTEGER_RE.is_match(token.text) {
        let i: i64 = token.text.parse().unwrap();
        Expression::new(token.position, Expression_::IntLiteral(i), id_gen.next())
    } else {
        diagnostics.push(ParseError::Invalid {
            position: token.position.clone(),
            message: ErrorMessage(format!("Not a valid integer literal: {}", token.text)),
            additional: vec![],
        });

        // Choose an arbitrary value that's hopefully unlikely to
        // occur in real code.
        Expression::new(
            token.position,
            Expression_::IntLiteral(11223344),
            id_gen.next(),
        )
    }
}

fn parse_variable_expression(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    Expression::new(
        variable.position.clone(),
        Expression_::Variable(variable),
        id_gen.next(),
    )
}

fn parse_parenthesis_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    require_token(tokens, diagnostics, "(");
    let expr = parse_expression(src, tokens, id_gen, diagnostics);
    require_token(tokens, diagnostics, ")");

    expr
}

fn parse_list_literal(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let open_bracket = require_token(tokens, diagnostics, "[");
    let items = parse_comma_separated_exprs(src, tokens, id_gen, diagnostics, "]");
    let close_bracket = require_token(tokens, diagnostics, "]");

    Expression::new(
        Position::merge(&open_bracket.position, &close_bracket.position),
        Expression_::ListLiteral(items),
        id_gen.next(),
    )
}

fn parse_lambda_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let fun_keyword = require_token(tokens, diagnostics, "fun");
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let params = parse_parameters(tokens, id_gen, diagnostics);
    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(src, tokens, id_gen, diagnostics, false);

    let start_offset = fun_keyword.position.start_offset;
    let end_offset = body.close_brace.end_offset;
    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    Expression::new(
        Position::merge(&fun_keyword.position, &body.close_brace),
        Expression_::FunLiteral(FunInfo {
            src_string,
            params,
            body,
            doc_comment: None,
            name: None,
            type_params,
            return_hint,
        }),
        id_gen.next(),
    )
}

fn parse_if_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let if_token = require_token(tokens, diagnostics, "if");

    let open_paren = require_token(tokens, diagnostics, "(");
    let cond_expr = parse_expression(src, tokens, id_gen, diagnostics);
    let close_paren = require_token(tokens, diagnostics, ")");
    let condition = ParenthesizedExpression {
        open_paren: open_paren.position,
        expr: Box::new(cond_expr),
        close_paren: close_paren.position,
    };

    let then_body = parse_block(src, tokens, id_gen, diagnostics, false);

    let else_body: Option<Block> = if peeked_symbol_is(tokens, "else") {
        tokens.pop();

        if peeked_symbol_is(tokens, "if") {
            let if_expr = parse_if_expression(src, tokens, id_gen, diagnostics);
            Some(Block {
                // TODO: when there is a chain of if/else if
                // expressions, the open brace isn't meaningful. This
                // is an ugly hack.
                open_brace: if_expr.pos.clone(),
                close_brace: if_expr.pos.clone(),
                exprs: vec![if_expr],
                is_loop_body: false,
            })
        } else {
            Some(parse_block(src, tokens, id_gen, diagnostics, false))
        }
    } else {
        None
    };

    let last_brace_pos = match &else_body {
        Some(else_body) => &else_body.close_brace,
        None => &then_body.close_brace,
    };

    Expression::new(
        Position::merge(&if_token.position, last_brace_pos),
        Expression_::If(condition, then_body, else_body),
        id_gen.next(),
    )
}

fn parse_while_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let while_token = require_token(tokens, diagnostics, "while");

    let open_paren = require_token(tokens, diagnostics, "(");
    let cond_expr = parse_expression(src, tokens, id_gen, diagnostics);
    let close_paren = require_token(tokens, diagnostics, ")");
    let condition = ParenthesizedExpression {
        open_paren: open_paren.position,
        expr: Box::new(cond_expr),
        close_paren: close_paren.position,
    };

    let body = parse_block(src, tokens, id_gen, diagnostics, true);

    Expression::new(
        Position::merge(&while_token.position, &body.close_brace),
        Expression_::While(condition, body),
        id_gen.next(),
    )
}

fn parse_break_expression(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let break_token = require_token(tokens, diagnostics, "break");
    Expression::new(break_token.position, Expression_::Break, id_gen.next())
}

fn parse_return_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let return_token = require_token(tokens, diagnostics, "return");
    let expr = parse_expression(src, tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&return_token.position, &expr.pos),
        Expression_::Return(Some(Box::new(expr))),
        id_gen.next(),
    )
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

fn parse_simple_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    if let Some(token) = tokens.peek() {
        if token.text == "{" {
            let block = parse_block(src, tokens, id_gen, diagnostics, false);

            return Expression::new(
                Position::merge(&block.open_brace, &block.close_brace),
                Expression_::Block(block),
                id_gen.next(),
            );
        }

        if token.text == "(" {
            return parse_parenthesis_expression(src, tokens, id_gen, diagnostics);
        }

        if token.text == "[" {
            return parse_list_literal(src, tokens, id_gen, diagnostics);
        }

        if token.text == "fun" {
            return parse_lambda_expression(src, tokens, id_gen, diagnostics);
        }

        if SYMBOL_RE.is_match(token.text) {
            if let Some((_, token)) = tokens.peek_two() {
                if token.text == "{" {
                    return parse_struct_literal(src, tokens, id_gen, diagnostics);
                }
            }

            return parse_variable_expression(tokens, id_gen, diagnostics);
        }

        if token.text.starts_with('\"') {
            tokens.pop();

            return Expression::new(
                token.position,
                Expression_::StringLiteral(unescape_string(token.text)),
                id_gen.next(),
            );
        }

        if INTEGER_RE.is_match(token.text) {
            return parse_integer(tokens, id_gen, diagnostics);
        }

        diagnostics.push(ParseError::Invalid {
            position: token.position.clone(),
            message: ErrorMessage(format!("Expected an expression, got: `{}`.", token.text)),
            additional: vec![],
        });

        return Expression::new(token.position, Expression_::Invalid, id_gen.next());
    }

    diagnostics.push(ParseError::Incomplete {
        message: ErrorMessage("Expected an expression.".to_owned()),
        position: Position::todo(),
    });

    Expression::new(Position::todo(), Expression_::Invalid, id_gen.next())
}

fn parse_struct_literal_fields(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<(Symbol, Expression)> {
    let mut fields = vec![];
    loop {
        if peeked_symbol_is(tokens, "}") {
            break;
        }

        let start_idx = tokens.idx;
        let sym = parse_symbol(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, ":");
        let expr = parse_expression(src, tokens, id_gen, diagnostics);

        if tokens.idx == start_idx {
            // We haven't made forward progress, the syntax must be
            // very broken. Give up on this struct, consuming until
            // the closing brace.
            while let Some(t) = tokens.peek() {
                if t.text == "}" {
                    break;
                } else {
                    tokens.pop();
                }
            }
            break;
        }

        fields.push((sym, expr));

        let Some(token) = tokens.peek() else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `}` here, but got EOF".to_string(),
                ),
            });
            break;
        };

        if token.text == "," {
            tokens.pop();
        }
    }

    fields
}

fn parse_struct_literal(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let name = parse_type_symbol(tokens, id_gen, diagnostics);
    let open_brace = require_token(tokens, diagnostics, "{");
    let fields = parse_struct_literal_fields(src, tokens, id_gen, diagnostics);

    let close_brace = require_token(tokens, diagnostics, "}");

    Expression::new(
        Position::merge(&open_brace.position, &close_brace.position),
        Expression_::StructLiteral(name, fields),
        id_gen.next(),
    )
}

fn parse_match_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let match_keyword = require_token(tokens, diagnostics, "match");

    let open_paren = require_token(tokens, diagnostics, "(");
    let scrutinee_expr = parse_expression(src, tokens, id_gen, diagnostics);
    let close_paren = require_token(tokens, diagnostics, ")");
    let scrutinee = ParenthesizedExpression {
        open_paren: open_paren.position,
        expr: Box::new(scrutinee_expr),
        close_paren: close_paren.position,
    };

    require_token(tokens, diagnostics, "{");

    let mut cases = vec![];
    loop {
        let Some(token) = tokens.peek() else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("Invalid syntax: Expected `}` here, but got EOF".to_string()),
            });
            break;
        };

        if token.text == "}" {
            break;
        }

        let start_idx = tokens.idx;
        let pattern = parse_pattern(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, "=>");
        let case_expr = parse_case_expr(src, tokens, id_gen, diagnostics);
        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );

        cases.push((pattern, Box::new(case_expr)));
    }

    let close_paren = require_token(tokens, diagnostics, "}");

    Expression::new(
        Position::merge(&match_keyword.position, &close_paren.position),
        Expression_::Match(scrutinee, cases),
        id_gen.next(),
    )
}

fn parse_case_expr(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let case_expr = parse_expression(src, tokens, id_gen, diagnostics);
    if peeked_symbol_is(tokens, ",") {
        tokens.pop().unwrap();
    }

    case_expr
}

fn parse_pattern(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Pattern {
    let symbol = parse_symbol(tokens, id_gen, diagnostics);

    let argument = if peeked_symbol_is(tokens, "(") {
        require_token(tokens, diagnostics, "(");
        let arg = parse_symbol(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, ")");
        Some(arg)
    } else {
        None
    };

    Pattern { symbol, argument }
}

fn parse_comma_separated_exprs(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
    terminator: &str,
) -> Vec<Expression> {
    let mut items = vec![];
    loop {
        if peeked_symbol_is(tokens, terminator) {
            break;
        }

        let start_idx = tokens.idx;
        let arg = parse_expression(src, tokens, id_gen, diagnostics);
        if arg.expr_.is_invalid_or_placeholder() {
            break;
        }

        items.push(arg);
        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text != terminator {
                diagnostics.push(ParseError::Invalid {
                    position: token.position.clone(),
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `{}`, got `{}`",
                        terminator, token.text
                    )),
                    additional: vec![],
                });
                continue;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(format!(
                    "Invalid syntax: Expected `,` or `{}` here, but got EOF",
                    terminator
                )),
            });
            break;
        }
    }

    items
}

fn parse_call_arguments(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ParenthesizedArguments {
    let open_paren_token = require_token(tokens, diagnostics, "(");
    let arguments = parse_comma_separated_exprs(src, tokens, id_gen, diagnostics, ")");
    let close_paren_token = require_token(tokens, diagnostics, ")");

    ParenthesizedArguments {
        arguments,
        open_paren: open_paren_token.position,
        close_paren: close_paren_token.position,
    }
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
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let mut expr = parse_simple_expression(src, tokens, id_gen, diagnostics);

    loop {
        let start_idx = tokens.idx;
        match tokens.peek() {
            Some(token) if token.text == "(" => {
                let arguments = parse_call_arguments(src, tokens, id_gen, diagnostics);

                expr = Expression::new(
                    Position::merge(&expr.pos, &arguments.close_paren),
                    Expression_::Call(Box::new(expr), arguments),
                    id_gen.next(),
                );
            }
            Some(token) if token.text == "." => {
                tokens.pop();
                let variable = parse_symbol(tokens, id_gen, diagnostics);

                if peeked_symbol_is(tokens, "(") {
                    // TODO: just treat a method call as a call of a dot access.
                    let arguments = parse_call_arguments(src, tokens, id_gen, diagnostics);

                    expr = Expression::new(
                        Position::merge(&expr.pos, &arguments.close_paren),
                        Expression_::MethodCall(Box::new(expr), variable, arguments),
                        id_gen.next(),
                    );
                } else {
                    expr = Expression::new(
                        Position::merge(&expr.pos, &variable.position),
                        Expression_::DotAccess(Box::new(expr), variable),
                        id_gen.next(),
                    );
                }
            }
            _ => break,
        }
        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );
    }

    expr
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

/// Parse an expression.
///
/// Examples:
///
/// ```garden
/// foo()
/// let x = y + 1
/// if (a) { b } else { c }
/// while (z) { foo() }
/// ```
fn parse_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    // TODO: Matching on tokens will prevent us from doing more
    // complex assignments like `foo.bar = 1`.
    if let Some((_, token)) = tokens.peek_two() {
        if token.text == "=" {
            return parse_assign_expression(src, tokens, id_gen, diagnostics);
        }
    }

    if let Some(token) = tokens.peek() {
        if token.text == "let" {
            return parse_let_expression(src, tokens, id_gen, diagnostics);
        }
        if token.text == "return" {
            return parse_return_expression(src, tokens, id_gen, diagnostics);
        }
        if token.text == "while" {
            return parse_while_expression(src, tokens, id_gen, diagnostics);
        }
        if token.text == "break" {
            return parse_break_expression(tokens, id_gen, diagnostics);
        }
        if token.text == "if" {
            return parse_if_expression(src, tokens, id_gen, diagnostics);
        }
        if token.text == "match" {
            return parse_match_expression(src, tokens, id_gen, diagnostics);
        }
    }

    parse_simple_expression_or_binop(src, tokens, id_gen, diagnostics)
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
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let mut expr = parse_simple_expression_with_trailing(src, tokens, id_gen, diagnostics);

    if let Some(token) = tokens.peek() {
        if let Some(op) = token_as_binary_op(token) {
            tokens.pop();

            let rhs_expr = parse_simple_expression_with_trailing(src, tokens, id_gen, diagnostics);

            expr = Expression::new(
                Position::merge(&expr.pos, &rhs_expr.pos),
                Expression_::BinaryOperator(Box::new(expr), op, Box::new(rhs_expr)),
                id_gen.next(),
            );
        }
    }

    expr
}

fn parse_definition(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<Definition> {
    if let Some(token) = tokens.peek() {
        if token.text == "fun" {
            return parse_function_or_method(src, tokens, id_gen, diagnostics);
        }
        if token.text == "test" {
            return Some(parse_test(src, tokens, id_gen, diagnostics));
        }
        if token.text == "enum" {
            return Some(parse_enum(src, tokens, id_gen, diagnostics));
        }
        if token.text == "struct" {
            return Some(parse_struct(src, tokens, id_gen, diagnostics));
        }

        // TODO: Include the token in the error message.
        diagnostics.push(ParseError::Invalid {
            position: token.position,
            message: ErrorMessage("Expected a definition".to_string()),
            additional: vec![],
        });
        return None;
    }

    diagnostics.push(ParseError::Incomplete {
        position: Position::todo(),
        message: ErrorMessage("Unfinished definition".to_owned()),
    });
    None
}

fn parse_enum_body(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<VariantInfo> {
    let mut variants = vec![];
    loop {
        if peeked_symbol_is(tokens, "}") {
            break;
        }

        let variant = parse_variant(tokens, id_gen, diagnostics);
        variants.push(variant);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == "}" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `}}` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `}` here, but got EOF".to_string(),
                ),
            });
            break;
        }
    }

    variants
}

/// Parse enum variant, e.g. `Some(T)`.
fn parse_variant(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> VariantInfo {
    let name = parse_symbol(tokens, id_gen, diagnostics);

    let mut payload_hint = None;
    if peeked_symbol_is(tokens, "(") {
        tokens.pop();
        payload_hint = Some(parse_type_hint(tokens, id_gen, diagnostics));
        require_token(tokens, diagnostics, ")");
    }

    VariantInfo {
        name_sym: name,
        payload_hint,
    }
}

fn parse_enum(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Definition {
    let enum_token = require_token(tokens, diagnostics, "enum");
    let doc_comment = parse_doc_comment(&enum_token);
    let name_sym = parse_type_symbol(tokens, id_gen, diagnostics);
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let _open_brace = require_token(tokens, diagnostics, "{");

    let variants = parse_enum_body(tokens, id_gen, diagnostics);

    let close_brace = require_token(tokens, diagnostics, "}");

    let mut start_offset = enum_token.position.start_offset;
    if let Some((comment_pos, _)) = enum_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = close_brace.position.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let position = Position::merge(&enum_token.position, &close_brace.position);

    Definition(
        src_string.clone(),
        position,
        Definition_::Enum(EnumInfo {
            src_string,
            doc_comment,
            name_sym,
            type_params,
            variants,
        }),
    )
}

fn parse_struct(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Definition {
    let struct_token = require_token(tokens, diagnostics, "struct");
    let doc_comment = parse_doc_comment(&struct_token);
    let name_sym = parse_type_symbol(tokens, id_gen, diagnostics);
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let _open_brace = require_token(tokens, diagnostics, "{");

    let fields = parse_struct_fields(tokens, id_gen, diagnostics);

    let close_brace = require_token(tokens, diagnostics, "}");

    let mut start_offset = struct_token.position.start_offset;
    if let Some((comment_pos, _)) = struct_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = close_brace.position.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let position = Position::merge(&struct_token.position, &close_brace.position);

    Definition(
        src_string.clone(),
        position,
        Definition_::Struct(StructInfo {
            src_string,
            doc_comment,
            name_sym,
            type_params,
            fields,
        }),
    )
}

fn parse_test(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Definition {
    let test_token = require_token(tokens, diagnostics, "test");
    let doc_comment = parse_doc_comment(&test_token);

    let name = if let Some(token) = tokens.peek() {
        if token.text == "{" {
            None
        } else {
            Some(parse_symbol(tokens, id_gen, diagnostics))
        }
    } else {
        diagnostics.push(ParseError::Incomplete {
            position: test_token.position.clone(),
            message: ErrorMessage("Unfinished test definition".to_owned()),
        });
        None
    };

    let body = parse_block(src, tokens, id_gen, diagnostics, false);

    let mut start_offset = test_token.position.start_offset;
    if let Some((comment_pos, _)) = test_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let position = Position::merge(&test_token.position, &body.close_brace);

    Definition(
        src_string.clone(),
        position,
        Definition_::Test(TestInfo {
            src_string,
            doc_comment,
            name,
            body,
        }),
    )
}

fn parse_type_symbol(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeSymbol {
    let name = parse_symbol(tokens, id_gen, diagnostics);
    TypeSymbol {
        name: TypeName { name: name.name.0 },
        position: name.position,
        id: id_gen.next(),
    }
}

/// Parse (possibly nested) type arguments, e.g. `<Int, T, Option<String>>`.
fn parse_type_arguments(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> (Vec<TypeHint>, Option<Position>) {
    if !peeked_symbol_is(tokens, "<") {
        return (vec![], None);
    }

    require_token(tokens, diagnostics, "<");

    let mut args = vec![];
    let close_pos = loop {
        if let Some(token) = tokens.peek() {
            if token.text == ">" {
                break token.position;
            }
        }
        let arg = parse_type_hint(tokens, id_gen, diagnostics);
        args.push(arg);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ">" {
                break token.position;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position.clone(),
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `>` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
                break token.position;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `>` here, but got EOF".to_owned(),
                ),
            });
            break Position::todo();
        }
    };

    require_token(tokens, diagnostics, ">");

    (args, Some(close_pos))
}

/// Parse type parameters for this definition, e.g. `<T, E>`.
fn parse_type_params(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<TypeSymbol> {
    if !peeked_symbol_is(tokens, "<") {
        return vec![];
    }

    require_token(tokens, diagnostics, "<");

    let mut params = vec![];
    loop {
        if peeked_symbol_is(tokens, ">") {
            break;
        }

        let arg = parse_type_symbol(tokens, id_gen, diagnostics);
        params.push(arg);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ">" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `>` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `>` here, but got EOF".to_owned(),
                ),
            });
            break;
        }
    }

    require_token(tokens, diagnostics, ">");

    params
}

/// Parse a tuple type hint, e.g. `(Int, String, Unit)`. Treat it as
/// syntactic sugar for `Tuple<Int, String, Unit>`.
fn parse_tuple_type_hint(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeHint {
    let open_paren = require_token(tokens, diagnostics, "(");

    let mut item_hints = vec![];
    loop {
        if peeked_symbol_is(tokens, ")") {
            break;
        }

        item_hints.push(parse_type_hint(tokens, id_gen, diagnostics));

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `)` here, but got EOF".to_owned(),
                ),
            });
            break;
        }
    }

    let close_paren = require_token(tokens, diagnostics, ")");

    TypeHint {
        sym: TypeSymbol {
            name: TypeName {
                name: "Tuple".to_owned(),
            },
            position: open_paren.position.clone(),
            id: id_gen.next(),
        },
        args: item_hints,
        position: Position::merge(&open_paren.position, &close_paren.position),
    }
}

/// Parse a type hint, such as `String`, `List<Foo>` or `(Int, T)`.
fn parse_type_hint(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeHint {
    if peeked_symbol_is(tokens, "(") {
        return parse_tuple_type_hint(tokens, id_gen, diagnostics);
    }

    let sym = parse_type_symbol(tokens, id_gen, diagnostics);
    let (args, close_pos) = parse_type_arguments(tokens, id_gen, diagnostics);

    let position = match close_pos {
        Some(close_pos) => Position::merge(&sym.position, &close_pos),
        None => sym.position.clone(),
    };

    TypeHint {
        sym,
        args,
        position,
    }
}

/// Parse a colon and a type hint, e.g. `: Int`.
fn parse_colon_and(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeHint {
    require_token(tokens, diagnostics, ":");
    parse_type_hint(tokens, id_gen, diagnostics)
}

/// Parse a type annotation, if present.
fn parse_colon_and_hint_opt(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<TypeHint> {
    if peeked_symbol_is(tokens, ":") {
        let type_hint = parse_colon_and(tokens, id_gen, diagnostics);
        return Some(type_hint);
    }

    None
}

fn parse_parameter(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
    require_type_hint: bool,
) -> SymbolWithHint {
    let param = parse_symbol(tokens, id_gen, diagnostics);

    let hint = if require_type_hint {
        Some(parse_colon_and(tokens, id_gen, diagnostics))
    } else {
        parse_colon_and_hint_opt(tokens, id_gen, diagnostics)
    };

    SymbolWithHint {
        symbol: param,
        hint,
    }
}

fn parse_parameters(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<SymbolWithHint> {
    require_token(tokens, diagnostics, "(");

    let mut params = vec![];
    loop {
        if peeked_symbol_is(tokens, ")") {
            break;
        }

        let param = parse_parameter(tokens, id_gen, diagnostics, false);
        params.push(param);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ")" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `)` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `)` here, but got EOF".to_string(),
                ),
            });
            break;
        }
    }

    require_token(tokens, diagnostics, ")");

    // Emit error if there are duplicate parameters.
    // TODO: allow parsing to return an AST even if errors are present.
    let mut seen = HashSet::new();
    for param in &params {
        let param_name = &param.symbol.name.0;
        if param.symbol.name.is_underscore() {
            continue;
        }

        if seen.contains(param_name) {
            diagnostics.push(ParseError::Invalid {
                position: param.symbol.position.clone(),
                message: ErrorMessage(format!("Duplicate parameter: `{}`", param_name)),
                // TODO: report the position of the previous parameter too.
                additional: vec![],
            });
        } else {
            seen.insert(param_name.clone());
        }
    }

    params
}

fn parse_struct_fields(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<FieldInfo> {
    let mut fields = vec![];
    loop {
        if peeked_symbol_is(tokens, "}") {
            break;
        }

        if let Some(token) = tokens.peek() {
            let doc_comment = parse_doc_comment(&token);
            let sym = parse_symbol(tokens, id_gen, diagnostics);
            let hint = parse_colon_and(tokens, id_gen, diagnostics);

            fields.push(FieldInfo {
                sym,
                hint,
                doc_comment,
            });
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected a struct field name here like `foo: String`, but got EOF".to_string(),
                ),
            });
            break;
        }

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == "}" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(format!(
                        "Invalid syntax: Expected `,` or `}}` here, but got `{}`",
                        token.text
                    )),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage(
                    "Invalid syntax: Expected `,` or `}}` here, but got EOF".to_string(),
                ),
            });
            break;
        }
    }

    // TODO: error on duplicate fields

    fields
}

fn parse_block(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
    is_loop_body: bool,
) -> Block {
    let open_brace = require_token(tokens, diagnostics, "{");

    let mut exprs = vec![];
    loop {
        if let Some(token) = tokens.peek() {
            if token.text == "}" {
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("Invalid syntax: Expected `}` here, but got EOF".to_string()),
            });
            break;
        }

        let start_idx = tokens.idx;
        let expr = parse_expression(src, tokens, id_gen, diagnostics);
        if expr.expr_.is_invalid_or_placeholder() {
            break;
        }
        exprs.push(expr);
        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );
    }

    let close_brace = require_token(tokens, diagnostics, "}");
    Block {
        open_brace: open_brace.position,
        exprs,
        close_brace: close_brace.position,
        is_loop_body,
    }
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
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<Definition> {
    let fun_token = require_token(tokens, diagnostics, "fun");
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    // We can distinguish between functions and methods based on the
    // token after the type parameters.
    //
    // ```
    // fun<T> i_am_a_fun() {}
    // fun<T> (self: String) i_am_a_method() {}
    // ```
    match tokens.peek() {
        Some(token) => Some(if token.text == "(" {
            parse_method(src, tokens, id_gen, diagnostics, fun_token, type_params)
        } else {
            parse_function(src, tokens, id_gen, diagnostics, fun_token, type_params)
        }),
        None => {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(),
                message: ErrorMessage("Unfinished function or method definition.".to_owned()),
            });
            None
        }
    }
}

fn parse_method(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
    fun_token: Token,
    type_params: Vec<TypeSymbol>,
) -> Definition {
    let doc_comment = parse_doc_comment(&fun_token);

    require_token(tokens, diagnostics, "(");
    let receiver_param = parse_parameter(tokens, id_gen, diagnostics, true);
    let receiver_sym = receiver_param.symbol.clone();
    let receiver_hint = match receiver_param.hint {
        Some(type_name) => type_name,
        None => {
            diagnostics.push(ParseError::Incomplete {
                position: receiver_param.symbol.position.clone(),
                message: ErrorMessage("This `self` argument requires a type.".to_owned()),
            });
            TypeHint {
                sym: TypeSymbol {
                    name: TypeName {
                        name: "__MISSING_TYPE".to_owned(),
                    },
                    position: receiver_sym.position.clone(),
                    id: id_gen.next(),
                },
                args: vec![],
                position: receiver_sym.position.clone(),
            }
        }
    };
    require_token(tokens, diagnostics, ")");

    let name = parse_symbol(tokens, id_gen, diagnostics);

    let params = parse_parameters(tokens, id_gen, diagnostics);
    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(src, tokens, id_gen, diagnostics, false);

    let mut start_offset = fun_token.position.start_offset;
    if let Some((comment_pos, _)) = fun_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;
    let close_brace_pos = body.close_brace.clone();

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let fun_info = FunInfo {
        src_string: src_string.clone(),
        doc_comment,
        name: Some(name.clone()),
        type_params,
        params,
        body,
        return_hint,
    };
    let meth_info = MethodInfo {
        receiver_hint,
        receiver_sym,
        name_sym: name,
        kind: MethodKind::UserDefinedMethod(fun_info),
    };

    let position = Position::merge(&fun_token.position, &close_brace_pos);

    Definition(src_string.clone(), position, Definition_::Method(meth_info))
}

fn parse_function(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
    fun_token: Token,
    type_params: Vec<TypeSymbol>,
) -> Definition {
    let doc_comment = parse_doc_comment(&fun_token);

    let name = parse_symbol(tokens, id_gen, diagnostics);

    let params = parse_parameters(tokens, id_gen, diagnostics);
    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(src, tokens, id_gen, diagnostics, false);

    let mut start_offset = fun_token.position.start_offset;
    if let Some((comment_pos, _)) = fun_token.preceding_comments.first() {
        start_offset = comment_pos.start_offset;
    }
    let end_offset = body.close_brace.end_offset;
    let close_brace_pos = body.close_brace.clone();

    let src_string = SourceString {
        offset: start_offset,
        src: src[start_offset..end_offset].to_owned(),
    };

    let position = Position::merge(&fun_token.position, &close_brace_pos);

    Definition(
        src_string.clone(),
        position,
        Definition_::Fun(
            name.clone(),
            FunInfo {
                src_string,
                doc_comment,
                name: Some(name),
                type_params,
                params,
                body,
                return_hint,
            },
        ),
    )
}

const RESERVED_WORDS: &[&str] = &[
    "let", "fun", "enum", "struct", "if", "else", "while", "return", "test", "match", "break",
    "continue",
];

pub fn placeholder_symbol(position: Position, id_gen: &mut SyntaxIdGenerator) -> Symbol {
    Symbol {
        position,
        name: SymbolName("__placeholder".to_string()),
        id: id_gen.next(),
    }
}

fn parse_symbol(
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Symbol {
    let variable_token = require_a_token(tokens, diagnostics, "variable name");
    if !SYMBOL_RE.is_match(variable_token.text) {
        diagnostics.push(ParseError::Invalid {
            position: variable_token.position.clone(),
            message: ErrorMessage(format!("Invalid name: '{}'", variable_token.text)),
            additional: vec![],
        });
        tokens.unpop();
        return placeholder_symbol(variable_token.position, id_gen);
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            diagnostics.push(ParseError::Invalid {
                position: variable_token.position.clone(),
                message: ErrorMessage(format!(
                    "'{}' is a reserved word that cannot be used as a name",
                    variable_token.text
                )),
                additional: vec![],
            });
            tokens.unpop();
            return placeholder_symbol(variable_token.position, id_gen);
        }
    }

    Symbol {
        position: variable_token.position,
        name: SymbolName(variable_token.text.to_string()),
        id: id_gen.next(),
    }
}

fn parse_let_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let let_token = require_token(tokens, diagnostics, "let");
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    let hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    require_token(tokens, diagnostics, "=");
    let expr = parse_expression(src, tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&let_token.position, &expr.pos),
        Expression_::Let(variable, hint, Box::new(expr)),
        id_gen.next(),
    )
}

fn parse_assign_expression(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    require_token(tokens, diagnostics, "=");
    let expr = parse_expression(src, tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&variable.position, &expr.pos),
        Expression_::Assign(variable, Box::new(expr)),
        id_gen.next(),
    )
}

fn parse_toplevel_expr(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let expr = parse_expression(src, tokens, id_gen, diagnostics);
    ToplevelItem::Expr(ToplevelExpression(expr))
}

fn parse_toplevel_items_from_tokens(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<ToplevelItem> {
    let mut items: Vec<ToplevelItem> = vec![];

    while !tokens.is_empty() {
        let start_idx = tokens.idx;
        match parse_toplevel_item_from_tokens(src, tokens, id_gen, diagnostics) {
            Some(item) => {
                let was_invalid = matches!(
                    item,
                    ToplevelItem::Expr(ToplevelExpression(Expression {
                        expr_: Expression_::Invalid,
                        ..
                    }))
                );

                items.push(item);
                if was_invalid {
                    break;
                }

                assert!(
                    tokens.idx > start_idx,
                    "The parser should always make forward progress, looking"
                );
            }
            None => break,
        }
    }
    items
}

fn parse_toplevel_item_from_tokens(
    src: &str,
    tokens: &mut TokenStream,
    id_gen: &mut SyntaxIdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<ToplevelItem> {
    if let Some(token) = tokens.peek() {
        if token.text == "fun"
            || token.text == "test"
            || token.text == "enum"
            || token.text == "struct"
        {
            return parse_definition(src, tokens, id_gen, diagnostics).map(ToplevelItem::Def);
        }
    }

    Some(parse_toplevel_expr(src, tokens, id_gen, diagnostics))
}

pub fn parse_inline_expr_from_str(
    path: &Path,
    src: &str,
    id_gen: &mut SyntaxIdGenerator,
) -> (Expression, Vec<ParseError>) {
    let mut diagnostics = vec![];

    let mut tokens = match lex(path, src) {
        Ok(tokens) => tokens,
        Err(e) => {
            diagnostics.push(e);
            TokenStream::empty()
        }
    };

    let expr = parse_expression(src, &mut tokens, id_gen, &mut diagnostics);
    (expr, diagnostics)
}

pub fn parse_toplevel_items(
    path: &Path,
    src: &str,
    id_gen: &mut SyntaxIdGenerator,
) -> (Vec<ToplevelItem>, Vec<ParseError>) {
    let mut diagnostics = vec![];
    let mut tokens = match lex(path, src) {
        Ok(tokens) => tokens,
        Err(e) => {
            diagnostics.push(e);
            TokenStream::empty()
        }
    };

    let items = parse_toplevel_items_from_tokens(src, &mut tokens, id_gen, &mut diagnostics);
    (items, diagnostics)
}

/// Parse all the toplevel items in `src` between `offset` and
/// `end_offset`.
pub fn parse_toplevel_items_from_span(
    path: &Path,
    src: &str,
    id_gen: &mut SyntaxIdGenerator,
    offset: usize,
    end_offset: usize,
) -> (Vec<ToplevelItem>, Vec<ParseError>) {
    let mut diagnostics = vec![];
    let mut tokens = match lex_between(path, src, offset, end_offset) {
        Ok(tokens) => tokens,
        Err(e) => {
            diagnostics.push(e);
            TokenStream::empty()
        }
    };

    let items = parse_toplevel_items_from_tokens(src, &mut tokens, id_gen, &mut diagnostics);
    (items, diagnostics)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_incomplete_expression() {
        let (_, errors) = parse_toplevel_items(
            &PathBuf::from("__test.gdn"),
            "1 + ",
            &mut SyntaxIdGenerator::default(),
        );
        assert!(!errors.is_empty())
    }

    #[test]
    fn test_repeated_param() {
        let (_, errors) = parse_toplevel_items(
            &PathBuf::from("__test.gdn"),
            "fun f(x, x) {} ",
            &mut SyntaxIdGenerator::default(),
        );
        assert!(!errors.is_empty())
    }

    #[test]
    fn test_repeated_param_underscore() {
        let (_, errors) = parse_toplevel_items(
            &PathBuf::from("__test.gdn"),
            "fun f(_, _) {} ",
            &mut SyntaxIdGenerator::default(),
        );
        assert!(errors.is_empty())
    }
}
