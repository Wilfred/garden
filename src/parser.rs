pub(crate) mod ast;
pub(crate) mod diagnostics;
pub(crate) mod lex;
pub(crate) mod position;
pub(crate) mod vfs;
pub(crate) mod visitor;

use std::collections::HashSet;
use std::rc::Rc;

use ast::{FieldInfo, StructInfo};
use position::Position;

use ast::*;
use diagnostics::ErrorMessage;
use diagnostics::MessagePart::*;
use lex::{lex, lex_between, Token, TokenStream, INTEGER_RE, SYMBOL_RE};
use vfs::VfsPathBuf;

use crate::{msgcode, msgtext};

// TODO: implement precedence using Pratt parsing, as discussed in
// <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

#[derive(Debug)]
#[allow(dead_code)] // additional isn't used yet.
pub(crate) enum ParseError {
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

impl ParseError {
    pub(crate) fn position(&self) -> &Position {
        match self {
            ParseError::Invalid { position, .. } => position,
            ParseError::Incomplete { position, .. } => position,
        }
    }

    pub(crate) fn message(&self) -> &ErrorMessage {
        match self {
            ParseError::Invalid { message, .. } => message,
            ParseError::Incomplete { message, .. } => message,
        }
    }
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
            let prev_token = tokens.prev();
            let position = prev_token
                .as_ref()
                .map(|t| t.position.clone())
                .unwrap_or(Position::todo(&tokens.vfs_path));

            diagnostics.push(ParseError::Incomplete {
                message: ErrorMessage(vec![msgtext!(
                    "Expected {}, but got EOF.",
                    token_description
                )]),
                position,
            });

            // TODO: this is arbitrarily choosing the previous token,
            // which is a horrible hack. It would be better to have a
            // way of generating placeholder tokens.
            prev_token.expect("TODO: handle empty token streams")
        }
    }
}

fn required_token_ok(
    tokens: &mut TokenStream<'_>,
    diagnostics: &mut Vec<ParseError>,
    expected: &str,
) -> bool {
    let (ok, _) = check_required_token(tokens, diagnostics, expected);
    ok
}

fn check_required_token<'a>(
    tokens: &mut TokenStream<'a>,
    diagnostics: &mut Vec<ParseError>,
    expected: &str,
) -> (bool, Token<'a>) {
    let prev_token = tokens.prev();

    match tokens.pop() {
        Some(token) => {
            let mut ok = true;
            if token.text != expected {
                let position = prev_token.as_ref().unwrap_or(&token).position.clone();

                diagnostics.push(ParseError::Invalid {
                    position,
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!("{}", expected),
                        msgtext!(" after this."),
                    ]),
                    additional: vec![],
                });
                ok = false;

                // Undo the pop. We saw an unexpected token, so it
                // might be e.g. a close brace so we shouldn't just
                // discard it.
                tokens.unpop();
            }

            (ok, token)
        }
        None => {
            let position = prev_token
                .as_ref()
                .map(|t| t.position.clone())
                .unwrap_or(Position::todo(&tokens.vfs_path));

            diagnostics.push(ParseError::Incomplete {
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!("{}", expected),
                    msgtext!(", but got EOF."),
                ]),
                position,
            });

            (false, prev_token.expect("TODO: handle empty file properly"))
        }
    }
}

fn require_token<'a>(
    tokens: &mut TokenStream<'a>,
    diagnostics: &mut Vec<ParseError>,
    expected: &str,
) -> Token<'a> {
    let (_, token) = check_required_token(tokens, diagnostics, expected);
    token
}

fn parse_integer(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let token = require_a_token(tokens, diagnostics, "integer literal");

    if INTEGER_RE.is_match(token.text) {
        let i: i64 = token.text.parse().unwrap();
        Expression::new(token.position, Expression_::IntLiteral(i), id_gen.next())
    } else {
        diagnostics.push(ParseError::Invalid {
            position: token.position.clone(),
            message: ErrorMessage(vec![
                msgcode!("{}", token.text),
                msgtext!(" is not a valid integer literal. Integer literals look like "),
                msgcode!("123"),
                msgtext!("."),
            ]),
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

fn parse_variable(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    Expression::new(
        variable.position.clone(),
        Expression_::Variable(variable),
        id_gen.next(),
    )
}

/// Parse a tuple literal `(1, 2)` or a parenthesised expression `(1 +
/// 2)`.
fn parse_tuple_literal_or_parentheses(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let open_paren = require_token(tokens, diagnostics, "(");
    if peeked_symbol_is(tokens, ")") {
        // Empty tuple.
        let close_paren = require_token(tokens, diagnostics, ")");

        return Expression::new(
            Position::merge(&open_paren.position, &close_paren.position),
            Expression_::TupleLiteral(vec![]),
            id_gen.next(),
        );
    }

    let expr = parse_expression(tokens, id_gen, diagnostics);
    let expr_pos = expr.position.clone();

    if peeked_symbol_is(tokens, ",") {
        let mut exprs = vec![expr];

        loop {
            if peeked_symbol_is(tokens, ",") {
                tokens.pop();
            } else if !peeked_symbol_is(tokens, ")") {
                let position = if let Some(token) = tokens.peek() {
                    token.position.clone()
                } else {
                    expr_pos
                };

                diagnostics.push(ParseError::Invalid {
                    position,
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!(")"),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });

                break;
            }

            // After a comma, we expect either a closing paren or another expression.
            if peeked_symbol_is(tokens, ")") {
                break;
            }

            let start_idx = tokens.idx;
            exprs.push(parse_expression(tokens, id_gen, diagnostics));
            assert!(
                tokens.idx > start_idx,
                "The parser should always make forward progress."
            );
        }

        let close_paren = require_token(tokens, diagnostics, ")");

        return Expression::new(
            Position::merge(&open_paren.position, &close_paren.position),
            Expression_::TupleLiteral(exprs.into_iter().map(Rc::new).collect()),
            id_gen.next(),
        );
    }

    // No comma, must be a parenthesised expression.
    let close_paren = require_token(tokens, diagnostics, ")");

    let position = Position::merge(&open_paren.position, &close_paren.position);
    Expression::new(
        position,
        Expression_::Parentheses(
            open_paren.position.clone(),
            Rc::new(expr),
            close_paren.position.clone(),
        ),
        id_gen.next(),
    )
}

fn parse_list_literal(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let open_bracket = require_token(tokens, diagnostics, "[");
    let items = parse_comma_separated_exprs(&open_bracket, tokens, id_gen, diagnostics, "]");
    let close_bracket = require_token(tokens, diagnostics, "]");

    Expression::new(
        Position::merge(&open_bracket.position, &close_bracket.position),
        Expression_::ListLiteral(items),
        id_gen.next(),
    )
}

fn parse_lambda(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let fun_keyword = require_token(tokens, diagnostics, "fun");
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let params = parse_parameters(tokens, id_gen, diagnostics);
    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(tokens, id_gen, diagnostics, false);

    let pos = Position::merge(&fun_keyword.position, &body.close_brace);

    Expression::new(
        pos.clone(),
        Expression_::FunLiteral(FunInfo {
            pos,
            params,
            body,
            doc_comment: None,
            name_sym: None,
            item_id: None,
            type_params,
            return_hint,
        }),
        id_gen.next(),
    )
}

fn parse_assert(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let assert_keyword = require_token(tokens, diagnostics, "assert");

    let open_paren = require_token(tokens, diagnostics, "(");

    if peeked_symbol_is(tokens, ")") {
        let close_paren = tokens.pop().unwrap();

        let position = Position::merge(&open_paren.position, &close_paren.position);
        diagnostics.push(ParseError::Invalid {
            position: position.clone(),
            message: ErrorMessage(vec![
                msgcode!("assert"),
                msgtext!(" requires an expression, for example "),
                msgcode!("assert(x == 42)"),
                msgtext!("."),
            ]),
            additional: vec![],
        });

        return Expression::new(position, Expression_::Invalid, id_gen.next());
    }

    let expr = parse_expression(tokens, id_gen, diagnostics);
    let close_paren = require_token(tokens, diagnostics, ")");

    Expression::new(
        Position::merge(&assert_keyword.position, &close_paren.position),
        Expression_::Assert(Rc::new(expr)),
        id_gen.next(),
    )
}

fn parse_if(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let if_token = require_token(tokens, diagnostics, "if");

    let cond_expr = parse_expression(tokens, id_gen, diagnostics);
    let mut then_body = parse_block(tokens, id_gen, diagnostics, false);

    let else_body: Option<Block> = if peeked_symbol_is(tokens, "else") {
        tokens.pop();

        if peeked_symbol_is(tokens, "if") {
            let if_expr = parse_if(tokens, id_gen, diagnostics);
            Some(Block {
                // TODO: when there is a chain of if/else if
                // expressions, the open brace isn't meaningful. This
                // is an ugly hack.
                open_brace: if_expr.position.clone(),
                close_brace: if_expr.position.clone(),
                exprs: vec![if_expr.into()],
            })
        } else {
            Some(parse_block(tokens, id_gen, diagnostics, false))
        }
    } else {
        None
    };

    if else_body.is_none() {
        // We have no else block, so we're not using any values from
        // the then block.
        let then_body_exprs: Vec<_> = then_body
            .exprs
            .iter()
            .map(|e| {
                let mut e = e.as_ref().clone();
                e.value_is_used = false;
                Rc::new(e)
            })
            .collect();
        then_body.exprs = then_body_exprs;
    }

    let last_brace_pos = match &else_body {
        Some(else_body) => &else_body.close_brace,
        None => &then_body.close_brace,
    };

    Expression::new(
        Position::merge(&if_token.position, last_brace_pos),
        Expression_::If(Rc::new(cond_expr), then_body, else_body),
        id_gen.next(),
    )
}

fn parse_while(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let while_token = require_token(tokens, diagnostics, "while");

    let cond_expr = parse_expression(tokens, id_gen, diagnostics);
    let body = parse_block(tokens, id_gen, diagnostics, true);

    Expression::new(
        Position::merge(&while_token.position, &body.close_brace),
        Expression_::While(Rc::new(cond_expr), body),
        id_gen.next(),
    )
}

fn parse_for_in(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let for_token = require_token(tokens, diagnostics, "for");
    let destination = parse_let_destination(tokens, id_gen, diagnostics);

    require_token(tokens, diagnostics, "in");

    let expr = parse_expression(tokens, id_gen, diagnostics);

    let body = parse_block(tokens, id_gen, diagnostics, true);

    Expression::new(
        Position::merge(&for_token.position, &body.close_brace),
        Expression_::ForIn(destination, Rc::new(expr), body),
        id_gen.next(),
    )
}
fn parse_break(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let break_token = require_token(tokens, diagnostics, "break");
    Expression::new(break_token.position, Expression_::Break, id_gen.next())
}

fn parse_continue(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let continue_token = require_token(tokens, diagnostics, "continue");
    Expression::new(
        continue_token.position,
        Expression_::Continue,
        id_gen.next(),
    )
}

fn parse_return(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let return_token = require_token(tokens, diagnostics, "return");

    let mut expr = None;
    let mut pos = return_token.position.clone();

    // The following is ambiguous:
    //
    // ```garden
    // return
    // foo()
    // ```
    //
    // Is this two expressions `return`, `foo()`, or `return foo()`?
    // We solve this ambiguity by requiring the returned expression to
    // start on the same line as the `return` keyword.
    if let Some(next_token) = tokens.peek() {
        if return_token.position.end_line_number == next_token.position.line_number {
            let returned_expr = parse_expression(tokens, id_gen, diagnostics);
            pos = Position::merge(&pos, &returned_expr.position);
            expr = Some(Rc::new(returned_expr));
        }
    }

    Expression::new(pos, Expression_::Return(expr), id_gen.next())
}

fn unescape_string(token: &Token<'_>) -> (Vec<ParseError>, String) {
    let mut diagnostics = vec![];
    let src = token.text;

    // Trim doublequotes.
    let mut s = &src[1..];
    if s.ends_with('"') {
        s = &s[..s.len() - 1];
    }

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
                Some('t') => {
                    res.push('\t');
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
                    diagnostics.push(ParseError::Invalid {
                        position: token.position.clone(),
                        message: ErrorMessage(vec![
                            msgtext!("Invalid escape sequence "),
                            msgcode!("\\{}", c),
                            msgtext!(". Only "),
                            msgcode!("\\\\"),
                            msgtext!(", "),
                            msgcode!("\\\""),
                            msgtext!(", "),
                            msgcode!("\\n"),
                            msgtext!(" and "),
                            msgcode!("\\t"),
                            msgtext!(" are supported."),
                        ]),
                        additional: vec![],
                    });

                    // Treat \z as \\z.
                    res.push(c);

                    i += 1;
                }
            }
        } else {
            res.push(c);
            i += 1;
        }
    }

    (diagnostics, res)
}

fn parse_simple_expression(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let prev_token = tokens.prev();

    if let Some(token) = tokens.peek() {
        if token.text == "(" {
            return parse_tuple_literal_or_parentheses(tokens, id_gen, diagnostics);
        }

        if token.text == "[" {
            return parse_list_literal(tokens, id_gen, diagnostics);
        }

        if token.text == "fun" {
            return parse_lambda(tokens, id_gen, diagnostics);
        }

        if token.text == "assert" {
            return parse_assert(tokens, id_gen, diagnostics);
        }

        if SYMBOL_RE.is_match(token.text) {
            if let Some((prev_token, token)) = tokens.peek_two() {
                // Require struct literals to have the opening brace
                // immediately after the name, to avoid ambiguity.
                //
                // TODO: arguably this should be done in the lexer.
                if token.text == "{"
                    && prev_token.position.end_offset == token.position.start_offset
                {
                    return parse_struct_literal(tokens, id_gen, diagnostics);
                }
            }

            return parse_variable(tokens, id_gen, diagnostics);
        }

        if token.text.starts_with('\"') {
            tokens.pop();

            let (errors, unescaped) = unescape_string(&token);
            diagnostics.extend(errors);

            return Expression::new(
                token.position,
                Expression_::StringLiteral(unescaped),
                id_gen.next(),
            );
        }

        if INTEGER_RE.is_match(token.text) {
            return parse_integer(tokens, id_gen, diagnostics);
        }

        let error_position = match prev_token {
            Some(prev_token) => prev_token.position.clone(),
            None => token.position.clone(),
        };

        diagnostics.push(ParseError::Invalid {
            position: error_position,
            message: ErrorMessage(vec![msgtext!("Expected an expression after this.")]),
            additional: vec![],
        });

        return Expression::new(token.position, Expression_::Invalid, id_gen.next());
    }

    diagnostics.push(ParseError::Incomplete {
        message: ErrorMessage(vec![Text("Expected an expression.".to_owned())]),
        position: Position::todo(&tokens.vfs_path),
    });

    Expression::new(
        Position::todo(&tokens.vfs_path),
        Expression_::Invalid,
        id_gen.next(),
    )
}

fn parse_struct_literal_fields(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<(Symbol, Rc<Expression>)> {
    let mut fields: Vec<(Symbol, Rc<Expression>)> = vec![];
    loop {
        if peeked_symbol_is(tokens, "}") {
            break;
        }

        let start_idx = tokens.idx;
        let sym = parse_symbol(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, ":");
        let expr = parse_expression(tokens, id_gen, diagnostics);

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

        fields.push((sym, Rc::new(expr)));

        let Some(token) = tokens.peek() else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(&tokens.vfs_path),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!("}}"),
                    msgtext!(" here, but got EOF."),
                ]),
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
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let name = parse_type_symbol(tokens, id_gen, diagnostics);
    require_token(tokens, diagnostics, "{");
    let fields = parse_struct_literal_fields(tokens, id_gen, diagnostics);

    let close_brace = require_token(tokens, diagnostics, "}");

    Expression::new(
        Position::merge(&name.position, &close_brace.position),
        Expression_::StructLiteral(name, fields),
        id_gen.next(),
    )
}

fn parse_match(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let match_keyword = require_token(tokens, diagnostics, "match");
    let scrutinee_expr = parse_expression(tokens, id_gen, diagnostics);

    let open_brace = require_token(tokens, diagnostics, "{");
    if open_brace.text != "{" {
        return Expression::new(
            scrutinee_expr.position.clone(),
            Expression_::Match(Rc::new(scrutinee_expr), vec![]),
            id_gen.next(),
        );
    }

    let mut cases = vec![];
    loop {
        let Some(token) = tokens.peek() else {
            let position = match tokens.prev() {
                Some(prev_token) => prev_token.position.clone(),
                None => Position::todo(&tokens.vfs_path),
            };

            diagnostics.push(ParseError::Incomplete {
                position,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!("}}"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break;
        };

        if token.text == "}" {
            break;
        }

        let start_idx = tokens.idx;
        let pattern = parse_pattern(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, "=>");
        let case_block = parse_case_block(tokens, id_gen, diagnostics);

        if tokens.idx <= start_idx {
            break;
        }

        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );

        cases.push((pattern, case_block));
    }

    let close_paren = require_token(tokens, diagnostics, "}");

    Expression::new(
        Position::merge(&match_keyword.position, &close_paren.position),
        Expression_::Match(Rc::new(scrutinee_expr), cases),
        id_gen.next(),
    )
}

fn parse_case_block(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Block {
    let block = if peeked_symbol_is(tokens, "{") {
        parse_block(tokens, id_gen, diagnostics, false)
    } else {
        // To simplify evaluation, we treat case expressions as
        // blocks, because they can have new bindings.
        let case_expr = parse_expression(tokens, id_gen, diagnostics);

        let pos = case_expr.position.clone();
        Block {
            open_brace: pos.clone(),
            exprs: vec![case_expr.into()],
            close_brace: pos,
        }
    };

    // Allow trailing comma after both expression and block syntax.
    if peeked_symbol_is(tokens, ",") {
        tokens.pop().unwrap();
    }
    block
}

fn parse_pattern(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Pattern {
    let variant_sym = parse_symbol(tokens, id_gen, diagnostics);

    let payload = if peeked_symbol_is(tokens, "(") {
        require_token(tokens, diagnostics, "(");
        let dest = parse_let_destination(tokens, id_gen, diagnostics);
        require_token(tokens, diagnostics, ")");
        Some(dest)
    } else {
        None
    };

    Pattern {
        variant_sym,
        payload,
    }
}

fn parse_comma_separated_exprs(
    open_token: &Token<'_>,
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
    terminator: &str,
) -> Vec<ExpressionWithComma> {
    let mut items: Vec<ExpressionWithComma> = vec![];
    loop {
        if peeked_symbol_is(tokens, terminator) {
            break;
        }

        let start_idx = tokens.idx;
        let arg = parse_expression(tokens, id_gen, diagnostics);
        let arg_pos = arg.position.clone();

        if arg.expr_.is_invalid_or_placeholder() {
            break;
        }

        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                items.push(ExpressionWithComma {
                    expr: Rc::new(arg),
                    comma: Some(token.position),
                });

                tokens.pop();
            } else if token.text != terminator {
                items.push(ExpressionWithComma {
                    expr: Rc::new(arg),
                    comma: None,
                });

                diagnostics.push(ParseError::Invalid {
                    position: arg_pos.clone(),
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!("{}", terminator),
                        msgtext!(" after this."),
                    ]),
                    additional: vec![],
                });

                // Attempt to recover a reasonable AST.
                if arg_pos.line_number == token.position.line_number {
                    // Next token is on the same line, treat it as
                    // `foo(a b)` with a forgotten comma.
                    continue;
                } else if open_token.position.line_number != arg_pos.line_number {
                    // Next token is on another line, but the open
                    // token was on an earlier line. Treat it as a
                    // forgotten comma.
                    //
                    // ```
                    // let x = [
                    //   1 // <-- missing here
                    //   2
                    // ]
                    // ```
                    continue;
                } else {
                    // Next token is on another line, treat it as a
                    // forgotten parenthesis `foo(a`.
                    break;
                }
            } else {
                items.push(ExpressionWithComma {
                    expr: Rc::new(arg),
                    comma: None,
                });
            }
        } else {
            let position = arg.position.clone();
            items.push(ExpressionWithComma {
                expr: Rc::new(arg),
                comma: None,
            });

            diagnostics.push(ParseError::Incomplete {
                position,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!("{}", terminator),
                    msgtext!(", but got EOF."),
                ]),
            });
            break;
        }
    }

    items
}

fn parse_call_arguments(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ParenthesizedArguments {
    let open_paren_token = require_token(tokens, diagnostics, "(");
    let arguments =
        parse_comma_separated_exprs(&open_paren_token, tokens, id_gen, diagnostics, ")");
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
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let mut expr = parse_simple_expression(tokens, id_gen, diagnostics);

    loop {
        let start_idx = tokens.idx;
        match tokens.peek() {
            Some(token)
                if token.text == "(" && expr.position.end_offset == token.position.start_offset =>
            {
                // Require parentheses to touch when we're parsing a
                // function call. This allows us to disambiguabe
                // `foo()` (a call) from `foo ()` (the variable `foo`
                // followed by a tuple).
                let arguments = parse_call_arguments(tokens, id_gen, diagnostics);

                expr = Expression::new(
                    Position::merge(&expr.position, &arguments.close_paren),
                    Expression_::Call(Rc::new(expr), arguments),
                    id_gen.next(),
                );
            }
            Some(token) if token.text == "." => {
                tokens.pop();

                let next_token = tokens.peek();

                // Require the symbol name to touch the dot when we're
                // parsing a dot access. This is an ambiguity problem
                // when the user hasn't finished writing the dot
                // access, but there is later code.
                if Some(token.position.end_offset)
                    == next_token.map(|tok| tok.position.start_offset)
                {
                    let variable = parse_symbol(tokens, id_gen, diagnostics);

                    if peeked_symbol_is(tokens, "(") {
                        // TODO: just treat a method call as a call of a dot access.
                        let arguments = parse_call_arguments(tokens, id_gen, diagnostics);

                        expr = Expression::new(
                            Position::merge(&expr.position, &arguments.close_paren),
                            Expression_::MethodCall(Rc::new(expr), variable, arguments),
                            id_gen.next(),
                        );
                    } else {
                        expr = Expression::new(
                            Position::merge(&expr.position, &variable.position),
                            Expression_::DotAccess(Rc::new(expr), variable),
                            id_gen.next(),
                        );
                    }
                } else {
                    let variable = placeholder_symbol(token.position, id_gen);

                    expr = Expression::new(
                        Position::merge(&expr.position, &variable.position),
                        Expression_::DotAccess(Rc::new(expr), variable),
                        id_gen.next(),
                    );
                }
            }
            Some(token) if token.text == "::" => {
                tokens.pop();

                let next_token = tokens.peek();

                // Require the symbol name to touch the :: when we're
                // parsing a namespace access. This is an ambiguity problem
                // when the user hasn't finished writing the dot
                // access, but there is later code.
                if Some(token.position.end_offset)
                    == next_token.map(|tok| tok.position.start_offset)
                {
                    let variable = parse_symbol(tokens, id_gen, diagnostics);

                    expr = Expression::new(
                        Position::merge(&expr.position, &variable.position),
                        Expression_::NamespaceAccess(Rc::new(expr), variable),
                        id_gen.next(),
                    );
                } else {
                    let variable = placeholder_symbol(token.position, id_gen);

                    expr = Expression::new(
                        Position::merge(&expr.position, &variable.position),
                        Expression_::NamespaceAccess(Rc::new(expr), variable),
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
        "%" => Some(BinaryOperatorKind::Modulo),
        "**" => Some(BinaryOperatorKind::Exponent),
        "==" => Some(BinaryOperatorKind::Equal),
        "!=" => Some(BinaryOperatorKind::NotEqual),
        "&&" => Some(BinaryOperatorKind::And),
        "||" => Some(BinaryOperatorKind::Or),
        "<" => Some(BinaryOperatorKind::LessThan),
        "<=" => Some(BinaryOperatorKind::LessThanOrEqual),
        ">" => Some(BinaryOperatorKind::GreaterThan),
        ">=" => Some(BinaryOperatorKind::GreaterThanOrEqual),
        "^" => Some(BinaryOperatorKind::StringConcat),
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
/// if a { b } else { c }
/// while z { foo() }
/// ```
fn parse_expression(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    // TODO: Matching on tokens will prevent us from doing more
    // complex assignments like `foo.bar = 1`.
    if let Some((_, token)) = tokens.peek_two() {
        if token.text == "=" {
            return parse_assign(tokens, id_gen, diagnostics);
        }
        if token.text == "+=" || token.text == "-=" {
            return parse_assign_update(tokens, id_gen, diagnostics);
        }
    }

    if let Some(token) = tokens.peek() {
        if token.text == "let" {
            return parse_let(tokens, id_gen, diagnostics);
        }
        if token.text == "return" {
            return parse_return(tokens, id_gen, diagnostics);
        }
        if token.text == "while" {
            return parse_while(tokens, id_gen, diagnostics);
        }
        if token.text == "for" {
            return parse_for_in(tokens, id_gen, diagnostics);
        }
        if token.text == "break" {
            return parse_break(tokens, id_gen, diagnostics);
        }
        if token.text == "continue" {
            return parse_continue(tokens, id_gen, diagnostics);
        }
        if token.text == "if" {
            return parse_if(tokens, id_gen, diagnostics);
        }
        if token.text == "match" {
            return parse_match(tokens, id_gen, diagnostics);
        }
    }

    parse_simple_expression_or_binop(tokens, id_gen, diagnostics)
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
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let mut expr = parse_simple_expression_with_trailing(tokens, id_gen, diagnostics);

    if let Some(token) = tokens.peek() {
        if let Some(op) = token_as_binary_op(token) {
            tokens.pop();

            let rhs_expr = parse_simple_expression_with_trailing(tokens, id_gen, diagnostics);

            expr = Expression::new(
                Position::merge(&expr.position, &rhs_expr.position),
                Expression_::BinaryOperator(Rc::new(expr), op, Rc::new(rhs_expr)),
                id_gen.next(),
            );
        }
    }

    expr
}

fn parse_definition(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<ToplevelItem> {
    if let Some((token, next_token)) = tokens.peek_two() {
        // Seeing `fun` then `(` indicates an anonymous function,
        // otherwise it's a function definition.
        if token.text == "fun" && next_token.text != "(" {
            return parse_function(tokens, id_gen, diagnostics);
        }
        // Arguably `external` then `fun` is always a function
        // definition, but check for `(` for consistency with the
        // `fun` case.
        let nextnext_is_paren = match tokens.peek_at(2) {
            Some(nextnext_token) => nextnext_token.text == "(",
            None => false,
        };
        if token.text == "external" && next_token.text == "fun" && !nextnext_is_paren {
            return parse_function(tokens, id_gen, diagnostics);
        }

        if token.text == "method" || token.text == "external" && next_token.text == "method" {
            return Some(parse_method(tokens, id_gen, diagnostics));
        }

        if token.text == "test" {
            return Some(parse_test(tokens, id_gen, diagnostics));
        }
        if token.text == "enum" || token.text == "external" && next_token.text == "enum" {
            return Some(parse_enum(tokens, id_gen, diagnostics));
        }
        if token.text == "struct" || token.text == "external" && next_token.text == "struct" {
            return Some(parse_struct(tokens, id_gen, diagnostics));
        }
        if token.text == "import" {
            return parse_import(tokens, id_gen, diagnostics);
        }

        // TODO: Include the token in the error message.
        diagnostics.push(ParseError::Invalid {
            position: token.position,
            message: ErrorMessage(vec![Text("Expected a definition".to_owned())]),
            additional: vec![],
        });
        return None;
    }

    let position = match tokens.prev() {
        Some(prev_token) => prev_token.position.clone(),
        None => Position::todo(&tokens.vfs_path),
    };

    diagnostics.push(ParseError::Incomplete {
        position,
        message: ErrorMessage(vec![Text("Unfinished definition".to_owned())]),
    });
    None
}

fn parse_enum_body(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
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
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!("}}"),
                        msgtext!(" here, but got "),
                        msgcode!("{}", token.text),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });
                break;
            }
        } else {
            let position = match tokens.prev() {
                Some(prev_token) => prev_token.position.clone(),
                None => Position::todo(&tokens.vfs_path),
            };

            diagnostics.push(ParseError::Incomplete {
                position,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!("}}"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break;
        }
    }

    variants
}

/// Parse enum variant, e.g. `Some(T)`.
fn parse_variant(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> VariantInfo {
    let name_symbol = parse_symbol(tokens, id_gen, diagnostics);

    let mut payload_hint = None;
    if peeked_symbol_is(tokens, "(") {
        tokens.pop();
        payload_hint = Some(parse_type_hint(tokens, id_gen, diagnostics));
        require_token(tokens, diagnostics, ")");
    }

    VariantInfo {
        name_sym: name_symbol,
        payload_hint,
    }
}

fn parse_enum(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let mut visibility = Visibility::CurrentFile;
    let mut first_token = None;

    if let Some(token) = tokens.peek() {
        if token.text == "external" {
            let token = tokens.pop().unwrap();
            visibility = Visibility::External(token.position.clone());
            first_token = Some(token);
        }
    }

    let enum_token = require_token(tokens, diagnostics, "enum");
    let first_token = first_token.unwrap_or_else(|| enum_token.clone());

    let doc_comment = parse_doc_comment(&first_token);
    let name_symbol = parse_type_symbol(tokens, id_gen, diagnostics);
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let saw_open_brace = required_token_ok(tokens, diagnostics, "{");
    let (variants, close_brace_pos) = if !saw_open_brace {
        (vec![], name_symbol.position.clone())
    } else {
        let variants = parse_enum_body(tokens, id_gen, diagnostics);
        let close_brace = require_token(tokens, diagnostics, "}");
        (variants, close_brace.position)
    };

    let position = Position::merge_token(&first_token, &close_brace_pos);

    ToplevelItem::Enum(EnumInfo {
        pos: position,
        visibility,
        doc_comment,
        name_sym: name_symbol,
        type_params,
        variants,
    })
}

fn parse_struct(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let mut visibility = Visibility::CurrentFile;
    let mut first_token = None;

    if let Some(token) = tokens.peek() {
        if token.text == "external" {
            let token = tokens.pop().unwrap();
            visibility = Visibility::External(token.position.clone());
            first_token = Some(token);
        }
    }

    let struct_token = require_token(tokens, diagnostics, "struct");
    let first_token = first_token.unwrap_or_else(|| struct_token.clone());

    let doc_comment = parse_doc_comment(&first_token);
    let name_sym = parse_type_symbol(tokens, id_gen, diagnostics);
    let type_params = parse_type_params(tokens, id_gen, diagnostics);

    let saw_open_brace = required_token_ok(tokens, diagnostics, "{");

    let (fields, close_brace_pos) = if !saw_open_brace {
        (vec![], name_sym.position.clone())
    } else {
        let fields = parse_struct_fields(tokens, id_gen, diagnostics);
        let close_brace = require_token(tokens, diagnostics, "}");
        (fields, close_brace.position)
    };

    let position = Position::merge_token(&struct_token, &close_brace_pos);

    ToplevelItem::Struct(StructInfo {
        pos: position,
        visibility,
        doc_comment,
        name_sym,
        type_params,
        fields,
    })
}

fn parse_test(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let test_token = require_token(tokens, diagnostics, "test");
    let doc_comment = parse_doc_comment(&test_token);

    let name = parse_symbol(tokens, id_gen, diagnostics);

    if let Some(token) = tokens.peek() {
        if token.text == "(" {
            // The user has accidentally added parameters. Parse them
            // and throw them away, including any additional parse
            // errors.
            let mut param_diagnostics = vec![];
            let params = parse_parameters(tokens, id_gen, &mut param_diagnostics);

            diagnostics.push(ParseError::Invalid {
                position: Position::merge(&params.open_paren, &params.close_paren),
                message: ErrorMessage(vec![
                    msgtext!("Tests should not have arguments. A valid test look like this: "),
                    msgcode!("test foo {{}}"),
                    msgtext!("."),
                ]),
                additional: vec![],
            });
        }
    }

    let body = parse_block(tokens, id_gen, diagnostics, false);
    let position = Position::merge_token(&test_token, &body.close_brace);

    ToplevelItem::Test(TestInfo {
        pos: position,
        doc_comment,
        name_sym: name,
        body,
    })
}

/// Parse an import statement, e.g. `import "./foo.gdn"` or
/// `import "./foo.gdn" as bar`.
fn parse_import(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<ToplevelItem> {
    let import_token = require_token(tokens, diagnostics, "import");

    let Some(path_token) = tokens.pop() else {
        diagnostics.push(ParseError::Incomplete {
            position: import_token.position.clone(),
            message: ErrorMessage(vec![
                msgtext!("Unfinished"),
                msgcode!("import"),
                msgtext!("."),
            ]),
        });

        return None;
    };

    let position = Position::merge_token(&import_token, &path_token.position);

    let path_s = if path_token.text.starts_with('\"') {
        let (errors, s) = unescape_string(&path_token);
        diagnostics.extend(errors);

        s
    } else {
        diagnostics.push(ParseError::Incomplete {
            position: path_token.position,
            message: ErrorMessage(vec![
                msgcode!("import"),
                msgtext!(" requires a path, for example "),
                msgcode!("import \"./foo.gdn\""),
                msgtext!("."),
            ]),
        });

        return None;
    };

    let mut namespace_sym = None;

    // Parse `as foo` syntax.
    if peeked_symbol_is(tokens, "as") {
        tokens.pop();

        namespace_sym = Some(parse_symbol(tokens, id_gen, diagnostics));
    }

    let import_info = ImportInfo {
        pos: position.clone(),
        path: path_s.into(),
        path_pos: path_token.position.clone(),
        id: id_gen.next(),
        namespace_sym,
    };

    Some(ToplevelItem::Import(import_info))
}

fn parse_type_symbol(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeSymbol {
    let name = parse_symbol(tokens, id_gen, diagnostics);
    TypeSymbol {
        name: TypeName {
            text: name.name.text,
        },
        position: name.position,
        id: id_gen.next(),
    }
}

/// Parse (possibly nested) type arguments, e.g. `<Int, T, Option<String>>`.
fn parse_type_arguments(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
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
        let arg_pos = arg.position.clone();
        args.push(arg);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ">" {
                break token.position;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position.clone(),
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!(">"),
                        msgtext!(" here, but got "),
                        msgcode!("{}", token.text),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });
                break token.position;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: arg_pos.clone(),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!(">"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break arg_pos;
        }
    };

    require_token(tokens, diagnostics, ">");

    (args, Some(close_pos))
}

/// Parse type parameters for this definition, e.g. `<T, E>`.
fn parse_type_params(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
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
        let arg_pos = arg.position.clone();
        params.push(arg);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ">" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!(">"),
                        msgtext!(" here, but got "),
                        msgcode!("{}", token.text),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: arg_pos,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!(">"),
                    msgtext!(" here, but got EOF."),
                ]),
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
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeHint {
    let open_paren = require_token(tokens, diagnostics, "(");

    let mut item_hints = vec![];
    loop {
        let start_idx = tokens.idx;
        if peeked_symbol_is(tokens, ")") {
            break;
        }

        let hint = parse_type_hint(tokens, id_gen, diagnostics);
        let hint_pos = hint.position.clone();
        item_hints.push(hint);

        let Some(token) = tokens.peek() else {
            diagnostics.push(ParseError::Incomplete {
                position: hint_pos,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!(")"),
                    msgtext!(" after this, but got EOF."),
                ]),
            });
            break;
        };

        if token.text == "," {
            tokens.pop();
        } else if token.text == ")" {
            break;
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: token.position.clone(),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!(")"),
                    msgtext!(" here."),
                ]),
            });
            tokens.pop();
        }

        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );
    }

    let close_paren = require_token(tokens, diagnostics, ")");

    TypeHint {
        sym: TypeSymbol {
            name: TypeName {
                text: "Tuple".to_owned(),
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
    id_gen: &mut IdGenerator,
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

    if sym.name.text == "Tuple" {
        let formatted_args = args
            .iter()
            .map(|h| h.as_src())
            .collect::<Vec<_>>()
            .join(", ");
        let equivalent_tuple_src = format!("({})", formatted_args);

        diagnostics.push(ParseError::Invalid {
            position: position.clone(),
            message: ErrorMessage(vec![
                msgcode!("Tuple"),
                msgtext!(" cannot be used a type hint. Use "),
                msgcode!("{}", equivalent_tuple_src),
                msgtext!(" instead."),
            ]),
            additional: vec![],
        });
    }

    TypeHint {
        sym,
        args,
        position,
    }
}

/// Parse a colon and a type hint, e.g. `: Int`.
fn parse_colon_and(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> TypeHint {
    require_token(tokens, diagnostics, ":");
    parse_type_hint(tokens, id_gen, diagnostics)
}

/// Parse a type annotation, if present.
fn parse_colon_and_hint_opt(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<TypeHint> {
    let token = tokens.peek()?;

    if token.text == ":" {
        let type_hint = parse_colon_and(tokens, id_gen, diagnostics);
        return Some(type_hint);
    }

    if SYMBOL_RE.is_match(token.text) && !RESERVED_WORDS.contains(&token.text) {
        diagnostics.push(ParseError::Invalid {
            position: token.position.clone(),
            message: ErrorMessage(vec![
                msgtext!("Expected a "),
                msgcode!(":"),
                msgtext!(" before this type hint."),
            ]),
            additional: vec![],
        });

        let type_hint = parse_type_hint(tokens, id_gen, diagnostics);
        return Some(type_hint);
    }

    None
}

fn parse_parameter(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
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
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ParenthesizedParameters {
    let (ok, open_paren) = check_required_token(tokens, diagnostics, "(");

    if !ok {
        return ParenthesizedParameters {
            open_paren: open_paren.position.clone(),
            params: vec![],
            close_paren: open_paren.position.clone(),
        };
    }

    let mut params = vec![];
    loop {
        let start_idx = tokens.idx;
        if peeked_symbol_is(tokens, ")") {
            break;
        }

        let param = parse_parameter(tokens, id_gen, diagnostics, false);
        let param_pos = param.symbol.position.clone();
        params.push(param);

        if let Some(token) = tokens.peek() {
            if token.text == "," {
                tokens.pop();
            } else if token.text == ")" {
                break;
            } else {
                diagnostics.push(ParseError::Invalid {
                    position: token.position,
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!(")"),
                        msgtext!(" here, but got "),
                        msgcode!("{}", token.text),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: param_pos,
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!(")"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break;
        }

        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );
    }

    let close_paren = require_token(tokens, diagnostics, ")");

    // Emit error if there are duplicate parameters.
    let mut seen = HashSet::new();
    for param in &params {
        if param.symbol.name.is_underscore() {
            continue;
        }

        let param_name = &param.symbol.name.text;
        if seen.contains(param_name) {
            diagnostics.push(ParseError::Invalid {
                position: param.symbol.position.clone(),
                message: ErrorMessage(vec![
                    msgtext!("Duplicate parameter "),
                    msgcode!("{}", param_name),
                    msgtext!("."),
                ]),
                // TODO: report the position of the previous parameter too.
                additional: vec![],
            });
        } else {
            seen.insert(param_name.clone());
        }
    }

    ParenthesizedParameters {
        open_paren: open_paren.position.clone(),
        params,
        close_paren: close_paren.position.clone(),
    }
}

fn parse_struct_fields(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
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
                position: Position::todo(&tokens.vfs_path),
                message: ErrorMessage(vec![
                    msgtext!("Expected a struct field name here, such as "),
                    msgcode!("age: Int"),
                    msgtext!(", but got EOF."),
                ]),
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
                    message: ErrorMessage(vec![
                        msgtext!("Expected "),
                        msgcode!(","),
                        msgtext!(" or "),
                        msgcode!("}}"),
                        msgtext!(" here, but got "),
                        msgcode!("{}", token.text),
                        msgtext!("."),
                    ]),
                    additional: vec![],
                });
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(&tokens.vfs_path),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!(","),
                    msgtext!(" or "),
                    msgcode!("}}"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break;
        }
    }

    // TODO: error on duplicate fields

    fields
}

fn parse_block(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
    is_loop_body: bool,
) -> Block {
    let open_brace = require_token(tokens, diagnostics, "{");
    if open_brace.text != "{" {
        // No block, we've already emitted an error, so just treat it
        // as an empty block.
        return Block {
            open_brace: open_brace.position.clone(),
            exprs: vec![],
            close_brace: open_brace.position,
        };
    }

    let mut exprs: Vec<Expression> = vec![];
    loop {
        if let Some(token) = tokens.peek() {
            if token.text == "}" {
                break;
            }
        } else {
            diagnostics.push(ParseError::Incomplete {
                position: Position::todo(&tokens.vfs_path),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!("}}"),
                    msgtext!(" here, but got EOF."),
                ]),
            });
            break;
        }

        let start_idx = tokens.idx;
        let expr = parse_expression(tokens, id_gen, diagnostics);
        if expr.expr_.is_invalid_or_placeholder() {
            break;
        }
        exprs.push(expr);
        assert!(
            tokens.idx > start_idx,
            "The parser should always make forward progress."
        );
    }

    // Mark all expressions as not having their value used, except the
    // last one. For loops, we don't use the last value either.
    let exprs_len = exprs.len();
    for (i, expr) in exprs.iter_mut().enumerate() {
        if i < exprs_len - 1 || is_loop_body {
            expr.value_is_used = false;
        }
    }

    let exprs: Vec<Rc<Expression>> = exprs.into_iter().map(Rc::new).collect();

    let close_brace = require_token(tokens, diagnostics, "}");
    Block {
        open_brace: open_brace.position,
        exprs,
        close_brace: close_brace.position,
    }
}

fn join_comments(comments: &[(Position, &str)]) -> String {
    let mut comment_texts = comments
        .iter()
        .map(|(_, comment)| {
            let comment_text = comment.strip_prefix("//").unwrap_or(comment);
            comment_text.strip_prefix(" ").unwrap_or(comment_text)
        })
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

fn parse_function(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<ToplevelItem> {
    let mut visibility = Visibility::CurrentFile;
    let mut first_token = None;

    if let Some(token) = tokens.peek() {
        if token.text == "external" {
            let token = tokens.pop().unwrap();
            visibility = Visibility::External(token.position.clone());
            first_token = Some(token);
        }
    }

    let fun_token = require_token(tokens, diagnostics, "fun");
    let first_token = first_token.unwrap_or_else(|| fun_token.clone());

    parse_function_(tokens, id_gen, diagnostics, first_token, visibility)
}

fn parse_method(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let mut visibility = Visibility::CurrentFile;
    let mut first_token = None;

    if let Some(token) = tokens.peek() {
        if token.text == "external" {
            let token = tokens.pop().unwrap();
            visibility = Visibility::External(token.position.clone());
            first_token = Some(token);
        }
    }

    let method_token = require_token(tokens, diagnostics, "method");
    let first_token = first_token.unwrap_or_else(|| method_token.clone());

    let doc_comment = parse_doc_comment(&first_token);

    let name_sym = parse_symbol(tokens, id_gen, diagnostics);

    let type_params = parse_type_params(tokens, id_gen, diagnostics);
    let mut params = parse_parameters(tokens, id_gen, diagnostics);

    let (receiver_sym, receiver_hint) = if params.params.is_empty() {
        // Use placeholders
        let position = name_sym.position.clone();
        let receiver_sym = placeholder_symbol(position.clone(), id_gen);

        let receiver_type_sym = TypeSymbol {
            name: TypeName {
                text: receiver_sym.name.text.clone(),
            },
            position: receiver_sym.position.clone(),
            id: id_gen.next(),
        };
        let receiver_type_hint = TypeHint {
            sym: receiver_type_sym,
            args: vec![],
            position: position.clone(),
        };
        (receiver_sym, receiver_type_hint)
    } else {
        let sym_with_hint = params.params.remove(0);

        let hint = match &sym_with_hint.hint {
            Some(h) => h.clone(),
            None => {
                // If the receiver doesn't have a type hint, we
                // use a placeholder type.
                let position = sym_with_hint.symbol.position.clone();

                let receiver_type_sym = placeholder_symbol(position.clone(), id_gen);

                let receiver_type_sym = TypeSymbol {
                    name: TypeName {
                        text: receiver_type_sym.name.text.clone(),
                    },
                    position: receiver_type_sym.position.clone(),
                    id: id_gen.next(),
                };
                TypeHint {
                    sym: receiver_type_sym,
                    args: vec![],
                    position: position.clone(),
                }
            }
        };
        (sym_with_hint.symbol.clone(), hint)
    };

    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(tokens, id_gen, diagnostics, false);
    let close_brace_pos = body.close_brace.clone();

    let position = Position::merge_token(&first_token, &close_brace_pos);

    let fun_info = FunInfo {
        pos: position.clone(),
        doc_comment,
        name_sym: Some(name_sym.clone()),
        item_id: Some(ToplevelItemId(id_gen.next().0)),
        type_params,
        params,
        body,
        return_hint,
    };
    let meth_info = MethodInfo {
        pos: position.clone(),
        receiver_hint,
        receiver_sym,
        name_sym,
        kind: MethodKind::UserDefinedMethod(fun_info),
    };

    ToplevelItem::Method(meth_info, visibility)
}

fn parse_function_(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
    first_token: Token,
    visibility: Visibility,
) -> Option<ToplevelItem> {
    let doc_comment = parse_doc_comment(&first_token);

    let name_sym = parse_symbol(tokens, id_gen, diagnostics);
    if is_reserved_word_placeholder(&name_sym) {
        // The next name is a keyword, it's probably the beginning of
        // a whole new definition. Give up on this definition having
        // only consumed our own keyword.
        return None;
    }

    let type_params = parse_type_params(tokens, id_gen, diagnostics);
    let params = parse_parameters(tokens, id_gen, diagnostics);
    let return_hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    let body = parse_block(tokens, id_gen, diagnostics, false);
    let close_brace_pos = body.close_brace.clone();
    let position = Position::merge_token(&first_token, &close_brace_pos);

    Some(ToplevelItem::Fun(
        name_sym.clone(),
        FunInfo {
            pos: position.clone(),
            doc_comment,
            name_sym: Some(name_sym),
            item_id: Some(ToplevelItemId(id_gen.next().0)),
            type_params,
            params,
            body,
            return_hint,
        },
        visibility,
    ))
}

pub(crate) const RESERVED_WORDS: &[&str] = &[
    "let", "fun", "enum", "struct", "internal", "external", "import", "if", "else", "while",
    "return", "test", "match", "break", "continue", "for", "in", "assert", "as", "method",
];

pub(crate) fn placeholder_symbol(position: Position, id_gen: &mut IdGenerator) -> Symbol {
    let name = SymbolName {
        text: "__placeholder".to_owned(),
    };
    Symbol {
        interned_id: id_gen.intern_symbol(&name),
        position,
        name,
        id: id_gen.next(),
    }
}

fn reserved_word_placeholder(position: Position, id_gen: &mut IdGenerator) -> Symbol {
    let name = SymbolName {
        text: "__reserved_word_placeholder".to_owned(),
    };
    Symbol {
        interned_id: id_gen.intern_symbol(&name),
        position,
        name,
        id: id_gen.next(),
    }
}

fn is_reserved_word_placeholder(symbol: &Symbol) -> bool {
    symbol.name.text == "__reserved_word_placeholder"
}

fn parse_let_destination(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> LetDestination {
    if peeked_symbol_is(tokens, "(") {
        tokens.pop();

        let mut symbols = vec![];
        loop {
            if peeked_symbol_is(tokens, ")") {
                tokens.pop();
                break;
            }

            let start_idx = tokens.idx;

            let symbol = parse_symbol(tokens, id_gen, diagnostics);
            if symbol.is_placeholder() {
                // If we saw a comma, the user has written e.g. `let
                // (, y)` and we can sensibly parse most of
                // it. Otherwise, give up.
                if !peeked_symbol_is(tokens, ",") {
                    break;
                }
            }

            symbols.push(symbol);

            if !peeked_symbol_is(tokens, ")") {
                require_token(tokens, diagnostics, ",");
            }

            assert!(
                tokens.idx > start_idx,
                "The parser should always make forward progress."
            );
        }

        let mut seen = HashSet::new();
        for symbol in &symbols {
            if symbol.name.is_underscore() {
                continue;
            }

            let name = &symbol.name.text;
            if seen.contains(name) {
                diagnostics.push(ParseError::Invalid {
                    position: symbol.position.clone(),
                    message: ErrorMessage(vec![
                        msgtext!("Duplicate variable  "),
                        msgcode!("{}", name),
                        msgtext!(" in destructuring "),
                        msgcode!("let"),
                        msgtext!("."),
                    ]),
                    // TODO: report the position of the previous occurrence too.
                    additional: vec![],
                });
            } else {
                seen.insert(name.clone());
            }
        }

        LetDestination::Destructure(symbols)
    } else {
        LetDestination::Symbol(parse_symbol(tokens, id_gen, diagnostics))
    }
}

fn parse_symbol(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Symbol {
    let prev_token = tokens.prev();
    let variable_token = require_a_token(tokens, diagnostics, "variable name");

    let prev_token_pos = match prev_token {
        Some(t) => t.position.clone(),
        None => variable_token.position.clone(),
    };

    if !SYMBOL_RE.is_match(variable_token.text) {
        diagnostics.push(ParseError::Invalid {
            position: prev_token_pos,
            message: ErrorMessage(vec![msgtext!("Expected a symbol after this.")]),
            additional: vec![],
        });
        tokens.unpop();
        return placeholder_symbol(variable_token.position, id_gen);
    }

    for reserved in RESERVED_WORDS {
        if variable_token.text == *reserved {
            diagnostics.push(ParseError::Invalid {
                position: prev_token_pos,
                message: ErrorMessage(vec![msgtext!("Expected a symbol after this.")]),
                additional: vec![],
            });
            tokens.unpop();
            return reserved_word_placeholder(variable_token.position, id_gen);
        }
    }

    let name = SymbolName {
        text: variable_token.text.to_owned(),
    };
    Symbol {
        interned_id: id_gen.intern_symbol(&name),
        position: variable_token.position,
        name,
        id: id_gen.next(),
    }
}

fn parse_let(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let let_token = require_token(tokens, diagnostics, "let");
    let destination = parse_let_destination(tokens, id_gen, diagnostics);

    let hint = parse_colon_and_hint_opt(tokens, id_gen, diagnostics);

    require_token(tokens, diagnostics, "=");
    let expr = parse_expression(tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&let_token.position, &expr.position),
        Expression_::Let(destination, hint, Rc::new(expr)),
        id_gen.next(),
    )
}

fn parse_assign(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    if !peeked_symbol_is(tokens, "=") {
        // Don't proceed if we don't have an equals sign at all. This
        // prevents infinite recursions between assign and expr when
        // no tokens are being consumed.
        let position = Position::todo(&tokens.vfs_path);
        return Expression::invalid(position, id_gen.next());
    }
    require_token(tokens, diagnostics, "=");
    let expr = parse_expression(tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&variable.position, &expr.position),
        Expression_::Assign(variable, Rc::new(expr)),
        id_gen.next(),
    )
}

fn parse_assign_update(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Expression {
    let variable = parse_symbol(tokens, id_gen, diagnostics);

    let op_token = require_a_token(tokens, diagnostics, "`+=` or `-=`");

    let op = match op_token.text {
        "+=" => AssignUpdateKind::Add,
        "-=" => AssignUpdateKind::Subtract,
        _ => {
            diagnostics.push(ParseError::Invalid {
                position: op_token.position.clone(),
                message: ErrorMessage(vec![
                    msgtext!("Expected "),
                    msgcode!("+="),
                    msgtext!(" or "),
                    msgcode!("-="),
                    msgtext!(", but got "),
                    msgcode!("{}", op_token.text),
                    msgtext!("."),
                ]),
                additional: vec![],
            });
            AssignUpdateKind::Add
        }
    };

    let expr = parse_expression(tokens, id_gen, diagnostics);

    Expression::new(
        Position::merge(&variable.position, &expr.position),
        Expression_::AssignUpdate(variable, op, Rc::new(expr)),
        id_gen.next(),
    )
}

fn parse_toplevel_expr(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let expr = parse_expression(tokens, id_gen, diagnostics);
    ToplevelItem::Expr(ToplevelExpression(expr))
}

fn parse_toplevel_block(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> ToplevelItem {
    let block = parse_block(tokens, id_gen, diagnostics, false);
    ToplevelItem::Block(block)
}

fn parse_toplevel_items_from_tokens(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Vec<ToplevelItem> {
    let mut items: Vec<ToplevelItem> = vec![];

    while !tokens.is_empty() {
        let start_idx = tokens.idx;
        match parse_toplevel_item_from_tokens(tokens, id_gen, diagnostics) {
            Some(item) => {
                let was_invalid = item.is_invalid_or_placeholder();

                items.push(item);
                if was_invalid {
                    break;
                }

                assert!(
                    tokens.idx > start_idx,
                    "The parser should always make forward progress",
                );
            }
            None => break,
        }
    }
    items
}

fn parse_toplevel_item_from_tokens(
    tokens: &mut TokenStream,
    id_gen: &mut IdGenerator,
    diagnostics: &mut Vec<ParseError>,
) -> Option<ToplevelItem> {
    if let Some(token) = tokens.peek() {
        if token.text == "fun"
            || token.text == "method"
            || token.text == "test"
            || token.text == "enum"
            || token.text == "struct"
            || token.text == "external"
            || token.text == "import"
        {
            return parse_definition(tokens, id_gen, diagnostics);
        }

        if token.text == "{" {
            return Some(parse_toplevel_block(tokens, id_gen, diagnostics));
        }
    }

    Some(parse_toplevel_expr(tokens, id_gen, diagnostics))
}

pub(crate) fn parse_inline_expr_from_str(
    vfs_path: &VfsPathBuf,
    src: &str,
    id_gen: &mut IdGenerator,
) -> (Expression, Vec<ParseError>) {
    let mut diagnostics = vec![];

    let (mut tokens, lex_errors) = lex(vfs_path, src);
    for error in lex_errors {
        diagnostics.push(error);
    }

    let expr = parse_expression(&mut tokens, id_gen, &mut diagnostics);
    (expr, diagnostics)
}

pub(crate) fn parse_toplevel_items(
    vfs_path: &VfsPathBuf,
    src: &str,
    id_gen: &mut IdGenerator,
) -> (Vec<ToplevelItem>, Vec<ParseError>) {
    let mut diagnostics = vec![];

    let (mut tokens, lex_errors) = lex(vfs_path, src);
    for error in lex_errors {
        diagnostics.push(error);
    }

    let items = parse_toplevel_items_from_tokens(&mut tokens, id_gen, &mut diagnostics);
    (items, diagnostics)
}

/// Parse all the toplevel items in `src` between `offset` and
/// `end_offset`.
pub(crate) fn parse_toplevel_items_from_span(
    vfs_path: &VfsPathBuf,
    src: &str,
    id_gen: &mut IdGenerator,
    offset: usize,
    end_offset: usize,
) -> (Vec<ToplevelItem>, Vec<ParseError>) {
    let mut diagnostics = vec![];

    let (mut tokens, lex_errors) = lex_between(vfs_path, src, offset, end_offset);
    for error in lex_errors {
        diagnostics.push(error);
    }

    let items = parse_toplevel_items_from_tokens(&mut tokens, id_gen, &mut diagnostics);
    (items, diagnostics)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::Vfs;

    use super::*;

    fn parse_toplevel_items(src: &str) -> (Vec<ToplevelItem>, Vec<ParseError>) {
        let mut vfs = Vfs::default();
        let vfs_path = vfs.insert(Rc::new(PathBuf::from("__test.gdn")), src.to_owned());

        super::parse_toplevel_items(&vfs_path, src, &mut IdGenerator::default())
    }

    #[test]
    fn test_incomplete_expression() {
        let (_, errors) = parse_toplevel_items("1 + ");
        assert!(!errors.is_empty())
    }

    #[test]
    fn test_repeated_param() {
        let (_, errors) = parse_toplevel_items("fun f(x, x) {} ");
        assert!(!errors.is_empty())
    }

    #[test]
    fn test_repeated_param_underscore() {
        let (_, errors) = parse_toplevel_items("fun f(_, _) {} ");
        assert!(errors.is_empty())
    }
}
