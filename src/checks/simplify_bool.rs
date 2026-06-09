//! Check for boolean expressions that can be simplified.
//!
//! Comparing a value with a boolean literal can be simplified:
//!
//! - `x == True` and `x != False` are just `x`
//! - `x == False` and `x != True` are just `not(x)`
//!
//! Similarly, an `if` whose branches only return boolean literals can
//! be simplified to its condition:
//!
//! - `if c { True } else { False }` is just `c`
//! - `if c { False } else { True }` is just `not(c)`

use std::rc::Rc;

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::parser::ast::{BinaryOperatorKind, Block, Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct SimplifyBoolVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// An `Autofix` that replaces the source between the end of `start`
/// and the end of `end` with `new_text`.
fn replace_to_end(start: &Position, end: &Position, new_text: &str) -> Autofix {
    Autofix {
        description: "Simplify boolean expression".to_owned(),
        position: Position {
            start_offset: start.end_offset,
            end_offset: end.end_offset,
            line_number: start.end_line_number,
            end_line_number: end.end_line_number,
            column: start.end_column,
            end_column: end.end_column,
            path: Rc::clone(&start.path),
            vfs_path: start.vfs_path.clone(),
        },
        new_text: new_text.to_owned(),
    }
}

/// An `Autofix` that replaces the source between the start of `start`
/// and the start of `end` with `new_text`.
fn replace_from_start(start: &Position, end: &Position, new_text: &str) -> Autofix {
    Autofix {
        description: "Simplify boolean expression".to_owned(),
        position: Position {
            start_offset: start.start_offset,
            end_offset: end.start_offset,
            line_number: start.line_number,
            end_line_number: end.line_number,
            column: start.column,
            end_column: end.column,
            path: Rc::clone(&start.path),
            vfs_path: start.vfs_path.clone(),
        },
        new_text: new_text.to_owned(),
    }
}

/// Build the fixes that rewrite a comparison to `operand`, wrapping it
/// in `not(...)` when `negates` is set.
fn comparison_fixes(operand: &Expression, lit_expr: &Expression, negates: bool) -> Vec<Autofix> {
    let operand_first = operand.position.start_offset < lit_expr.position.start_offset;
    match (operand_first, negates) {
        // `operand OP lit` -> `operand`
        (true, false) => vec![replace_to_end(&operand.position, &lit_expr.position, "")],
        // `operand OP lit` -> `not(operand)`
        (true, true) => vec![
            replace_from_start(&operand.position, &operand.position, "not("),
            replace_to_end(&operand.position, &lit_expr.position, ")"),
        ],
        // `lit OP operand` -> `operand`
        (false, false) => vec![replace_from_start(
            &lit_expr.position,
            &operand.position,
            "",
        )],
        // `lit OP operand` -> `not(operand)`
        (false, true) => vec![
            replace_from_start(&lit_expr.position, &operand.position, "not("),
            replace_to_end(&operand.position, &operand.position, ")"),
        ],
    }
}

/// Build the fixes that rewrite `if cond { .. } else { .. }` to `cond`,
/// wrapping it in `not(...)` when `negates` is set.
fn if_fixes(if_expr: &Expression, cond: &Expression, negates: bool) -> Vec<Autofix> {
    let (prefix, suffix) = if negates { ("not(", ")") } else { ("", "") };
    vec![
        replace_from_start(&if_expr.position, &cond.position, prefix),
        replace_to_end(&cond.position, &if_expr.position, suffix),
    ]
}

/// If `expr` is the boolean literal `True` or `False`, return its
/// value. Looks through parentheses.
fn bool_literal(expr: &Expression) -> Option<bool> {
    match &expr.expr_ {
        Expression_::Variable(sym) => match sym.name.text.as_str() {
            "True" => Some(true),
            "False" => Some(false),
            _ => None,
        },
        Expression_::Parentheses(paren) => bool_literal(&paren.expr),
        _ => None,
    }
}

/// If `block` contains a single expression that is a boolean literal,
/// return its value.
fn block_bool_literal(block: &Block) -> Option<bool> {
    match block.exprs.as_slice() {
        [expr] => bool_literal(expr),
        _ => None,
    }
}

/// Build the diagnostic message for a comparison with the boolean
/// literal `lit` that can be simplified. `negates` is true when the
/// simplified expression needs a `not()` call.
fn comparison_message(lit: bool, negates: bool) -> ErrorMessage {
    let lit_code = if lit {
        msgcode!("True")
    } else {
        msgcode!("False")
    };
    if negates {
        ErrorMessage(vec![
            msgtext!("This comparison with "),
            lit_code,
            msgtext!(" can be simplified. Use "),
            msgcode!("not()"),
            msgtext!(" on the value instead."),
        ])
    } else {
        ErrorMessage(vec![
            msgtext!("This comparison with "),
            lit_code,
            msgtext!(" can be simplified. Use the boolean value on its own."),
        ])
    }
}

impl Visitor for SimplifyBoolVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::BinaryOperator(lhs, op, rhs)
                if matches!(
                    op.kind,
                    BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual
                ) =>
            {
                // Find the boolean literal, if one side is a literal
                // and the other isn't, keeping the operand it's
                // compared with.
                let matched = match (bool_literal(lhs), bool_literal(rhs)) {
                    (Some(lit), None) => Some((lit, &**rhs, &**lhs)),
                    (None, Some(lit)) => Some((lit, &**lhs, &**rhs)),
                    _ => None,
                };

                if let Some((lit, operand, lit_expr)) = matched {
                    // `== True` and `!= False` keep the value as-is;
                    // `== False` and `!= True` negate it.
                    let negates = match op.kind {
                        BinaryOperatorKind::Equal => !lit,
                        _ => lit,
                    };
                    self.diagnostics.push(Diagnostic {
                        message: comparison_message(lit, negates),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: comparison_fixes(operand, lit_expr, negates),
                    });
                }
            }
            Expression_::If(cond, then_block, Some(else_block)) => {
                if let (Some(then_lit), Some(else_lit)) = (
                    block_bool_literal(then_block),
                    block_bool_literal(else_block),
                ) {
                    if then_lit != else_lit {
                        // `if c { True } else { False }` is `c`,
                        // `if c { False } else { True }` is `not(c)`.
                        let negates = !then_lit;
                        let message = if negates {
                            ErrorMessage(vec![
                                msgtext!("This "),
                                msgcode!("if"),
                                msgtext!(" can be simplified. Use "),
                                msgcode!("not()"),
                                msgtext!(" on the condition instead."),
                            ])
                        } else {
                            ErrorMessage(vec![
                                msgtext!("This "),
                                msgcode!("if"),
                                msgtext!(" can be simplified. Use the condition directly."),
                            ])
                        };
                        self.diagnostics.push(Diagnostic {
                            message,
                            position: expr.position.clone(),
                            notes: vec![],
                            severity: Severity::Warning,
                            fixes: if_fixes(expr, cond, negates),
                        });
                    }
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_simplify_bool(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = SimplifyBoolVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    // The prelude deliberately compares boolean values with literals
    // in its unit tests, e.g. `assert(True.not() == False)`, so don't
    // flag those.
    visitor
        .diagnostics
        .retain(|d| !d.position.path.ends_with("__prelude.gdn"));

    visitor.diagnostics
}
