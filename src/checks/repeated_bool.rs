//! Check for repeated subexpressions in boolean expressions.
//!
//! For example, `x == 1 || x == 2 || x == 1` has a repeated
//! subexpression `x == 1`.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::parser::ast::{BinaryOperatorKind, Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct RepeatedBoolVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// Return true if the expression is pure (no side effects), meaning
/// it's safe to flag as a duplicate when structurally equal.
fn is_pure(expr: &Expression) -> bool {
    is_pure_inner(&expr.expr_)
}

fn is_pure_inner(expr: &Expression_) -> bool {
    match expr {
        Expression_::Variable(_) | Expression_::IntLiteral(_) | Expression_::StringLiteral(_) => {
            true
        }
        Expression_::Parentheses(paren) => is_pure(&paren.expr),
        Expression_::BinaryOperator(lhs, _, rhs) => is_pure(lhs) && is_pure(rhs),
        Expression_::DotAccess(lhs, _) | Expression_::NamespaceAccess(lhs, _) => is_pure(lhs),
        Expression_::ListLiteral(items) => items.iter().all(|item| is_pure(&item.expr)),
        Expression_::TupleLiteral(items) => items.iter().all(|item| is_pure(item)),
        _ => false,
    }
}

/// Collect operands from a boolean chain, returning them as references
/// to the inner expressions. For `a || b || c`, this returns `[a, b, c]`.
fn collect_operands<'a>(expr: &'a Expression, op_kind: &BinaryOperatorKind) -> Vec<&'a Expression> {
    let mut result = Vec::new();
    collect_operands_inner(expr, op_kind, &mut result);
    result
}

fn collect_operands_inner<'a>(
    expr: &'a Expression,
    op_kind: &BinaryOperatorKind,
    result: &mut Vec<&'a Expression>,
) {
    match &expr.expr_ {
        Expression_::BinaryOperator(lhs, op, rhs) if op == op_kind => {
            collect_operands_inner(lhs, op_kind, result);
            collect_operands_inner(rhs, op_kind, result);
        }
        Expression_::Parentheses(paren) => {
            collect_operands_inner(&paren.expr, op_kind, result);
        }
        _ => {
            result.push(expr);
        }
    }
}

/// Return true if `expr` is the root of a boolean chain (i.e. it uses
/// `||` or `&&` but its parent is not the same operator). We detect
/// this by checking if the expression is a `||`/`&&` and handling it
/// only at the top level in `visit_expr`.
fn is_boolean_chain_root(expr: &Expression) -> Option<BinaryOperatorKind> {
    if let Expression_::BinaryOperator(_, op, _) = &expr.expr_ {
        if matches!(op, BinaryOperatorKind::And | BinaryOperatorKind::Or) {
            return Some(*op);
        }
    }
    None
}

impl Visitor for RepeatedBoolVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        if let Some(op_kind) = is_boolean_chain_root(expr) {
            let operands = collect_operands(expr, &op_kind);

            if operands.len() >= 2 {
                // Compare pure operands by structural equality.
                let mut seen: Vec<&Expression> = Vec::new();
                let mut prev_operand: Option<&Expression> = None;

                for operand in &operands {
                    if is_pure(operand) {
                        if seen.contains(operand) {
                            // Build a fix that removes ` || operand` by
                            // deleting from the previous operand's end
                            // to this operand's end.
                            let fixes = if let Some(prev) = prev_operand {
                                let mut fix_pos = operand.position.clone();
                                fix_pos.start_offset = prev.position.end_offset;
                                vec![Autofix {
                                    description: "Remove this duplicate".to_owned(),
                                    position: fix_pos,
                                    new_text: String::new(),
                                }]
                            } else {
                                vec![]
                            };

                            self.diagnostics.push(Diagnostic {
                                message: ErrorMessage(vec![
                                    msgtext!("This expression has already appeared in this "),
                                    msgcode!("{}", op_kind),
                                    msgtext!(" chain."),
                                ]),
                                position: operand.position.clone(),
                                notes: vec![],
                                severity: Severity::Warning,
                                fixes,
                            });
                        } else {
                            seen.push(operand);
                        }
                    }
                    prev_operand = Some(operand);
                }
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_repeated_bool(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = RepeatedBoolVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
