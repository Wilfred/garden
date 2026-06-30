//! Check for repeated subexpressions in boolean expressions.
//!
//! For example, `x == 1 || x == 2 || x == 1` has a repeated
//! subexpression `x == 1`.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::parser::ast::{
    BinaryOperatorKind, BinaryOperatorSymbol, Expression, Expression_, ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct RepeatedBoolVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// Return true if the expression is pure (no side effects), meaning
/// it's safe to flag as a duplicate when structurally equal.
fn is_pure(expr: &Expression) -> bool {
    is_pure_(&expr.expr_)
}

fn is_pure_(expr: &Expression_) -> bool {
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

/// An operand in a boolean chain, paired with the offset at which a
/// deletion should start if this operand turns out to be a duplicate.
///
/// `delete_from` is the end offset of the operand's left sibling at the
/// binary operator node, so deleting from there to the end of the
/// operand removes ` <op> operand` without crossing a parenthesis
/// boundary. It is `None` for the leftmost operand, which has no left
/// sibling.
struct Operand<'a> {
    expr: &'a Expression,
    delete_from: Option<usize>,
}

/// Collect operands from a boolean chain, returning them as references
/// to the inner expressions. For `a || b || c`, this returns `[a, b, c]`.
fn collect_operands<'a>(expr: &'a Expression, op_sym: &BinaryOperatorSymbol) -> Vec<Operand<'a>> {
    let mut result = Vec::new();
    collect_operands_(expr, op_sym, None, &mut result);
    result
}

fn collect_operands_<'a>(
    expr: &'a Expression,
    op_sym: &BinaryOperatorSymbol,
    delete_from: Option<usize>,
    result: &mut Vec<Operand<'a>>,
) {
    match &expr.expr_ {
        Expression_::BinaryOperator(lhs, op, rhs) if op == op_sym => {
            collect_operands_(lhs, op_sym, delete_from, result);
            // The right operand's left sibling is the whole left
            // subtree, so deletions start at its end (after any closing
            // parenthesis), not at the previous flattened operand.
            collect_operands_(rhs, op_sym, Some(lhs.position.end_offset), result);
        }
        Expression_::Parentheses(paren) => {
            collect_operands_(&paren.expr, op_sym, delete_from, result);
        }
        _ => {
            result.push(Operand { expr, delete_from });
        }
    }
}

/// Return true if `expr` is the root of a boolean chain (i.e. it uses
/// `||` or `&&` but its parent is not the same operator). We detect
/// this by checking if the expression is a `||`/`&&` and handling it
/// only at the top level in `visit_expr`.
fn is_boolean_chain_root(expr: &Expression) -> Option<&BinaryOperatorSymbol> {
    if let Expression_::BinaryOperator(_, op, _) = &expr.expr_ {
        if matches!(op.kind, BinaryOperatorKind::And | BinaryOperatorKind::Or) {
            return Some(op);
        }
    }
    None
}

impl Visitor for RepeatedBoolVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        if let Some(op_sym) = is_boolean_chain_root(expr) {
            let operands = collect_operands(expr, op_sym);

            if operands.len() >= 2 {
                // Compare pure operands by structural equality.
                let mut seen: Vec<&Expression> = Vec::new();

                for operand in &operands {
                    let expr = operand.expr;
                    if is_pure(expr) {
                        if seen.contains(&expr) {
                            // Build a fix that removes ` || operand` by
                            // deleting from the end of the operand's left
                            // sibling to the end of this operand.
                            let fixes = if let Some(delete_from) = operand.delete_from {
                                let mut fix_pos = expr.position.clone();
                                fix_pos.start_offset = delete_from;
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
                                    msgcode!("{}", op_sym.kind.as_src()),
                                    msgtext!(" chain."),
                                ]),
                                position: expr.position.clone(),
                                notes: vec![],
                                severity: Severity::Warning,
                                fixes,
                            });
                        } else {
                            seen.push(expr);
                        }
                    }
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
