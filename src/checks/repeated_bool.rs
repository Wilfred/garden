//! Check for repeated subexpressions in boolean expressions.
//!
//! For example, `x == 1 || x == 2 || x == 1` has a repeated
//! subexpression `x == 1`.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{BinaryOperatorKind, Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct RepeatedBoolVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// Return a string key representing a pure expression, or `None` if
/// the expression has side effects (calls, assignments, etc.).
fn pure_expr_key(expr: &Expression) -> Option<String> {
    pure_expr_key_inner(&expr.expr_)
}

fn pure_expr_key_inner(expr: &Expression_) -> Option<String> {
    match expr {
        Expression_::Variable(sym) => Some(format!("var:{}", sym.name.text)),
        Expression_::IntLiteral(n) => Some(format!("int:{}", n)),
        Expression_::StringLiteral(s) => Some(format!("str:{}", s)),
        Expression_::Parentheses(_, inner, _) => pure_expr_key(inner),
        Expression_::BinaryOperator(lhs, op, rhs) => {
            let lhs_key = pure_expr_key(lhs)?;
            let rhs_key = pure_expr_key(rhs)?;
            Some(format!("({} {} {})", lhs_key, op, rhs_key))
        }
        Expression_::DotAccess(lhs, field) => {
            let lhs_key = pure_expr_key(lhs)?;
            Some(format!("{}.{}", lhs_key, field.name.text))
        }
        Expression_::NamespaceAccess(lhs, name) => {
            let lhs_key = pure_expr_key(lhs)?;
            Some(format!("{}::{}", lhs_key, name.name.text))
        }
        Expression_::ListLiteral(items) => {
            let mut key = String::from("[");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    key.push_str(", ");
                }
                key.push_str(&pure_expr_key(&item.expr)?);
            }
            key.push(']');
            Some(key)
        }
        Expression_::TupleLiteral(items) => {
            let mut key = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    key.push_str(", ");
                }
                key.push_str(&pure_expr_key(item)?);
            }
            key.push(')');
            Some(key)
        }
        // Anything else is either impure or too complex to compare.
        _ => None,
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
        Expression_::Parentheses(_, inner, _) => {
            collect_operands_inner(inner, op_kind, result);
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
                // Build keys for pure operands and find duplicates.
                let mut seen: Vec<(String, &Expression)> = Vec::new();

                for operand in &operands {
                    if let Some(key) = pure_expr_key(operand) {
                        let is_duplicate = seen.iter().any(|(k, _)| k == &key);
                        if is_duplicate {
                            self.diagnostics.push(Diagnostic {
                                message: ErrorMessage(vec![
                                    msgtext!("This expression has already appeared in this "),
                                    msgcode!("{}", op_kind),
                                    msgtext!(" chain."),
                                ]),
                                position: operand.position.clone(),
                                notes: vec![],
                                severity: Severity::Warning,
                                fixes: vec![],
                            });
                        } else {
                            seen.push((key, operand));
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
