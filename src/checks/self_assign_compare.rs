//! Check for self-assignment (`x = x`) and self-comparison (`x == x`).

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{BinaryOperatorKind, Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct SelfAssignCompareVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// Extract a comparable key from an expression. Returns `None` for
/// expressions that are too complex to compare syntactically.
fn expr_key(expr: &Expression) -> Option<String> {
    match &expr.expr_ {
        Expression_::Variable(sym) => Some(sym.name.text.clone()),
        Expression_::IntLiteral(n) => Some(format!("int:{}", n)),
        Expression_::StringLiteral(s) => Some(format!("str:{}", s)),
        Expression_::Parentheses(_, inner, _) => expr_key(inner),
        _ => None,
    }
}

fn is_comparison_op(op: &BinaryOperatorKind) -> bool {
    matches!(
        op,
        BinaryOperatorKind::Equal
            | BinaryOperatorKind::NotEqual
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEqual
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEqual
    )
}

impl Visitor for SelfAssignCompareVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::Assign(sym, rhs) => {
                if let Expression_::Variable(var) = &rhs.expr_ {
                    if var.name.text == sym.name.text {
                        self.diagnostics.push(Diagnostic {
                            message: ErrorMessage(vec![
                                msgcode!("{}", sym.name),
                                msgtext!(" is assigned to itself."),
                            ]),
                            position: expr.position.clone(),
                            notes: vec![],
                            severity: Severity::Warning,
                            fixes: vec![],
                        });
                    }
                }
            }
            Expression_::BinaryOperator(lhs, op, rhs) => {
                if is_comparison_op(op) {
                    if let (Some(lhs_key), Some(rhs_key)) = (expr_key(lhs), expr_key(rhs)) {
                        if lhs_key == rhs_key {
                            self.diagnostics.push(Diagnostic {
                                message: ErrorMessage(vec![
                                    msgtext!("Both sides of "),
                                    msgcode!("{}", op),
                                    msgtext!(" are identical."),
                                ]),
                                position: expr.position.clone(),
                                notes: vec![],
                                severity: Severity::Warning,
                                fixes: vec![],
                            });
                        }
                    }
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_self_assign_compare(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = SelfAssignCompareVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
