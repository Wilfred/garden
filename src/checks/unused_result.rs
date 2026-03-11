//! Check that Result values from function/method calls are used.

use crate::checks::type_checker::TCSummary;
use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct UnusedResultVisitor<'a> {
    summary: &'a TCSummary,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for UnusedResultVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expression) {
        if !expr.value_is_used {
            let is_call = matches!(
                &expr.expr_,
                Expression_::Call(..) | Expression_::MethodCall(..)
            );

            if is_call {
                if let Some(ty) = self.summary.id_to_ty.get(&expr.id) {
                    if let Some(name) = ty.type_name() {
                        if name.text == "Result" {
                            self.diagnostics.push(Diagnostic {
                                message: ErrorMessage(vec![
                                    msgtext!("Unused "),
                                    msgcode!("Result"),
                                    msgtext!(" value. Use "),
                                    msgcode!("let _ = ..."),
                                    msgtext!(" if intentionally ignoring the result."),
                                ]),
                                position: expr.position.clone(),
                                severity: Severity::Warning,
                                notes: vec![],
                                fixes: vec![],
                            });
                        }
                    }
                }
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_unused_results(items: &[ToplevelItem], summary: &TCSummary) -> Vec<Diagnostic> {
    let mut visitor = UnusedResultVisitor {
        summary,
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
