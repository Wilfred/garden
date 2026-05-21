//! Check for `let` bindings whose right-hand side has type `Unit`.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

use super::type_checker::TCSummary;

struct UnitLetVisitor<'a> {
    summary: &'a TCSummary,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for UnitLetVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expression) {
        if let Expression_::Let(_, _, rhs) = &expr.expr_ {
            if let Some(ty) = self.summary.id_to_ty.get(&rhs.id) {
                if ty.is_unit() {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("Binding a value of type "),
                            msgcode!("Unit"),
                            msgtext!(" to a variable is rarely useful."),
                        ]),
                        position: rhs.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_unit_let(items: &[ToplevelItem], summary: &TCSummary) -> Vec<Diagnostic> {
    let mut visitor = UnitLetVisitor {
        summary,
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
