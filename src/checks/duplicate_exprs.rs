//! Check for if/else branches with identical bodies and match
//! expressions where every arm has the same body.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::msgtext;
use crate::parser::ast::{Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;

struct DuplicateExprVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for DuplicateExprVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::If(_, then_body, Some(else_body)) => {
                if !then_body.exprs.is_empty() && then_body == else_body {
                    // Replace the entire if/else with just the
                    // then-block body by removing everything before
                    // and after it.
                    let mut before = expr.position.clone();
                    before.end_offset = then_body.open_brace.end_offset;

                    let mut after = then_body.close_brace.clone();
                    after.end_offset = expr.position.end_offset;

                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![msgtext!(
                            "The if and else branches have identical expressions."
                        )]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![
                            Autofix {
                                description: "Replace with the body".to_owned(),
                                position: before,
                                new_text: String::new(),
                            },
                            Autofix {
                                description: "Replace with the body".to_owned(),
                                position: after,
                                new_text: String::new(),
                            },
                        ],
                    });
                }
            }
            Expression_::Match(_, cases) => {
                if cases.len() >= 2
                    && !cases[0].1.exprs.is_empty()
                    && cases.windows(2).all(|pair| pair[0].1 == pair[1].1)
                {
                    let first_body = &cases[0].1;

                    let mut before = expr.position.clone();
                    before.end_offset = first_body.open_brace.end_offset;

                    let mut after = first_body.close_brace.clone();
                    after.end_offset = expr.position.end_offset;

                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![msgtext!(
                            "All match arms have identical expressions."
                        )]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![
                            Autofix {
                                description: "Replace with the body".to_owned(),
                                position: before,
                                new_text: String::new(),
                            },
                            Autofix {
                                description: "Replace with the body".to_owned(),
                                position: after,
                                new_text: String::new(),
                            },
                        ],
                    });
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_duplicate_exprs(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = DuplicateExprVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
