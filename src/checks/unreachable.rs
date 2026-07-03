use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Block, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::visitor::Visitor;

pub(crate) fn check_unreachable(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnreachableVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics
}

struct UnreachableVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for UnreachableVisitor {
    fn visit_block(&mut self, block: &Block) {
        let mut unreachable_reason: Option<&str> = None;

        for expr in &block.exprs {
            self.visit_expr(expr);

            if let Some(reason) = unreachable_reason {
                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![Text(reason.to_owned())]),
                    position: expr.position.clone(),
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Warning,
                });
                unreachable_reason = None;
            }

            match &expr.expr_ {
                Expression_::While { .. } if expr.expr_.is_diverging_loop() => {
                    unreachable_reason =
                        Some("Unreachable code after `while` loop which never terminates.");
                }
                Expression_::Return(_) => {
                    unreachable_reason = Some("Unreachable code after `return`.");
                }
                Expression_::Call(recv, _) => {
                    if let Expression_::Variable(v) = &recv.expr_ {
                        if v.name.text == "error" {
                            unreachable_reason = Some("Unreachable code after `error`.");
                        }
                    }
                }
                _ => {}
            }
        }
    }
}
