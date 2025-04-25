use rustc_hash::FxHashSet;

use crate::diagnostics::{Diagnostic, Level};
use crate::parser::ast::{Block, Expression, Expression_, SyntaxId, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::visitor::Visitor;

pub(crate) fn check_unreachable(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnreachableVisitor {
        diagnostics: vec![],
        enclosing_loop_id: None,
        known_terminating_loops: FxHashSet::default(),
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics
}

struct UnreachableVisitor {
    enclosing_loop_id: Option<SyntaxId>,
    known_terminating_loops: FxHashSet<SyntaxId>,
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
                    level: Level::Warning,
                });
                unreachable_reason = None;
            }

            match &expr.expr_ {
                Expression_::While(_, _) => {
                    if !self.known_terminating_loops.contains(&expr.id) {
                        unreachable_reason =
                            Some("Unreachable code after `while` loop which never terminates.");
                    }
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

    fn visit_expr(&mut self, expr: &Expression) {
        let prev_loop_id = self.enclosing_loop_id;

        match &expr.expr_ {
            Expression_::While(expression, _) => {
                self.enclosing_loop_id = Some(expr.id);

                let is_true = if let Expression_::Variable(v) = &expression.expr_ {
                    // TODO: Handle the case when `v` isn't actually
                    // `True` from the prelude because the user has
                    // rebound it.
                    v.name.text == "True"
                } else {
                    false
                };

                if !is_true {
                    self.known_terminating_loops.insert(expr.id);
                }
            }
            Expression_::ForIn(_, _, _) => {
                self.enclosing_loop_id = Some(expr.id);
                self.known_terminating_loops.insert(expr.id);
            }
            Expression_::Break => {
                if let Some(loop_id) = self.enclosing_loop_id {
                    self.known_terminating_loops.insert(loop_id);
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);

        self.enclosing_loop_id = prev_loop_id;
    }
}
