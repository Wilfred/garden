//! Check for loops that unconditionally break on every iteration,
//! making the loop body execute at most once.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Block, Expression, Expression_, FunInfo, LetDestination, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct UnconditionalBreakVisitor {
    diagnostics: Vec<Diagnostic>,
}

/// Check whether a block will always reach a `break` statement,
/// regardless of which branch is taken.
fn block_always_breaks(block: &Block) -> bool {
    for expr in &block.exprs {
        if expr_always_breaks(expr) {
            return true;
        }
    }
    false
}

fn expr_always_breaks(expr: &Expression) -> bool {
    match &expr.expr_ {
        Expression_::Break => true,
        Expression_::If(_, then_block, Some(else_block)) => {
            block_always_breaks(then_block) && block_always_breaks(else_block)
        }
        Expression_::Match(_, cases) if !cases.is_empty() => {
            cases.iter().all(|(_, block)| block_always_breaks(block))
        }
        _ => false,
    }
}

impl Visitor for UnconditionalBreakVisitor {
    fn visit_expr_while(&mut self, cond: &Expression, body: &Block) {
        self.visit_expr(cond);

        if block_always_breaks(body) {
            self.diagnostics.push(Diagnostic {
                message: ErrorMessage(vec![
                    msgtext!("This loop always "),
                    msgcode!("break"),
                    msgtext!("s on the first iteration, so it can only execute once."),
                ]),
                position: body.open_brace.clone(),
                notes: vec![],
                severity: Severity::Warning,
                fixes: vec![],
            });
        }

        self.visit_block(body);
    }

    fn visit_expr_for_in(&mut self, _: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_expr(expr);

        if block_always_breaks(body) {
            self.diagnostics.push(Diagnostic {
                message: ErrorMessage(vec![
                    msgtext!("This loop always "),
                    msgcode!("break"),
                    msgtext!("s on the first iteration, so it can only execute once."),
                ]),
                position: body.open_brace.clone(),
                notes: vec![],
                severity: Severity::Warning,
                fixes: vec![],
            });
        }

        self.visit_block(body);
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        // A break inside a closure belongs to the closure, not the
        // outer loop. Continue visiting but don't consider closures
        // as part of the loop body analysis (block_always_breaks
        // won't recurse into closures since it only looks at
        // if/match branches).
        self.visit_fun_info(fun_info);
    }
}

pub(crate) fn check_unconditional_break(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnconditionalBreakVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
