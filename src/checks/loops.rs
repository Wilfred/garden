//! Check that loops are well-formed.

use garden_lang_parser::{
    ast::{Block, Expression, Expression_, FunInfo, Symbol, ToplevelItem},
    visitor::Visitor,
};

use crate::diagnostics::{Diagnostic, Level};

struct LoopVisitor {
    in_loop: bool,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for LoopVisitor {
    fn visit_expr_while(&mut self, cond: &Expression, body: &Block) {
        self.visit_expr(cond);

        let prev_in_loop = self.in_loop;
        self.in_loop = true;

        self.visit_block(body);

        self.in_loop = prev_in_loop;
    }

    fn visit_expr_for_in(&mut self, _: &Symbol, expr: &Expression, body: &Block) {
        self.visit_expr(expr);

        let prev_in_loop = self.in_loop;
        self.in_loop = true;

        self.visit_block(body);

        self.in_loop = prev_in_loop;
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        let prev_in_loop = self.in_loop;
        self.in_loop = false;

        self.visit_fun_info(fun_info);

        self.in_loop = prev_in_loop;
    }

    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::Break => {
                if !self.in_loop {
                    self.diagnostics.push(Diagnostic {
                        message: "`break` can only be used inside loops.".to_owned(),
                        position: expr.pos.clone(),
                        level: Level::Error,
                    });
                }
            }
            Expression_::Continue => {
                if !self.in_loop {
                    self.diagnostics.push(Diagnostic {
                        // TODO: Add an example of valid usage.
                        message: "`continue` can only be used inside loops.".to_owned(),
                        position: expr.pos.clone(),
                        level: Level::Error,
                    });
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_loops(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = LoopVisitor {
        in_loop: false,
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
