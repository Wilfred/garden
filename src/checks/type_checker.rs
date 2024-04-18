use garden_lang_parser::ast::{Block, Expression, FunInfo};

use crate::diagnostics::Warning;
use crate::visitor::Visitor;

pub(crate) fn check_types(fun_info: &FunInfo) -> Vec<Warning> {
    let b = &fun_info.body;
    assign_expr_ids(b);

    vec![]
}

fn assign_expr_ids(block: &Block) {
    let mut visitor = AssignExprIds::default();
    visitor.visit_block(block);
}

#[derive(Debug, Default, Clone)]
struct AssignExprIds {
    next_id: usize,
}

impl Visitor for AssignExprIds {
    fn visit_expr(&mut self, expr: &Expression) {
        expr.2
            .set(self.next_id)
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.1)
    }
}
