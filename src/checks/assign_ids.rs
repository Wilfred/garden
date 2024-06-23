use garden_lang_parser::ast::{Block, Expression, ExpressionId, ToplevelItem};

use crate::visitor::Visitor;

#[derive(Debug, Default, Clone)]
struct AssignExprIds {
    next_id: usize,
}

impl Visitor for AssignExprIds {
    fn visit_expr(&mut self, expr: &Expression) {
        expr.id
            .set(ExpressionId(self.next_id))
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.expr_)
    }
}

pub(crate) fn assign_toplevel_item_ids(items: &[ToplevelItem]) {
    let mut visitor = AssignExprIds::default();
    for item in items {
        visitor.visit_toplevel_item(item);
    }
}

pub(crate) fn assign_expr_ids(block: &Block) {
    let mut visitor = AssignExprIds::default();
    visitor.visit_block(block);
}
