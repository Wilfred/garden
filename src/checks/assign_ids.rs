use garden_lang_parser::ast::{Expression, Symbol, SyntaxId, ToplevelItem};

use crate::visitor::Visitor;

#[derive(Debug, Default, Clone)]
struct AssignSyntaxIds {
    next_id: usize,
}

impl Visitor for AssignSyntaxIds {
    fn visit_expr(&mut self, expr: &Expression) {
        expr.id
            .set(SyntaxId(self.next_id))
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.expr_)
    }

    fn visit_symbol(&mut self, symbol: &Symbol) {
        symbol
            .id
            .set(SyntaxId(self.next_id))
            .expect("Symbols should not have IDs yet.");
        self.next_id += 1;
    }
}

pub(crate) fn assign_toplevel_item_ids(items: &[ToplevelItem]) {
    let mut visitor = AssignSyntaxIds::default();
    for item in items {
        visitor.visit_toplevel_item(item);
    }
}
