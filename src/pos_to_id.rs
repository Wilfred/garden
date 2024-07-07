use garden_lang_parser::ast::{Expression, Symbol, SyntaxId, ToplevelItem};

use crate::visitor::Visitor;

pub(crate) fn find_item_at(items: &[ToplevelItem], offset: usize) -> Vec<SyntaxId> {
    let mut visitor = IdFinder {
        offset,
        found_ids: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.found_ids
}

/// Stores a vec of all the expressions whose expression includes
/// `offset`.
#[derive(Debug, Default, Clone)]
struct IdFinder {
    offset: usize,
    found_ids: Vec<SyntaxId>,
}

impl Visitor for IdFinder {
    fn visit_expr(&mut self, expr: &Expression) {
        let pos = &expr.pos;
        if !pos.contains_offset(self.offset) {
            return;
        }

        self.found_ids.push(*expr.id.get().unwrap());
        self.visit_expr_(&expr.expr_)
    }

    fn visit_symbol(&mut self, symbol: &Symbol) {
        if symbol.position.contains_offset(self.offset) {
            self.found_ids.push(*symbol.id.get().unwrap());
        }
    }
}
