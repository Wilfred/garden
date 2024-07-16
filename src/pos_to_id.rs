use garden_lang_parser::ast::{Expression, Symbol, SyntaxId, ToplevelItem, TypeSymbol};

use garden_lang_parser::visitor::Visitor;

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

pub(crate) fn find_expr_of_id(items: &[ToplevelItem], id: SyntaxId) -> Option<Expression> {
    let mut visitor = ExprOfIdFinder { id, expr: None };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.expr
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

    fn visit_type_symbol(&mut self, type_symbol: &TypeSymbol) {
        if type_symbol.position.contains_offset(self.offset) {
            self.found_ids.push(*type_symbol.id.get().unwrap());
        }
    }
}

#[derive(Debug, Clone)]
struct ExprOfIdFinder {
    id: SyntaxId,
    expr: Option<Expression>,
}

impl Visitor for ExprOfIdFinder {
    fn visit_expr(&mut self, expr: &Expression) {
        if expr.id.get() == Some(&self.id) {
            self.expr = Some(expr.clone());
            return;
        }

        self.visit_expr_(&expr.expr_);
    }
}
