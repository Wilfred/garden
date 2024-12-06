use garden_lang_parser::ast::{
    AstId, Expression, LetDestination, Symbol, SyntaxId, ToplevelItem, TypeHint, TypeSymbol,
};

use garden_lang_parser::visitor::Visitor;

/// All the items (expressions, symbols) whose position includes
/// `offset` and `end_offset`, outermost first.
pub(crate) fn find_item_at(items: &[ToplevelItem], offset: usize, end_offset: usize) -> Vec<AstId> {
    let mut visitor = IdFinder {
        offset,
        end_offset,
        found_ids: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.found_ids
}

/// Find the expression whose ID is `id`. If the ID points to a symbol
/// in an assignment, return the RHS of that assignment.
pub(crate) fn find_expr_of_id(items: &[ToplevelItem], id: SyntaxId) -> Option<Expression> {
    let mut visitor = ExprOfIdFinder { id, expr: None };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.expr
}

/// Stores a vec of all the expressions whose expression includes
/// both `offset` and `end_offset`.
#[derive(Debug, Default, Clone)]
struct IdFinder {
    offset: usize,
    end_offset: usize,
    found_ids: Vec<AstId>,
}

impl Visitor for IdFinder {
    fn visit_expr(&mut self, expr: &Expression) {
        let pos = &expr.pos;
        if !(pos.contains_offset(self.offset) && pos.contains_offset(self.end_offset)) {
            return;
        }

        self.found_ids.push(AstId::Expr(expr.id));
        self.visit_expr_(&expr.expr_)
    }

    fn visit_symbol(&mut self, symbol: &Symbol) {
        if symbol.position.contains_offset(self.offset)
            && symbol.position.contains_offset(self.end_offset)
        {
            self.found_ids.push(AstId::Sym(symbol.id));
        }
    }

    fn visit_type_symbol(&mut self, type_symbol: &TypeSymbol) {
        if type_symbol.position.contains_offset(self.offset)
            && type_symbol.position.contains_offset(self.end_offset)
        {
            self.found_ids.push(AstId::TypeSym(type_symbol.id));
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
        if expr.id == self.id {
            self.expr = Some(expr.clone());
            return;
        }

        self.visit_expr_(&expr.expr_);
    }

    fn visit_expr_let(&mut self, dest: &LetDestination, _: Option<&TypeHint>, expr: &Expression) {
        let symbols = match dest {
            LetDestination::Symbol(symbol) => {
                vec![symbol.clone()]
            }
            LetDestination::Destructure(symbols) => symbols.to_vec(),
        };

        for symbol in symbols {
            if symbol.id == self.id {
                self.expr = Some(expr.clone());
                return;
            }
        }

        self.visit_expr(expr);
    }
}
