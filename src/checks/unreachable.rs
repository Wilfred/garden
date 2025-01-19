use garden_lang_parser::{
    ast::{Block, Expression_, ToplevelItem},
    visitor::Visitor,
};

use crate::diagnostics::{Diagnostic, Level};

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
        let mut prev_was_return = false;
        for expr in &block.exprs {
            if matches!(expr.expr_, Expression_::Return(_)) {
                prev_was_return = true;
            } else if prev_was_return {
                self.diagnostics.push(Diagnostic {
                    message: "Unreachable code after `return`.".to_owned(),
                    position: expr.position.clone(),
                    level: Level::Warning,
                });
            }

            self.visit_expr(expr);
        }
    }
}
