//! Find stores overwritten without a read in straight-line code.

use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    BinaryOperatorKind, Block, Expression, Expression_, InternedSymbolId, LetDestination, Symbol,
    ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct DeadStoreVisitor {
    pending: FxHashMap<InternedSymbolId, Symbol>,
    diagnostics: Vec<Diagnostic>,
}

impl DeadStoreVisitor {
    fn write(&mut self, symbol: &Symbol) {
        if let Some(previous) = self.pending.insert(symbol.interned_id, symbol.clone()) {
            self.diagnostics.push(Diagnostic {
                message: ErrorMessage(vec![
                    msgtext!("The value assigned to "),
                    msgcode!("{}", previous.name),
                    msgtext!(" is overwritten before it is read."),
                ]),
                position: previous.position,
                severity: Severity::Warning,
                notes: vec![(
                    ErrorMessage(vec![msgtext!("Overwritten here.")]),
                    symbol.position.clone(),
                )],
                fixes: vec![],
            });
        }
    }
}

impl Visitor for DeadStoreVisitor {
    fn visit_block(&mut self, block: &Block) {
        self.pending.clear();
        for expr in &block.exprs {
            self.visit_expr(expr);
        }
        self.pending.clear();
    }

    fn visit_expr(&mut self, expr: &Expression) {
        // Only compare stores along a straight-line sequence, without
        // inferring execution paths through control flow or closures.
        let boundary = match &expr.expr_ {
            Expression_::If(..)
            | Expression_::Match(..)
            | Expression_::While(..)
            | Expression_::ForIn(..)
            | Expression_::Try(..)
            | Expression_::FunLiteral(..)
            | Expression_::Return(..)
            | Expression_::Break
            | Expression_::Continue => true,
            Expression_::BinaryOperator(_, op, _) => {
                matches!(op.kind, BinaryOperatorKind::And | BinaryOperatorKind::Or)
            }
            _ => false,
        };
        if boundary {
            self.pending.clear();
        }
        self.visit_expr_(&expr.expr_);
        if boundary {
            self.pending.clear();
        }
    }

    fn visit_expr_for_in(&mut self, _dest: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_expr(expr);
        self.visit_block(body);
    }

    fn visit_expr_variable(&mut self, symbol: &Symbol) {
        self.pending.remove(&symbol.interned_id);
    }

    fn visit_dest(&mut self, dest: &LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => {
                // A let introduces a binding, even when it shadows a name.
                self.pending.insert(symbol.interned_id, symbol.clone());
            }
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.pending.insert(symbol.interned_id, symbol.clone());
                }
            }
        }
    }

    fn visit_expr_assign(&mut self, symbol: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
        self.write(symbol);
    }

    fn visit_expr_assign_update(&mut self, symbol: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
        self.visit_expr_variable(symbol);
        self.write(symbol);
    }
}

pub(crate) fn check_dead_stores(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = DeadStoreVisitor {
        pending: FxHashMap::default(),
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
