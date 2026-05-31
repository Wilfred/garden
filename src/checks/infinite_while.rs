//! Check for while loops whose condition only references local
//! variables that are never modified in the body.
//!
//! Such loops either run forever or terminate immediately, both of
//! which are almost always a bug.

use rustc_hash::FxHashSet;

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    Block, Expression, Expression_, FunInfo, InternedSymbolId, LetDestination, ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct InfiniteWhileVisitor {
    diagnostics: Vec<Diagnostic>,
    bound_scopes: Vec<FxHashSet<InternedSymbolId>>,
}

impl InfiniteWhileVisitor {
    fn add_binding(&mut self, id: InternedSymbolId) {
        if let Some(scope) = self.bound_scopes.last_mut() {
            scope.insert(id);
        }
    }

    fn is_locally_bound(&self, id: InternedSymbolId) -> bool {
        self.bound_scopes.iter().any(|scope| scope.contains(&id))
    }
}

/// If `expr` is built only from variable references, literals,
/// parentheses, and binary operators, return the set of variable IDs
/// it references. Otherwise return `None`, indicating that the value
/// of `expr` can change without any of its referenced variables being
/// reassigned (e.g. it contains a call or field access).
fn collect_simple_vars(expr: &Expression) -> Option<FxHashSet<InternedSymbolId>> {
    let mut vars = FxHashSet::default();
    collect_simple_vars_(expr, &mut vars)?;
    Some(vars)
}

fn collect_simple_vars_(expr: &Expression, out: &mut FxHashSet<InternedSymbolId>) -> Option<()> {
    match &expr.expr_ {
        Expression_::Variable(sym) => {
            out.insert(sym.interned_id);
            Some(())
        }
        Expression_::IntLiteral(_)
        | Expression_::FloatLiteral(_)
        | Expression_::StringLiteral(_) => Some(()),
        Expression_::Parentheses(paren) => collect_simple_vars_(&paren.expr, out),
        Expression_::BinaryOperator(lhs, _, rhs) => {
            collect_simple_vars_(lhs, out)?;
            collect_simple_vars_(rhs, out)
        }
        _ => None,
    }
}

struct BodyAnalysis {
    targets: FxHashSet<InternedSymbolId>,
    target_assigned: bool,
    has_exit: bool,
    lambda_depth: usize,
}

impl Visitor for BodyAnalysis {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::Assign(sym, _) | Expression_::AssignUpdate(sym, _, _)
                if self.targets.contains(&sym.interned_id) =>
            {
                // A lambda may or may not be called, so we
                // conservatively assume any assignment inside one
                // might run.
                self.target_assigned = true;
            }
            Expression_::Break | Expression_::Return(_) if self.lambda_depth == 0 => {
                self.has_exit = true;
            }
            _ => {}
        }
        self.visit_expr_(&expr.expr_);
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        self.lambda_depth += 1;
        self.visit_fun_info(fun_info);
        self.lambda_depth -= 1;
    }
}

impl Visitor for InfiniteWhileVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.bound_scopes.push(FxHashSet::default());
        for param in &fun_info.params.params {
            self.add_binding(param.symbol.interned_id);
        }
        self.visit_block(&fun_info.body);
        self.bound_scopes.pop();
    }

    fn visit_block(&mut self, block: &Block) {
        self.bound_scopes.push(FxHashSet::default());
        for expr in &block.exprs {
            self.visit_expr(expr);
        }
        self.bound_scopes.pop();
    }

    fn visit_dest(&mut self, dest: &LetDestination) {
        match dest {
            LetDestination::Symbol(sym) => self.add_binding(sym.interned_id),
            LetDestination::Destructure(syms) => {
                for sym in syms {
                    self.add_binding(sym.interned_id);
                }
            }
        }
    }

    fn visit_expr_while(&mut self, cond: &Expression, body: &Block) {
        if let Some(cond_vars) = collect_simple_vars(cond) {
            if !cond_vars.is_empty() && cond_vars.iter().all(|id| self.is_locally_bound(*id)) {
                let mut analysis = BodyAnalysis {
                    targets: cond_vars,
                    target_assigned: false,
                    has_exit: false,
                    lambda_depth: 0,
                };
                analysis.visit_block(body);

                if !analysis.target_assigned && !analysis.has_exit {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("This "),
                            msgcode!("while"),
                            msgtext!(
                                " loop's condition references only local variables, but none of them are modified in the body."
                            ),
                        ]),
                        position: cond.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }
            }
        }

        self.visit_expr(cond);
        self.visit_block(body);
    }
}

pub(crate) fn check_infinite_while(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = InfiniteWhileVisitor {
        diagnostics: vec![],
        bound_scopes: vec![FxHashSet::default()],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
