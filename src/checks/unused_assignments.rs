//! Check for assignments where the value is never read afterwards.
//!
//! This detects dead stores: `x = expr` or `x += expr` where the
//! assigned value is never subsequently read before the variable is
//! reassigned or goes out of scope.

use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    Block, Expression, Expression_, FunInfo, InternedSymbolId, LetDestination, Pattern, Symbol,
    SymbolName, TestInfo, ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct UnusedAssignmentVisitor {
    diagnostics: Vec<Diagnostic>,
    /// Tracks the last unread assignment for each variable.
    /// Maps variable ID to (name, position of the assignment expression).
    pending: FxHashMap<InternedSymbolId, (SymbolName, Position)>,
    /// Whether we're inside a function, method, or test body.
    in_definition: bool,
    /// Depth of loop nesting. When > 0, don't record pending
    /// assignments because variables may be read on the next iteration.
    loop_depth: u32,
}

impl UnusedAssignmentVisitor {
    fn new() -> Self {
        Self {
            diagnostics: vec![],
            pending: FxHashMap::default(),
            in_definition: false,
            loop_depth: 0,
        }
    }

    fn record_assignment(&mut self, sym: &Symbol, position: &Position) {
        if sym.name.text.starts_with('_') || sym.name.text == "self" {
            return;
        }
        self.pending
            .insert(sym.interned_id, (sym.name.clone(), position.clone()));
    }

    fn warn_if_pending(&mut self, sym: &Symbol) {
        if let Some((name, pos)) = self.pending.remove(&sym.interned_id) {
            self.emit_warning(name, pos);
        }
    }

    fn mark_read(&mut self, sym: &Symbol) {
        self.pending.remove(&sym.interned_id);
    }

    fn warn_remaining(&mut self) {
        let remaining: Vec<_> = self.pending.drain().collect();
        for (_, (name, pos)) in remaining {
            if !name.text.starts_with('_') && name.text != "self" {
                self.emit_warning(name, pos);
            }
        }
    }

    fn emit_warning(&mut self, name: SymbolName, position: Position) {
        self.diagnostics.push(Diagnostic {
            message: ErrorMessage(vec![
                msgcode!("{}", name),
                msgtext!(" is not read after this assignment."),
            ]),
            position,
            notes: vec![],
            severity: Severity::Warning,
            fixes: vec![],
        });
    }
}

impl Visitor for UnusedAssignmentVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        let saved_pending = std::mem::take(&mut self.pending);
        let saved_in_def = self.in_definition;
        let saved_loop = self.loop_depth;

        self.in_definition = true;
        self.loop_depth = 0;

        self.visit_fun_info_default(fun_info);
        self.warn_remaining();

        self.pending = saved_pending;
        self.in_definition = saved_in_def;
        self.loop_depth = saved_loop;
    }

    fn visit_test_info(&mut self, test_info: &TestInfo) {
        let saved_pending = std::mem::take(&mut self.pending);
        let saved_in_def = self.in_definition;
        let saved_loop = self.loop_depth;

        self.in_definition = true;
        self.loop_depth = 0;

        self.visit_block(&test_info.body);
        self.warn_remaining();

        self.pending = saved_pending;
        self.in_definition = saved_in_def;
        self.loop_depth = saved_loop;
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        // Closures can capture outer variables, so conservatively
        // assume all pending assignments might be read by the closure.
        self.pending.clear();
        self.visit_fun_info(fun_info);
    }

    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::Assign(sym, rhs) => {
                // Visit RHS first (might read the variable, e.g. x = x + 1).
                self.visit_expr(rhs);
                if self.in_definition {
                    if self.loop_depth == 0 {
                        self.warn_if_pending(sym);
                        self.record_assignment(sym, &expr.position);
                    } else {
                        // In a loop, the variable might be read on the next iteration.
                        self.pending.remove(&sym.interned_id);
                    }
                }
            }
            Expression_::AssignUpdate(sym, _, rhs) => {
                // Visit RHS first.
                self.visit_expr(rhs);
                // += reads the current value of the variable.
                self.mark_read(sym);
                if self.in_definition {
                    if self.loop_depth == 0 {
                        self.record_assignment(sym, &expr.position);
                    } else {
                        self.pending.remove(&sym.interned_id);
                    }
                }
            }
            Expression_::Variable(sym) => {
                self.mark_read(sym);
            }
            _ => {
                self.visit_expr_(&expr.expr_);
            }
        }
    }

    fn visit_expr_if(&mut self, cond: &Expression, then_body: &Block, else_body: Option<&Block>) {
        self.visit_expr(cond);

        let snapshot = self.pending.clone();

        self.visit_block(then_body);
        let after_then = std::mem::replace(&mut self.pending, snapshot);

        if let Some(else_body) = else_body {
            self.visit_block(else_body);
        }
        let after_else = std::mem::take(&mut self.pending);

        // Conservatively keep pending only if pending in BOTH branches.
        // If one branch reads the variable, it might not be a dead store.
        let mut merged = FxHashMap::default();
        for (id, entry) in &after_then {
            if after_else.contains_key(id) {
                merged.insert(*id, entry.clone());
            }
        }
        self.pending = merged;
    }

    fn visit_expr_while(&mut self, cond: &Expression, body: &Block) {
        self.visit_expr(cond);
        self.loop_depth += 1;
        self.visit_block(body);
        self.loop_depth -= 1;
    }

    fn visit_expr_for_in(&mut self, dest: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_expr(expr);
        self.loop_depth += 1;
        self.visit_dest(dest);
        self.visit_block(body);
        self.loop_depth -= 1;
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Block)]) {
        self.visit_expr(scrutinee);

        if cases.is_empty() {
            return;
        }

        let snapshot = self.pending.clone();
        let mut branch_results = vec![];

        for (pattern, block) in cases {
            self.pending = snapshot.clone();
            self.visit_symbol(&pattern.variant_sym);
            if let Some(dest) = &pattern.payload {
                self.visit_dest(dest);
            }
            self.visit_block(block);
            branch_results.push(std::mem::take(&mut self.pending));
        }

        // Keep pending only if pending in ALL branches.
        let mut merged = branch_results[0].clone();
        for branch in &branch_results[1..] {
            merged.retain(|id, _| branch.contains_key(id));
        }
        self.pending = merged;
    }
}

pub(crate) fn check_unused_assignments(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnusedAssignmentVisitor::new();
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics.sort_by_key(|d| d.position.clone());
    visitor.diagnostics
}
