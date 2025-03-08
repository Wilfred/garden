use garden_lang_parser::ast::{Expression_, LetDestination};
use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::diagnostics::MessagePart::*;
use garden_lang_parser::visitor::Visitor;
use garden_lang_parser::{
    ast::{Block, Expression, FunInfo, Pattern, Symbol, SymbolName, ToplevelItem, ToplevelItem_},
    position::Position,
};
use rustc_hash::FxHashMap;

use crate::diagnostics::Level;
use crate::{diagnostics::Diagnostic, env::Env};

pub(crate) fn check_unused_variables(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut visitor = UnusedVariableVisitor::new(env);
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics()
}

#[derive(Debug, Clone)]
enum UseState {
    Used,
    /// An unused variable, along with its definition position.
    NotUsed(Position),
}

struct UnusedVariableVisitor<'a> {
    env: &'a Env,
    /// For each scope, the variables defined, the definition
    /// positions, and whether they have been used afterwards.
    bound_scopes: Vec<FxHashMap<SymbolName, UseState>>,
    unused: Vec<(SymbolName, Position)>,
}

impl UnusedVariableVisitor<'_> {
    fn new(env: &Env) -> UnusedVariableVisitor<'_> {
        UnusedVariableVisitor {
            env,
            bound_scopes: vec![FxHashMap::default()],
            unused: vec![],
        }
    }

    fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut unused = self.unused.clone();
        unused.sort_by_key(|(_, position)| position.clone());

        let mut diagnostics = vec![];
        for (name, position) in unused {
            diagnostics.push(Diagnostic {
                level: Level::Warning,
                message: ErrorMessage(vec![
                    Code(format!("{name}")),
                    Text(" is unused.".to_owned()),
                ]),
                position: position.clone(),
            });
        }

        diagnostics
    }

    /// Is `name` locally bound in syntactic context we're currently
    /// checking?
    fn is_locally_bound(&self, name: &SymbolName) -> bool {
        if name.name == "__BUILTIN_IMPLEMENTATION" {
            return true;
        }

        for scope in &self.bound_scopes {
            if scope.contains_key(name) {
                return true;
            }
        }

        false
    }

    /// Mark `name`, a local variable, as used in `self.bound_scopes`.
    fn mark_used(&mut self, name: &SymbolName) {
        if name.name == "__BUILTIN_IMPLEMENTATION" {
            // Mark everything as used, because this is just a stub.
            for scope in self.bound_scopes.iter_mut() {
                let keys = scope.keys().cloned().collect::<Vec<_>>();

                for name in keys {
                    scope.insert(name.clone(), UseState::Used);
                }
            }
            return;
        }

        for scope in self.bound_scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.clone(), UseState::Used);
                return;
            }
        }

        panic!("Tried to mark an unbound variable {name} as used.")
    }

    fn add_binding(&mut self, symbol: &Symbol) {
        let scope = self
            .bound_scopes
            .last_mut()
            .expect("Should always be non-empty");
        scope.insert(
            symbol.name.clone(),
            UseState::NotUsed(symbol.position.clone()),
        );
    }

    fn push_scope(&mut self) {
        self.bound_scopes.push(FxHashMap::default());
    }

    fn pop_scope(&mut self) {
        let scope = self
            .bound_scopes
            .pop()
            .expect("Tried to pop an empty scope stack.");

        for (name, use_state) in scope.into_iter() {
            // TODO: Use the actual receiver symbol name rather than
            // hardcoding `self` here.
            if name.to_string().starts_with('_') || name.to_string() == "self" {
                continue;
            }

            if let UseState::NotUsed(position) = use_state {
                self.unused.push((name, position));
            }
        }
    }

    fn check_symbol(&mut self, var: &Symbol) {
        if self.is_locally_bound(&var.name) {
            self.mark_used(&var.name);
        }
    }
}

impl Visitor for UnusedVariableVisitor<'_> {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        match &item.2 {
            ToplevelItem_::Expr(_) => {}
            _ => {
                self.push_scope();
            }
        }

        if let ToplevelItem_::Method(method_info, _) = &item.2 {
            self.add_binding(&method_info.receiver_sym);
            // Always treat the method receiver as used, because we
            // can't avoid defining this parameter even when we don't
            // use it.
            self.mark_used(&method_info.receiver_sym.name);
        }

        self.visit_item_(&item.2);

        // Don't worry about unused variables in top level
        // expressions, as they're legitimate in a REPL. If the user
        // has written `let x = 1` they might be planning on using `x`
        // in their next REPL expression!
        if let ToplevelItem_::Expr(toplevel_expr) = &item.2 {
            if let Expression_::Let(dest, _, _) = &toplevel_expr.0.expr_ {
                match dest {
                    LetDestination::Symbol(symbol) => {
                        self.mark_used(&symbol.name);
                    }
                    LetDestination::Destructure(symbols) => {
                        for symbol in symbols {
                            self.mark_used(&symbol.name);
                        }
                    }
                }
            }
        }

        match &item.2 {
            ToplevelItem_::Expr(_) => {}
            _ => {
                self.pop_scope();
            }
        }
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.push_scope();
        for param in &fun_info.params.params {
            self.add_binding(&param.symbol);
        }

        self.visit_block(&fun_info.body);

        self.pop_scope();
    }

    fn visit_expr_variable(&mut self, var: &Symbol) {
        self.check_symbol(var);
    }

    fn visit_dest(&mut self, dest: &LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => {
                self.add_binding(symbol);
            }
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.add_binding(symbol);
                }
            }
        }
    }

    fn visit_expr_assign(&mut self, var: &Symbol, expr: &Expression) {
        self.check_symbol(var);
        self.visit_expr(expr);
    }

    fn visit_expr_assign_update(&mut self, symbol: &Symbol, expr: &Expression) {
        self.check_symbol(symbol);
        self.visit_expr(expr);
    }

    fn visit_expr_for_in(&mut self, dest: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_expr(expr);

        self.push_scope();
        self.visit_dest(dest);
        self.visit_block(body);
        self.pop_scope();
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Block)]) {
        self.visit_expr(scrutinee);
        for (pattern, case_expr) in cases {
            // TODO: add a check that there's an enum with this
            // variant, and that we've covered all the variants.

            self.push_scope();
            if let Some(pattern_arg) = &pattern.argument {
                self.add_binding(pattern_arg);
            }

            self.visit_block(case_expr);
            self.pop_scope();
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.push_scope();

        for expr in &block.exprs {
            self.visit_expr(expr);
        }

        self.pop_scope();
    }
}
