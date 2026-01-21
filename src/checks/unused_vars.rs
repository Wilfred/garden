use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    Block, Expression, Expression_, FunInfo, ImportInfo, InternedSymbolId, LetDestination,
    MethodInfo, Pattern, Symbol, SymbolName, ToplevelItem, TypeHint, TypeName, TypeSymbol,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

pub(crate) fn check_unused_variables(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnusedVariableVisitor::new();
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

struct UnusedVariableVisitor {
    /// For each scope, the variables defined, the definition
    /// positions, and whether they have been used afterwards.
    ///
    /// Note that a variable may occur multiple times in the same
    /// scope, e.g.
    ///
    /// ```
    /// let x = 1
    /// let x = x + 1
    /// ```
    bound_scopes: Vec<Vec<(InternedSymbolId, SymbolName, UseState)>>,
    /// Symbols that are bound in the current file, such as `foo` in
    /// `import "x.gdn" as foo`.
    file_bindings: FxHashMap<SymbolName, UseState>,

    /// Unused variables (e.g. locals, parameters).
    unused: Vec<(SymbolName, Position)>,

    method_this_type_hint: Option<TypeHint>,

    /// All type variables in the current definition, and whether
    /// they're used.
    type_vars: Vec<FxHashMap<TypeName, UseState>>,

    /// Unused type parameters.
    unused_type_vars: Vec<(TypeName, Position)>,
}

impl UnusedVariableVisitor {
    fn new() -> UnusedVariableVisitor {
        UnusedVariableVisitor {
            bound_scopes: vec![vec![]],
            file_bindings: FxHashMap::default(),
            unused: vec![],
            method_this_type_hint: None,
            type_vars: vec![],
            unused_type_vars: vec![],
        }
    }

    fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        for (name, position) in &self.unused {
            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgcode!("{}", name), msgtext!(" is unused.")]),
                position: position.clone(),
                fixes: vec![],
            });
        }

        for (name, use_state) in &self.file_bindings {
            let UseState::NotUsed(position) = use_state else {
                continue;
            };
            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgcode!("{}", name), msgtext!(" is unused.")]),
                position: position.clone(),
                fixes: vec![],
            });
        }

        for (name, position) in &self.unused_type_vars {
            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgcode!("{}", name), msgtext!(" is unused.")]),
                position: position.clone(),
                fixes: vec![],
            });
        }

        diagnostics.sort_by_key(|d| d.position.clone());
        diagnostics
    }

    /// Is `name` locally bound in the syntactic context we're currently
    /// checking?
    fn is_locally_bound(&self, sym: &Symbol) -> bool {
        if sym.name.text == "__BUILT_IN_IMPLEMENTATION" {
            return true;
        }

        for scope in &self.bound_scopes {
            if scope.iter().any(|(id, _, _)| *id == sym.interned_id) {
                return true;
            }
        }

        false
    }

    /// Mark `name`, a local variable, as used in `self.bound_scopes`.
    fn mark_used(&mut self, sym: &Symbol) {
        if sym.name.text == "__BUILT_IN_IMPLEMENTATION" {
            // Mark everything as used, because this is just a stub.
            for scope in self.bound_scopes.iter_mut() {
                for (_, _, state) in scope.iter_mut() {
                    *state = UseState::Used;
                }
            }
            return;
        }

        for scope in self.bound_scopes.iter_mut().rev() {
            if let Some((_, _, state)) = scope
                .iter_mut()
                .rev()
                .find(|(id, _, _)| *id == sym.interned_id)
            {
                *state = UseState::Used;
                return;
            }
        }

        panic!("Tried to mark an unbound variable {} as used.", sym.name)
    }

    fn add_binding(&mut self, symbol: &Symbol) {
        let scope = self
            .bound_scopes
            .last_mut()
            .expect("Should always be non-empty");
        scope.push((
            symbol.interned_id,
            symbol.name.clone(),
            UseState::NotUsed(symbol.position.clone()),
        ));
    }

    fn push_scope(&mut self) {
        self.bound_scopes.push(vec![]);
    }

    fn pop_scope(&mut self) {
        let scope = self
            .bound_scopes
            .pop()
            .expect("Tried to pop an empty scope stack.");

        for (_id, name, use_state) in scope.into_iter() {
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
        if let Some(use_state) = self.file_bindings.get_mut(&var.name) {
            *use_state = UseState::Used;
            return;
        }

        if self.is_locally_bound(var) {
            self.mark_used(var);
        }
    }
}

impl Visitor for UnusedVariableVisitor {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        match &item {
            ToplevelItem::Expr(_) => {}
            _ => {
                self.push_scope();
            }
        }

        if let ToplevelItem::Method(method_info, _) = &item {
            self.add_binding(&method_info.receiver_sym);
            // Always treat the method receiver as used, because we
            // can't avoid defining this parameter even when we don't
            // use it.
            self.mark_used(&method_info.receiver_sym);
        }

        self.visit_toplevel_item_default(item);

        // Don't worry about unused variables in top level
        // expressions, as they're legitimate in a REPL. If the user
        // has written `let x = 1` they might be planning on using `x`
        // in their next REPL expression!
        if let ToplevelItem::Expr(toplevel_expr) = &item {
            if let Expression_::Let(dest, _, _) = &toplevel_expr.0.expr_ {
                match dest {
                    LetDestination::Symbol(symbol) => {
                        self.mark_used(symbol);
                    }
                    LetDestination::Destructure(symbols) => {
                        for symbol in symbols {
                            self.mark_used(symbol);
                        }
                    }
                }
            }
        }

        match &item {
            ToplevelItem::Expr(_) => {}
            _ => {
                self.pop_scope();
            }
        }
    }

    fn visit_import_info(&mut self, import_info: &ImportInfo) {
        let Some(namespace_sym) = &import_info.namespace_sym else {
            return;
        };

        // We don't want to complain on unused placeholder symbols,
        // they indicate an incomplete expression with a parse error.
        if namespace_sym.name.is_placeholder() {
            return;
        }

        self.file_bindings.insert(
            namespace_sym.name.clone(),
            UseState::NotUsed(namespace_sym.position.clone()),
        );
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.method_this_type_hint = Some(method_info.receiver_hint.clone());
        self.visit_method_info_default(method_info);
        self.method_this_type_hint = None;
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        let mut type_vars_in_scope = FxHashMap::default();
        for type_param in &fun_info.type_params {
            type_vars_in_scope.insert(
                type_param.name.clone(),
                UseState::NotUsed(type_param.position.clone()),
            );
        }

        // Given a method definition `method foo(this: List<T>)` we
        // want `T` to be marked as used.
        //
        // We ignore the cases where the type arguments are more
        // complex hints, as method definitions are required to be
        // completely generic. `method foo(this: List<(Foo, Bar)>)`
        // isn't supported, for example.
        if let Some(this_type_hint) = &self.method_this_type_hint {
            if type_vars_in_scope.contains_key(&this_type_hint.sym.name) {
                type_vars_in_scope.insert(this_type_hint.sym.name.clone(), UseState::Used);
            }

            for type_arg in &this_type_hint.args {
                if type_vars_in_scope.contains_key(&type_arg.sym.name) {
                    type_vars_in_scope.insert(type_arg.sym.name.clone(), UseState::Used);
                }
            }
        }

        self.type_vars.push(type_vars_in_scope);

        if let Some(ret_hint) = &fun_info.return_hint {
            self.visit_type_hint(ret_hint);
        }

        self.push_scope();
        for param in &fun_info.params.params {
            self.add_binding(&param.symbol);

            if let Some(param_hint) = &param.hint {
                self.visit_type_hint(param_hint);
            }
        }

        self.visit_block(&fun_info.body);

        self.pop_scope();

        let type_scope = self.type_vars.pop().unwrap();
        for (name, use_state) in type_scope {
            if name.text.starts_with("_") {
                continue;
            }

            if let UseState::NotUsed(position) = use_state {
                self.unused_type_vars.push((name, position));
            }
        }
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
            if let Some(payload_dest) = &pattern.payload {
                self.visit_dest(payload_dest);
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

    fn visit_type_symbol(&mut self, type_sym: &TypeSymbol) {
        for type_scope in self.type_vars.iter_mut().rev() {
            if type_scope.contains_key(&type_sym.name) {
                type_scope.insert(type_sym.name.clone(), UseState::Used);
                return;
            }
        }
    }
}
