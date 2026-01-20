use rustc_hash::FxHashMap;

use crate::diagnostics::{Autofix, Diagnostic, Severity};
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

/// Information about type parameters in a function/method.
#[derive(Debug, Clone)]
struct TypeParamInfo {
    /// All type parameters and their use states.
    params: Vec<(TypeSymbol, UseState)>,
}

#[derive(Debug, Clone)]
enum UseState {
    Used,
    /// An unused variable, along with its definition position.
    NotUsed(Position),
}

/// Tracks information needed for import use checking.
#[derive(Debug, Clone)]
enum ImportUseState {
    Used,
    /// Symbol position and full import position.
    NotUsed {
        symbol_position: Position,
        import_position: Position,
    },
}

/// Information about an unused local variable and how to fix it.
#[derive(Debug, Clone)]
struct UnusedLocalVar {
    name: SymbolName,
    /// Position of the variable name (for the warning).
    name_position: Position,
    /// How to fix this unused variable.
    fix: UnusedVarFix,
}

/// The type of fix to apply for an unused variable.
#[derive(Debug, Clone)]
enum UnusedVarFix {
    /// Rename the variable with underscore prefix (for parameters).
    Rename,
    /// Remove the `let x = ` part, keeping just the expression.
    RemoveLet { removal_position: Position },
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
    file_bindings: FxHashMap<SymbolName, ImportUseState>,

    /// Unused local variables with their fix info.
    unused: Vec<UnusedLocalVar>,

    /// For let bindings, track the removal position (from `let` to value expr).
    /// Key is the interned symbol ID.
    let_removal_positions: FxHashMap<InternedSymbolId, Position>,

    method_this_type_hint: Option<TypeHint>,

    /// Type parameter tracking for the current function/method.
    /// Stores all type params and their use states.
    type_param_info: Vec<TypeParamInfo>,

    /// Unused type parameters with their removal info.
    unused_type_params: Vec<UnusedTypeParam>,
}

/// Information about an unused type parameter and how to remove it.
#[derive(Debug, Clone)]
struct UnusedTypeParam {
    name: TypeName,
    /// Position of the type parameter name (for the warning).
    name_position: Position,
    /// Position to remove (may include comma and the `<>` brackets).
    removal_position: Position,
}

impl UnusedVariableVisitor {
    fn new() -> UnusedVariableVisitor {
        UnusedVariableVisitor {
            bound_scopes: vec![vec![]],
            file_bindings: FxHashMap::default(),
            unused: vec![],
            let_removal_positions: FxHashMap::default(),
            method_this_type_hint: None,
            type_param_info: vec![],
            unused_type_params: vec![],
        }
    }

    fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        // Unused local variables
        for unused_var in &self.unused {
            let fix = match &unused_var.fix {
                UnusedVarFix::Rename => Autofix {
                    description: format!("Rename to `_{}`", unused_var.name),
                    position: unused_var.name_position.clone(),
                    new_text: format!("_{}", unused_var.name),
                },
                UnusedVarFix::RemoveLet { removal_position } => Autofix {
                    description: "Remove this let binding".to_owned(),
                    position: removal_position.clone(),
                    new_text: String::new(),
                },
            };

            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![
                    msgcode!("{}", unused_var.name),
                    msgtext!(" is unused."),
                ]),
                position: unused_var.name_position.clone(),
                fixes: vec![fix],
            });
        }

        // Unused imports - remove the entire import
        for (name, use_state) in &self.file_bindings {
            let ImportUseState::NotUsed {
                symbol_position,
                import_position,
            } = use_state
            else {
                continue;
            };
            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![msgcode!("{name}"), msgtext!(" is unused.")]),
                position: symbol_position.clone(),
                fixes: vec![Autofix {
                    description: "Remove this import".to_owned(),
                    position: import_position.clone(),
                    new_text: String::new(),
                }],
            });
        }

        // Unused type parameters - remove the type parameter
        for unused_type_param in &self.unused_type_params {
            diagnostics.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![
                    msgcode!("{}", unused_type_param.name),
                    msgtext!(" is unused."),
                ]),
                position: unused_type_param.name_position.clone(),
                fixes: vec![Autofix {
                    description: "Remove this type parameter".to_owned(),
                    position: unused_type_param.removal_position.clone(),
                    new_text: String::new(),
                }],
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

        for (id, name, use_state) in scope.into_iter() {
            // TODO: Use the actual receiver symbol name rather than
            // hardcoding `self` here.
            if name.to_string().starts_with('_') || name.to_string() == "self" {
                continue;
            }

            if let UseState::NotUsed(position) = use_state {
                // Check if this is a let binding with removal info
                let fix = if let Some(removal_position) = self.let_removal_positions.remove(&id) {
                    UnusedVarFix::RemoveLet { removal_position }
                } else {
                    UnusedVarFix::Rename
                };

                self.unused.push(UnusedLocalVar {
                    name,
                    name_position: position,
                    fix,
                });
            }
        }
    }

    fn check_symbol(&mut self, var: &Symbol) {
        if let Some(use_state) = self.file_bindings.get_mut(&var.name) {
            *use_state = ImportUseState::Used;
            return;
        }

        if self.is_locally_bound(var) {
            self.mark_used(var);
        }
    }

    /// Process type parameters after visiting a function, generating
    /// removal positions for unused ones.
    fn process_unused_type_params(
        &mut self,
        type_param_info: &TypeParamInfo,
        open_paren: &Position,
    ) {
        let params = &type_param_info.params;
        if params.is_empty() {
            return;
        }

        // Find which params are unused (and not prefixed with _).
        let unused_indices: Vec<usize> = params
            .iter()
            .enumerate()
            .filter_map(|(i, (tp, state))| {
                if tp.name.text.starts_with('_') {
                    return None;
                }
                if matches!(state, UseState::NotUsed(_)) {
                    Some(i)
                } else {
                    None
                }
            })
            .collect();

        if unused_indices.is_empty() {
            return;
        }

        let all_unused = unused_indices.len() == params.len();

        for &idx in &unused_indices {
            let (tp, _) = &params[idx];
            let name_position = tp.position.clone();

            let removal_position = if all_unused {
                // Remove entire <...> section. The `<` is right before the first
                // type param, and `>` is right after the last one (before open paren).
                let first_tp = &params[0].0;
                let last_tp = &params[params.len() - 1].0;
                Position {
                    // Start at `<` which is one char before the first type param
                    start_offset: first_tp.position.start_offset - 1,
                    // End at `>` which is right before the open paren
                    end_offset: open_paren.start_offset,
                    line_number: first_tp.position.line_number,
                    end_line_number: last_tp.position.end_line_number,
                    column: first_tp.position.column.saturating_sub(1),
                    end_column: open_paren.column,
                    path: tp.position.path.clone(),
                    vfs_path: tp.position.vfs_path.clone(),
                }
            } else if idx == 0 {
                // First param but not all unused: remove "T, " (param and trailing comma+space)
                let next_tp = &params[idx + 1].0;
                Position {
                    start_offset: tp.position.start_offset,
                    end_offset: next_tp.position.start_offset,
                    line_number: tp.position.line_number,
                    end_line_number: next_tp.position.line_number,
                    column: tp.position.column,
                    end_column: next_tp.position.column,
                    path: tp.position.path.clone(),
                    vfs_path: tp.position.vfs_path.clone(),
                }
            } else {
                // Not first param: remove ", T" (leading comma+space and param)
                let prev_tp = &params[idx - 1].0;
                Position {
                    start_offset: prev_tp.position.end_offset,
                    end_offset: tp.position.end_offset,
                    line_number: prev_tp.position.end_line_number,
                    end_line_number: tp.position.end_line_number,
                    column: prev_tp.position.end_column,
                    end_column: tp.position.end_column,
                    path: tp.position.path.clone(),
                    vfs_path: tp.position.vfs_path.clone(),
                }
            };

            self.unused_type_params.push(UnusedTypeParam {
                name: tp.name.clone(),
                name_position,
                removal_position,
            });
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
            ImportUseState::NotUsed {
                symbol_position: namespace_sym.position.clone(),
                import_position: import_info.pos.clone(),
            },
        );
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.method_this_type_hint = Some(method_info.receiver_hint.clone());
        self.visit_method_info_default(method_info);
        self.method_this_type_hint = None;
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        // Track all type parameters with their use state.
        let mut type_param_info = TypeParamInfo {
            params: fun_info
                .type_params
                .iter()
                .map(|tp| (tp.clone(), UseState::NotUsed(tp.position.clone())))
                .collect(),
        };

        // Given a method definition `method foo(this: List<T>)` we
        // want `T` to be marked as used.
        //
        // We ignore the cases where the type arguments are more
        // complex hints, as method definitions are required to be
        // completely generic. `method foo(this: List<(Foo, Bar)>)`
        // isn't supported, for example.
        if let Some(this_type_hint) = &self.method_this_type_hint {
            for (tp, state) in &mut type_param_info.params {
                if tp.name == this_type_hint.sym.name {
                    *state = UseState::Used;
                }
            }

            for type_arg in &this_type_hint.args {
                for (tp, state) in &mut type_param_info.params {
                    if tp.name == type_arg.sym.name {
                        *state = UseState::Used;
                    }
                }
            }
        }

        self.type_param_info.push(type_param_info);

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

        let type_param_info = self.type_param_info.pop().unwrap();
        self.process_unused_type_params(&type_param_info, &fun_info.params.open_paren);
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

    fn visit_expr(&mut self, expr: &Expression) {
        // Special handling for let expressions to track removal positions.
        if let Expression_::Let(dest, hint, value_expr) = &expr.expr_ {
            // Visit the expression before the destination, so we're not
            // confused by cases like `let x = x`.
            self.visit_expr(value_expr);

            // Calculate the removal position: from start of `let` to start of value expr.
            let removal_position = Position {
                start_offset: expr.position.start_offset,
                end_offset: value_expr.position.start_offset,
                line_number: expr.position.line_number,
                end_line_number: value_expr.position.line_number,
                column: expr.position.column,
                end_column: value_expr.position.column,
                path: expr.position.path.clone(),
                vfs_path: expr.position.vfs_path.clone(),
            };

            // Register removal positions for symbols in the destination.
            match dest {
                LetDestination::Symbol(symbol) => {
                    self.let_removal_positions
                        .insert(symbol.interned_id, removal_position);
                }
                LetDestination::Destructure(_) => {
                    // For destructuring, we can't simply remove the let,
                    // so we don't register removal positions.
                }
            }

            self.visit_dest(dest);

            if let Some(hint) = hint {
                self.visit_type_hint(hint);
            }
        } else {
            self.visit_expr_(&expr.expr_);
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
        for type_info in self.type_param_info.iter_mut().rev() {
            for (tp, state) in &mut type_info.params {
                if tp.name == type_sym.name {
                    *state = UseState::Used;
                    return;
                }
            }
        }
    }
}
