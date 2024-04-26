use std::collections::HashMap;

use garden_lang_parser::{
    ast::{
        Block, Definition, Definition_, Expression, FunInfo, Pattern, Symbol, SymbolName,
        ToplevelItem,
    },
    position::Position,
};

use crate::{diagnostics::Diagnostic, env::Env};
use crate::{diagnostics::Level, visitor::Visitor};

pub(crate) fn check_free_variables(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut visitor = FreeVariableVisitor::new(env);
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.warnings()
}

#[derive(Debug, Clone)]
enum UseState {
    Used,
    /// An unused variable, along with its definition position.
    NotUsed(Position),
}

struct FreeVariableVisitor<'a> {
    env: &'a Env,
    /// For each scope, the variables defined, the definition
    /// positions, and whether they have been used afterwards.
    bound_scopes: Vec<HashMap<SymbolName, UseState>>,
    free: HashMap<SymbolName, Position>,
    unused: Vec<(SymbolName, Position)>,
}

impl FreeVariableVisitor<'_> {
    fn new(env: &Env) -> FreeVariableVisitor<'_> {
        FreeVariableVisitor {
            env,
            bound_scopes: vec![HashMap::new()],
            free: HashMap::new(),
            unused: vec![],
        }
    }

    fn warnings(&self) -> Vec<Diagnostic> {
        let mut warnings = vec![];
        for (free_sym, position) in &self.free {
            warnings.push(Diagnostic {
                level: Level::Error,
                message: format!("Unbound symbol: {free_sym}"),
                position: position.clone(),
            });
        }

        for (name, position) in &self.unused {
            warnings.push(Diagnostic {
                level: Level::Warning,
                message: format!("`{name}` is unused."),
                position: position.clone(),
            });
        }

        warnings
    }

    fn is_bound(&self, name: &SymbolName) -> bool {
        if name.0 == "__BUILTIN_IMPLEMENTATION" {
            return true;
        }

        for scope in &self.bound_scopes {
            if scope.contains_key(name) {
                return true;
            }
        }

        false
    }

    fn mark_used(&mut self, name: &SymbolName) {
        if name.0 == "__BUILTIN_IMPLEMENTATION" {
            return;
        }

        for scope in self.bound_scopes.iter_mut() {
            if scope.contains_key(name) {
                scope.insert(name.clone(), UseState::Used);
                return;
            }
        }

        panic!("Tried to mark an unbound variable {name} as used.")
    }

    fn add_binding(&mut self, sym: &Symbol) {
        let scope = self
            .bound_scopes
            .last_mut()
            .expect("Should always be non-empty");
        scope.insert(sym.name.clone(), UseState::NotUsed(sym.position.clone()));
    }

    fn push_scope(&mut self) {
        self.bound_scopes.push(HashMap::new());
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
        if self.is_bound(&var.name) {
            self.mark_used(&var.name);
        } else if self.env.file_scope.contains_key(&var.name) {
            // Bound in file scope, nothing to do.
        } else {
            // Variable is free.
            if !self.free.contains_key(&var.name) {
                // Only record the first occurrence as free.
                self.free.insert(var.name.clone(), var.position.clone());
            }
        }
    }
}

impl Visitor for FreeVariableVisitor<'_> {
    fn visit_def(&mut self, def: &Definition) {
        self.push_scope();
        if let Definition_::Method(method_info) = &def.2 {
            self.add_binding(&method_info.receiver_sym);
        }

        self.visit_def_(&def.2);
        self.pop_scope();
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.push_scope();
        for param in &fun_info.params {
            self.add_binding(&param.symbol);
        }

        self.visit_block(&fun_info.body);

        self.pop_scope();
    }

    fn visit_expr_variable(&mut self, var: &Symbol) {
        self.check_symbol(var);
    }

    fn visit_expr_let(&mut self, var: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
        self.add_binding(var);
    }

    fn visit_expr_assign(&mut self, var: &Symbol, expr: &Expression) {
        self.check_symbol(var);
        self.visit_expr(expr);
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Box<Expression>)]) {
        self.visit_expr(scrutinee);
        for (pattern, case_expr) in cases {
            // TODO: add a check that there's an enum with this
            // variant, and that we've covered all the variants.

            self.push_scope();
            if let Some(pattern_arg) = &pattern.argument {
                self.add_binding(pattern_arg);
            }

            self.visit_expr(case_expr);
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
