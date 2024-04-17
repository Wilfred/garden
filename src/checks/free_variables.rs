use std::collections::{HashMap, HashSet};

use garden_lang_parser::{
    ast::{Block, Expression, FunInfo, Pattern, Symbol, SymbolName},
    position::Position,
};

use crate::visitor::Visitor;
use crate::{diagnostics::Warning, env::Env};

pub(crate) fn check_free_variables(
    fun_info: &FunInfo,
    env: &Env,
    receiver_sym: Option<&Symbol>,
) -> Vec<Warning> {
    let mut visitor = FreeVariableVisitor::new(env);
    if let Some(receiver_sym) = receiver_sym {
        visitor.add_binding(&receiver_sym.name);
    }

    visitor.visit_fun_info(fun_info);

    let mut warnings = vec![];
    for (free_sym, position) in visitor.free {
        warnings.push(Warning {
            message: format!("Unbound symbol: {free_sym}"),
            position,
        });
    }

    warnings
}

pub(crate) fn check_free_variables_block(block: &Block, env: &Env) -> Vec<Warning> {
    let mut visitor = FreeVariableVisitor::new(env);
    visitor.visit_block(block);

    let mut warnings = vec![];

    for (free_sym, position) in visitor.free {
        warnings.push(Warning {
            message: format!("Unbound symbol: {free_sym}"),
            position,
        });
    }

    warnings
}

struct FreeVariableVisitor<'a> {
    env: &'a Env,
    bound_scopes: Vec<HashSet<SymbolName>>,
    free: HashMap<SymbolName, Position>,
}

impl FreeVariableVisitor<'_> {
    fn new(env: &Env) -> FreeVariableVisitor<'_> {
        FreeVariableVisitor {
            env,
            bound_scopes: vec![HashSet::new()],
            free: HashMap::new(),
        }
    }

    fn is_bound(&self, name: &SymbolName) -> bool {
        if name.0 == "__BUILTIN_IMPLEMENTATION" {
            return true;
        }

        for scope in &self.bound_scopes {
            if scope.contains(name) {
                return true;
            }
        }

        false
    }

    fn add_binding(&mut self, name: &SymbolName) {
        let scope = self
            .bound_scopes
            .last_mut()
            .expect("Should always be non-empty");
        scope.insert(name.clone());
    }

    fn push_scope(&mut self) {
        self.bound_scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.bound_scopes.pop();
    }

    fn check_symbol(&mut self, var: &Symbol) {
        if !self.is_bound(&var.name) && !self.env.file_scope.contains_key(&var.name) {
            // Variable is free.
            if !self.free.contains_key(&var.name) {
                // Only record the first occurrence as free.
                self.free.insert(var.name.clone(), var.position.clone());
            }
        }
    }
}

impl Visitor for FreeVariableVisitor<'_> {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.push_scope();
        for param in &fun_info.params {
            self.add_binding(&param.symbol.name);
        }

        self.visit_block(&fun_info.body);

        self.pop_scope();
    }

    fn visit_expr_variable(&mut self, var: &Symbol) {
        self.check_symbol(var);
    }

    fn visit_expr_let(&mut self, var: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
        self.add_binding(&var.name);
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
                self.add_binding(&pattern_arg.name);
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
