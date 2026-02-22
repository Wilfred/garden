//! Check for variables that are only used in recursive calls.
//!
//! If a parameter is only passed to a recursive call and never used
//! for any other computation, it's effectively unused.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    Expression, Expression_, FunInfo, InternedSymbolId, Symbol, SymbolName, ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct ParamUsage {
    interned_id: InternedSymbolId,
    name: SymbolName,
    position: Position,
    used_outside_recursion: bool,
    used_in_recursion: bool,
}

struct FunctionContext {
    function_name: SymbolName,
    params: Vec<ParamUsage>,
}

struct RecursionVariableVisitor {
    diagnostics: Vec<Diagnostic>,
    function_stack: Vec<FunctionContext>,
    in_recursive_args: bool,
}

impl RecursionVariableVisitor {
    fn is_current_function(&self, name: &SymbolName) -> bool {
        self.function_stack
            .last()
            .is_some_and(|ctx| ctx.function_name == *name)
    }

    fn mark_param_usage(&mut self, sym: &Symbol) {
        let in_recursion = self.in_recursive_args;
        for ctx in self.function_stack.iter_mut().rev() {
            for param in &mut ctx.params {
                if param.interned_id == sym.interned_id {
                    if in_recursion {
                        param.used_in_recursion = true;
                    } else {
                        param.used_outside_recursion = true;
                    }
                    return;
                }
            }
        }
    }
}

impl Visitor for RecursionVariableVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        let Some(name_sym) = &fun_info.name_sym else {
            // Closures don't have names, handled by visit_expr_fun_literal.
            self.visit_fun_info_default(fun_info);
            return;
        };

        let params: Vec<ParamUsage> = fun_info
            .params
            .params
            .iter()
            .filter(|p| !p.symbol.name.text.starts_with('_'))
            .map(|p| ParamUsage {
                interned_id: p.symbol.interned_id,
                name: p.symbol.name.clone(),
                position: p.symbol.position.clone(),
                used_outside_recursion: false,
                used_in_recursion: false,
            })
            .collect();

        self.function_stack.push(FunctionContext {
            function_name: name_sym.name.clone(),
            params,
        });

        let prev_in_recursive_args = self.in_recursive_args;
        self.in_recursive_args = false;

        self.visit_block(&fun_info.body);

        self.in_recursive_args = prev_in_recursive_args;
        let ctx = self.function_stack.pop().unwrap();

        for param in &ctx.params {
            if param.used_in_recursion && !param.used_outside_recursion {
                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![
                        msgcode!("{}", param.name),
                        msgtext!(" is only used in recursion, so is effectively unused."),
                    ]),
                    position: param.position.clone(),
                    severity: Severity::Warning,
                    notes: vec![],
                    fixes: vec![],
                });
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        if let Expression_::Call(recv, paren_args) = &expr.expr_ {
            if let Expression_::Variable(var) = &recv.expr_ {
                if self.is_current_function(&var.name) {
                    // This is a recursive call. Visit arguments in
                    // recursive mode.
                    let prev = self.in_recursive_args;
                    self.in_recursive_args = true;
                    for arg in &paren_args.arguments {
                        self.visit_expr(&arg.expr);
                    }
                    self.in_recursive_args = prev;
                    return;
                }
            }
        }

        self.visit_expr_(&expr.expr_);
    }

    fn visit_expr_variable(&mut self, var: &Symbol) {
        self.mark_param_usage(var);
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        // Closures are a barrier: prevent recursive call matching
        // inside the closure body. Variable references inside the
        // closure count as "used outside recursion".
        let prev = self.in_recursive_args;
        self.in_recursive_args = false;

        self.function_stack.push(FunctionContext {
            function_name: SymbolName::from(""),
            params: vec![],
        });

        self.visit_fun_info_default(fun_info);

        self.function_stack.pop();
        self.in_recursive_args = prev;
    }
}

pub(crate) fn check_recursion_variables(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = RecursionVariableVisitor {
        diagnostics: vec![],
        function_stack: vec![],
        in_recursive_args: false,
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
