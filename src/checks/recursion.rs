//! Check for unconditional recursion in functions.
//!
//! A function has unconditional recursion if every code path through
//! its body calls itself, with no base case that could terminate the
//! recursion.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Block, Expression, Expression_, FunInfo, MethodInfo, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct RecursionVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for RecursionVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        if let Some(name_sym) = &fun_info.name_sym {
            if block_always_recurses(&fun_info.body, &name_sym.name.text) {
                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![
                        msgtext!("Function "),
                        msgcode!("{}", name_sym.name),
                        msgtext!(
                            " calls itself on every code path, \
                             which will cause infinite recursion."
                        ),
                    ]),
                    position: name_sym.position.clone(),
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Warning,
                });
            }
        }

        // Continue recursing to check nested function literals.
        self.visit_fun_info_default(fun_info);
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        if let Some(fun_info) = method_info.fun_info() {
            let name = &method_info.name_sym.name.text;
            if block_always_recurses_method(&fun_info.body, name) {
                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![
                        msgtext!("Method "),
                        msgcode!("{}", method_info.full_name()),
                        msgtext!(
                            " calls itself on every code path, \
                             which will cause infinite recursion."
                        ),
                    ]),
                    position: method_info.name_sym.position.clone(),
                    notes: vec![],
                    fixes: vec![],
                    severity: Severity::Warning,
                });
            }
        }

        // Continue recursing to check nested function literals.
        self.visit_method_info_default(method_info);
    }
}

pub(crate) fn check_recursion(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = RecursionVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics
}

/// Does this block always end up calling the function `name`?
fn block_always_recurses(block: &Block, name: &str) -> bool {
    for expr in &block.exprs {
        if expr_always_recurses(expr, name) {
            return true;
        }
        if might_exit_without_recursion(expr, name) {
            return false;
        }
    }
    false
}

/// Does this expression always call the function `name`?
fn expr_always_recurses(expr: &Expression, name: &str) -> bool {
    match &expr.expr_ {
        Expression_::Call(recv, paren_args) => {
            if let Expression_::Variable(sym) = &recv.expr_ {
                if sym.name.text == name {
                    return true;
                }
            }
            // Check if the receiver or any argument always recurses.
            if expr_always_recurses(recv, name) {
                return true;
            }
            paren_args
                .arguments
                .iter()
                .any(|arg| expr_always_recurses(&arg.expr, name))
        }
        Expression_::Return(Some(inner)) => expr_always_recurses(inner, name),
        Expression_::Return(None) => false,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_always_recurses(then_body, name) && block_always_recurses(else_body, name)
        }
        Expression_::If(_, _, None) => false,
        Expression_::Match(_, cases) => {
            !cases.is_empty()
                && cases
                    .iter()
                    .all(|(_, block)| block_always_recurses(block, name))
        }
        Expression_::Let(_, _, rhs) => expr_always_recurses(rhs, name),
        Expression_::Assign(_, rhs) => expr_always_recurses(rhs, name),
        Expression_::Parentheses(_, inner, _) => expr_always_recurses(inner, name),
        Expression_::BinaryOperator(lhs, _, rhs) => {
            expr_always_recurses(lhs, name) || expr_always_recurses(rhs, name)
        }
        Expression_::MethodCall(recv, method_sym, paren_args) => {
            // A method call like `recv.name(args)` isn't a direct
            // function self-call, but the receiver or arguments might
            // contain one.
            let _ = method_sym;
            if expr_always_recurses(recv, name) {
                return true;
            }
            paren_args
                .arguments
                .iter()
                .any(|arg| expr_always_recurses(&arg.expr, name))
        }
        _ => false,
    }
}

/// Could this expression exit the function (via return/break/continue)
/// without calling `name`?
fn might_exit_without_recursion(expr: &Expression, name: &str) -> bool {
    match &expr.expr_ {
        Expression_::Return(Some(inner)) => !expr_always_recurses(inner, name),
        Expression_::Return(None) => true,
        Expression_::Break => true,
        Expression_::Continue => true,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_might_exit_without_recursion(then_body, name)
                || block_might_exit_without_recursion(else_body, name)
        }
        Expression_::If(_, then_body, None) => {
            block_might_exit_without_recursion(then_body, name)
        }
        Expression_::Match(_, cases) => cases
            .iter()
            .any(|(_, block)| block_might_exit_without_recursion(block, name)),
        _ => false,
    }
}

fn block_might_exit_without_recursion(block: &Block, name: &str) -> bool {
    block
        .exprs
        .iter()
        .any(|e| might_exit_without_recursion(e, name))
}

fn block_always_recurses_method(block: &Block, name: &str) -> bool {
    for expr in &block.exprs {
        if expr_always_recurses_method(expr, name) {
            return true;
        }
        if might_exit_without_recursion_method(expr, name) {
            return false;
        }
    }
    false
}

fn expr_always_recurses_method(expr: &Expression, name: &str) -> bool {
    match &expr.expr_ {
        Expression_::MethodCall(_, method_sym, _) => {
            // A method call `self.name(...)` is a self-call if the
            // method name matches. This is a heuristic: the receiver
            // might not be `self`, but it covers the common case.
            method_sym.name.text == name
        }
        Expression_::Return(Some(inner)) => expr_always_recurses_method(inner, name),
        Expression_::Return(None) => false,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_always_recurses_method(then_body, name)
                && block_always_recurses_method(else_body, name)
        }
        Expression_::If(_, _, None) => false,
        Expression_::Match(_, cases) => {
            !cases.is_empty()
                && cases
                    .iter()
                    .all(|(_, block)| block_always_recurses_method(block, name))
        }
        Expression_::Let(_, _, rhs) => expr_always_recurses_method(rhs, name),
        Expression_::Assign(_, rhs) => expr_always_recurses_method(rhs, name),
        Expression_::Parentheses(_, inner, _) => expr_always_recurses_method(inner, name),
        _ => false,
    }
}

fn might_exit_without_recursion_method(expr: &Expression, name: &str) -> bool {
    match &expr.expr_ {
        Expression_::Return(Some(inner)) => !expr_always_recurses_method(inner, name),
        Expression_::Return(None) => true,
        Expression_::Break => true,
        Expression_::Continue => true,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_might_exit_without_recursion_method(then_body, name)
                || block_might_exit_without_recursion_method(else_body, name)
        }
        Expression_::If(_, then_body, None) => {
            block_might_exit_without_recursion_method(then_body, name)
        }
        Expression_::Match(_, cases) => cases
            .iter()
            .any(|(_, block)| block_might_exit_without_recursion_method(block, name)),
        _ => false,
    }
}

fn block_might_exit_without_recursion_method(block: &Block, name: &str) -> bool {
    block
        .exprs
        .iter()
        .any(|e| might_exit_without_recursion_method(e, name))
}
