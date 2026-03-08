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

use super::type_checker::TCSummary;

struct RecursionVisitor<'a> {
    summary: &'a TCSummary,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for RecursionVisitor<'_> {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        if let Some(name_sym) = &fun_info.name_sym {
            let name = &name_sym.name.text;
            let is_self_call = |expr: &Expression| {
                matches!(&expr.expr_,
                    Expression_::Call(recv, _) if matches!(&recv.expr_, Expression_::Variable(sym) if sym.name.text == *name)
                )
            };

            if block_always_recurses(&fun_info.body, &is_self_call) {
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

        self.visit_fun_info_default(fun_info);
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        if let Some(fun_info) = method_info.fun_info() {
            let method_name = &method_info.name_sym.name.text;
            let receiver_type_name = &method_info.receiver_hint.sym.name;
            let summary = self.summary;
            let is_self_call = |expr: &Expression| {
                let Expression_::MethodCall(recv, sym, _) = &expr.expr_ else {
                    return false;
                };
                if sym.name.text != *method_name {
                    return false;
                }
                match summary.id_to_ty.get(&recv.id) {
                    Some(ty) => ty.type_name().as_ref() == Some(receiver_type_name),
                    None => false,
                }
            };

            if block_always_recurses(&fun_info.body, &is_self_call) {
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

        self.visit_method_info_default(method_info);
    }
}

pub(crate) fn check_recursion(items: &[ToplevelItem], summary: &TCSummary) -> Vec<Diagnostic> {
    let mut visitor = RecursionVisitor {
        summary,
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics
}

/// Does this block always end up making a self-call?
fn block_always_recurses(block: &Block, is_self_call: &dyn Fn(&Expression) -> bool) -> bool {
    for expr in &block.exprs {
        if expr_always_recurses(expr, is_self_call) {
            return true;
        }
        if might_exit_without_recursion(expr, is_self_call) {
            return false;
        }
    }
    false
}

/// Does this expression always make a self-call?
fn expr_always_recurses(expr: &Expression, is_self_call: &dyn Fn(&Expression) -> bool) -> bool {
    if is_self_call(expr) {
        return true;
    }

    match &expr.expr_ {
        Expression_::Call(recv, paren_args) => {
            if expr_always_recurses(recv, is_self_call) {
                return true;
            }
            paren_args
                .arguments
                .iter()
                .any(|arg| expr_always_recurses(&arg.expr, is_self_call))
        }
        Expression_::MethodCall(recv, _, paren_args) => {
            if expr_always_recurses(recv, is_self_call) {
                return true;
            }
            paren_args
                .arguments
                .iter()
                .any(|arg| expr_always_recurses(&arg.expr, is_self_call))
        }
        Expression_::Return(Some(inner)) => expr_always_recurses(inner, is_self_call),
        Expression_::Return(None) => false,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_always_recurses(then_body, is_self_call)
                && block_always_recurses(else_body, is_self_call)
        }
        Expression_::If(_, _, None) => false,
        Expression_::Match(_, cases) => {
            !cases.is_empty()
                && cases
                    .iter()
                    .all(|(_, block)| block_always_recurses(block, is_self_call))
        }
        Expression_::Let(_, _, rhs) => expr_always_recurses(rhs, is_self_call),
        Expression_::Assign(_, rhs) => expr_always_recurses(rhs, is_self_call),
        Expression_::Parentheses(paren) => expr_always_recurses(&paren.expr, is_self_call),
        Expression_::BinaryOperator(lhs, _, rhs) => {
            expr_always_recurses(lhs, is_self_call) || expr_always_recurses(rhs, is_self_call)
        }
        _ => false,
    }
}

/// Could this expression exit the function (via return/break/continue)
/// without making a self-call?
fn might_exit_without_recursion(
    expr: &Expression,
    is_self_call: &dyn Fn(&Expression) -> bool,
) -> bool {
    match &expr.expr_ {
        Expression_::Return(Some(inner)) => !expr_always_recurses(inner, is_self_call),
        Expression_::Return(None) => true,
        Expression_::Break => true,
        Expression_::Continue => true,
        Expression_::If(_, then_body, Some(else_body)) => {
            block_might_exit_without_recursion(then_body, is_self_call)
                || block_might_exit_without_recursion(else_body, is_self_call)
        }
        Expression_::If(_, then_body, None) => {
            block_might_exit_without_recursion(then_body, is_self_call)
        }
        Expression_::Match(_, cases) => cases
            .iter()
            .any(|(_, block)| block_might_exit_without_recursion(block, is_self_call)),
        _ => false,
    }
}

fn block_might_exit_without_recursion(
    block: &Block,
    is_self_call: &dyn Fn(&Expression) -> bool,
) -> bool {
    block
        .exprs
        .iter()
        .any(|e| might_exit_without_recursion(e, is_self_call))
}
