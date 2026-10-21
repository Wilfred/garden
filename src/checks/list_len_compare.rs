//! Check for comparisons of `xs.len()` against `0` or `1` on lists,
//! which are better expressed as `xs.is_empty()` and
//! `xs.is_non_empty()`. This covers `==` and `!=` as well as ordering
//! comparisons such as `xs.len() > 0` and `xs.len() < 1`.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::garden_type::Type;
use crate::parser::ast::{
    BinaryOperatorKind, Expression, Expression_, MethodInfo, ParenthesizedArguments, Symbol,
    ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};
use std::rc::Rc;

use super::type_checker::TCSummary;

struct ListLenCompareVisitor<'a> {
    summary: &'a TCSummary,
    /// True while we're traversing the body of `is_empty` or
    /// `is_non_empty`, which legitimately compare `.len()` with `0`.
    in_replacement_method: bool,
    diagnostics: Vec<Diagnostic>,
}

/// If `expr` is `recv.len()` and `recv` has type `List<_>`, return the
/// method name symbol and the arguments parens.
fn list_len_call<'a>(
    expr: &'a Expression,
    summary: &TCSummary,
) -> Option<(&'a Symbol, &'a ParenthesizedArguments)> {
    let Expression_::MethodCall(recv, method_sym, args) = &expr.expr_ else {
        return None;
    };
    if method_sym.name.text != "len" {
        return None;
    }
    if !args.arguments.is_empty() {
        return None;
    }

    let recv_ty = summary.id_to_ty.get(&recv.id)?;
    let Type::UserDefined { name, .. } = recv_ty else {
        return None;
    };
    if name.text != "List" {
        return None;
    }

    Some((method_sym, args))
}

fn int_literal(expr: &Expression) -> Option<i64> {
    match &expr.expr_ {
        Expression_::IntLiteral(n) => Some(*n),
        _ => None,
    }
}

/// Given a comparison `recv.len() OP lit` (or the reversed
/// `lit OP recv.len()` when `len_on_left` is false), return the
/// equivalent method (`is_empty` or `is_non_empty`), if any.
fn replacement_method(op: BinaryOperatorKind, lit: i64, len_on_left: bool) -> Option<&'static str> {
    // Normalise to `len OP lit` by flipping the operator when the
    // length is on the right-hand side.
    let op = if len_on_left {
        op
    } else {
        match op {
            BinaryOperatorKind::LessThan => BinaryOperatorKind::GreaterThan,
            BinaryOperatorKind::LessThanOrEqual => BinaryOperatorKind::GreaterThanOrEqual,
            BinaryOperatorKind::GreaterThan => BinaryOperatorKind::LessThan,
            BinaryOperatorKind::GreaterThanOrEqual => BinaryOperatorKind::LessThanOrEqual,
            other => other,
        }
    };

    match (op, lit) {
        (BinaryOperatorKind::Equal, 0) => Some("is_empty"),
        (BinaryOperatorKind::NotEqual, 0) => Some("is_non_empty"),
        (BinaryOperatorKind::GreaterThan, 0) => Some("is_non_empty"),
        (BinaryOperatorKind::GreaterThanOrEqual, 1) => Some("is_non_empty"),
        (BinaryOperatorKind::LessThan, 1) => Some("is_empty"),
        (BinaryOperatorKind::LessThanOrEqual, 0) => Some("is_empty"),
        _ => None,
    }
}

impl Visitor for ListLenCompareVisitor<'_> {
    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        let name = &method_info.name_sym.name.text;
        let is_replacement = name == "is_empty" || name == "is_non_empty";

        let prev = self.in_replacement_method;
        self.in_replacement_method = prev || is_replacement;

        self.visit_method_info_default(method_info);

        self.in_replacement_method = prev;
    }

    fn visit_expr(&mut self, expr: &Expression) {
        if self.in_replacement_method {
            self.visit_expr_(&expr.expr_);
            return;
        }

        if let Expression_::BinaryOperator(lhs, op, rhs) = &expr.expr_ {
            // Match `recv.len() OP lit` and `lit OP recv.len()`, where
            // `lit` is an integer literal.
            let call_and_lit = if let Some((method_sym, args)) = list_len_call(lhs, self.summary) {
                int_literal(rhs).map(|lit| (method_sym, args, &**lhs, &**rhs, lit, true))
            } else if let Some((method_sym, args)) = list_len_call(rhs, self.summary) {
                int_literal(lhs).map(|lit| (method_sym, args, &**rhs, &**lhs, lit, false))
            } else {
                None
            };

            if let Some((method_sym, args, call_expr, lit_expr, lit, len_on_left)) = call_and_lit {
                if let Some(new_method) = replacement_method(op.kind, lit, len_on_left) {
                    // Fix 1: rename `len` to the new method.
                    let rename_fix = Autofix {
                        description: format!("Use `.{}()`", new_method),
                        position: method_sym.position.clone(),
                        new_text: new_method.to_owned(),
                    };

                    // Fix 2: remove the comparison portion (everything
                    // between the call and the literal, on whichever side
                    // it appears).
                    let removal_position =
                        if call_expr.position.start_offset < lit_expr.position.start_offset {
                            // `recv.len() OP lit` — delete from end of `)` to end of `lit`.
                            Position {
                                start_offset: args.close_paren.end_offset,
                                end_offset: lit_expr.position.end_offset,
                                line_number: args.close_paren.end_line_number,
                                end_line_number: lit_expr.position.end_line_number,
                                column: args.close_paren.end_column,
                                end_column: lit_expr.position.end_column,
                                path: Rc::clone(&expr.position.path),
                                vfs_path: expr.position.vfs_path.clone(),
                            }
                        } else {
                            // `lit OP recv.len()` — delete from start of `lit` to
                            // start of receiver.
                            Position {
                                start_offset: lit_expr.position.start_offset,
                                end_offset: call_expr.position.start_offset,
                                line_number: lit_expr.position.line_number,
                                end_line_number: call_expr.position.line_number,
                                column: lit_expr.position.column,
                                end_column: call_expr.position.column,
                                path: Rc::clone(&expr.position.path),
                                vfs_path: expr.position.vfs_path.clone(),
                            }
                        };

                    let removal_fix = Autofix {
                        description: format!("Use `.{}()`", new_method),
                        position: removal_position,
                        new_text: String::new(),
                    };

                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("Prefer "),
                            msgcode!(".{}()", new_method),
                            msgtext!(" over comparing "),
                            msgcode!(".len()"),
                            msgtext!(" with "),
                            msgcode!("{}", lit),
                            msgtext!("."),
                        ]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![rename_fix, removal_fix],
                    });
                }
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_list_len_compare(
    items: &[ToplevelItem],
    summary: &TCSummary,
) -> Vec<Diagnostic> {
    let mut visitor = ListLenCompareVisitor {
        summary,
        in_replacement_method: false,
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
