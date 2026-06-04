//! Check for `xs.len() == 0` and `xs.len() != 0` on lists, which are
//! better expressed as `xs.is_empty()` and `xs.is_non_empty()`.

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

fn is_zero_literal(expr: &Expression) -> bool {
    matches!(&expr.expr_, Expression_::IntLiteral(0))
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
            let new_method = match op.kind {
                BinaryOperatorKind::Equal => Some("is_empty"),
                BinaryOperatorKind::NotEqual => Some("is_non_empty"),
                _ => None,
            };

            if let Some(new_method) = new_method {
                // Match `recv.len() == 0` and `0 == recv.len()`.
                let call_and_zero =
                    if let Some((method_sym, args)) = list_len_call(lhs, self.summary) {
                        if is_zero_literal(rhs) {
                            Some((method_sym, args, lhs, rhs))
                        } else {
                            None
                        }
                    } else if let Some((method_sym, args)) = list_len_call(rhs, self.summary) {
                        if is_zero_literal(lhs) {
                            Some((method_sym, args, rhs, lhs))
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                if let Some((method_sym, args, call_expr, zero_expr)) = call_and_zero {
                    // Fix 1: rename `len` to the new method.
                    let rename_fix = Autofix {
                        description: format!("Use `.{}()`", new_method),
                        position: method_sym.position.clone(),
                        new_text: new_method.to_owned(),
                    };

                    // Fix 2: remove the ` == 0` portion (everything between
                    // the call and the zero literal, on whichever side it
                    // appears).
                    let removal_position =
                        if call_expr.position.start_offset < zero_expr.position.start_offset {
                            // `recv.len() == 0` — delete from end of `)` to end of `0`.
                            Position {
                                start_offset: args.close_paren.end_offset,
                                end_offset: zero_expr.position.end_offset,
                                line_number: args.close_paren.end_line_number,
                                end_line_number: zero_expr.position.end_line_number,
                                column: args.close_paren.end_column,
                                end_column: zero_expr.position.end_column,
                                path: Rc::clone(&expr.position.path),
                                vfs_path: expr.position.vfs_path.clone(),
                            }
                        } else {
                            // `0 == recv.len()` — delete from start of `0` to
                            // start of receiver.
                            Position {
                                start_offset: zero_expr.position.start_offset,
                                end_offset: call_expr.position.start_offset,
                                line_number: zero_expr.position.line_number,
                                end_line_number: call_expr.position.line_number,
                                column: zero_expr.position.column,
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
                            msgcode!("0"),
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
