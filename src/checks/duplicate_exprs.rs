//! Check for if/else branches with identical bodies and match
//! expressions where every arm has the same body.

use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Severity};
use crate::msgtext;
use crate::parser::ast::{Block, Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;

/// Compare two expressions structurally, ignoring positions and IDs.
fn expr_eq(a: &Expression, b: &Expression) -> bool {
    expr_inner_eq(&a.expr_, &b.expr_)
}

fn symbol_eq(a: &crate::parser::ast::Symbol, b: &crate::parser::ast::Symbol) -> bool {
    a.name.text == b.name.text
}

fn type_symbol_eq(a: &crate::parser::ast::TypeSymbol, b: &crate::parser::ast::TypeSymbol) -> bool {
    a.name.text == b.name.text
}

fn block_eq(a: &Block, b: &Block) -> bool {
    a.exprs.len() == b.exprs.len()
        && a.exprs
            .iter()
            .zip(b.exprs.iter())
            .all(|(ae, be)| expr_eq(ae, be))
}

fn exprs_eq(a: &[Rc<Expression>], b: &[Rc<Expression>]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(ae, be)| expr_eq(ae, be))
}

fn opt_expr_eq(a: &Option<Rc<Expression>>, b: &Option<Rc<Expression>>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => expr_eq(a, b),
        (None, None) => true,
        _ => false,
    }
}

fn expr_inner_eq(a: &Expression_, b: &Expression_) -> bool {
    use Expression_::*;
    match (a, b) {
        (Match(sa, ca), Match(sb, cb)) => {
            expr_eq(sa, sb)
                && ca.len() == cb.len()
                && ca.iter().zip(cb.iter()).all(|((pa, ba), (pb, bb))| {
                    symbol_eq(&pa.variant_sym, &pb.variant_sym)
                        && pa.payload.is_some() == pb.payload.is_some()
                        && block_eq(ba, bb)
                })
        }
        (If(ca, ta, ea), If(cb, tb, eb)) => {
            expr_eq(ca, cb)
                && block_eq(ta, tb)
                && match (ea, eb) {
                    (Some(a), Some(b)) => block_eq(a, b),
                    (None, None) => true,
                    _ => false,
                }
        }
        (While(ca, ba), While(cb, bb)) => expr_eq(ca, cb) && block_eq(ba, bb),
        (Break, Break) | (Continue, Continue) | (Invalid, Invalid) => true,
        (Assign(sa, ea), Assign(sb, eb)) => symbol_eq(sa, sb) && expr_eq(ea, eb),
        (Let(_, _, ea), Let(_, _, eb)) => expr_eq(ea, eb),
        (Return(a), Return(b)) => opt_expr_eq(a, b),
        (IntLiteral(a), IntLiteral(b)) => a == b,
        (StringLiteral(a), StringLiteral(b)) => a == b,
        (ListLiteral(a), ListLiteral(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(ae, be)| expr_eq(&ae.expr, &be.expr))
        }
        (TupleLiteral(a), TupleLiteral(b)) => exprs_eq(a, b),
        (StructLiteral(ta, fa), StructLiteral(tb, fb)) => {
            type_symbol_eq(ta, tb)
                && fa.len() == fb.len()
                && fa
                    .iter()
                    .zip(fb.iter())
                    .all(|((sa, ea), (sb, eb))| symbol_eq(sa, sb) && expr_eq(ea, eb))
        }
        (BinaryOperator(la, oa, ra), BinaryOperator(lb, ob, rb)) => {
            oa == ob && expr_eq(la, lb) && expr_eq(ra, rb)
        }
        (Variable(a), Variable(b)) => symbol_eq(a, b),
        (Call(fa, aa), Call(fb, ab)) => {
            expr_eq(fa, fb)
                && aa.arguments.len() == ab.arguments.len()
                && aa
                    .arguments
                    .iter()
                    .zip(ab.arguments.iter())
                    .all(|(a, b)| expr_eq(&a.expr, &b.expr))
        }
        (MethodCall(ra, sa, aa), MethodCall(rb, sb, ab)) => {
            expr_eq(ra, rb)
                && symbol_eq(sa, sb)
                && aa.arguments.len() == ab.arguments.len()
                && aa
                    .arguments
                    .iter()
                    .zip(ab.arguments.iter())
                    .all(|(a, b)| expr_eq(&a.expr, &b.expr))
        }
        (DotAccess(ea, sa), DotAccess(eb, sb)) => expr_eq(ea, eb) && symbol_eq(sa, sb),
        (NamespaceAccess(ea, sa), NamespaceAccess(eb, sb)) => expr_eq(ea, eb) && symbol_eq(sa, sb),
        (Assert(a), Assert(b)) => expr_eq(a, b),
        (Parentheses(_, a, _), Parentheses(_, b, _)) => expr_eq(a, b),
        _ => false,
    }
}

struct DuplicateExprVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for DuplicateExprVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::If(_, then_body, Some(else_body)) => {
                if !then_body.exprs.is_empty() && block_eq(then_body, else_body) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![msgtext!(
                            "The if and else branches have identical expressions."
                        )]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }
            }
            Expression_::Match(_, cases) => {
                if cases.len() >= 2
                    && !cases[0].1.exprs.is_empty()
                    && cases
                        .windows(2)
                        .all(|pair| block_eq(&pair[0].1, &pair[1].1))
                {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![msgtext!(
                            "All match arms have identical expressions."
                        )]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_duplicate_exprs(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = DuplicateExprVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
