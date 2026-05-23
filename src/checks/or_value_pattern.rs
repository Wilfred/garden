//! Check for `match` expressions on `Option` values that could be
//! replaced with a call to `.or_value()`.
//!
//! For example:
//!
//! ```garden
//! match opt {
//!     Some(x) => x,
//!     None => 0,
//! }
//! ```
//!
//! can be written more concisely as `opt.or_value(0)`.

use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{
    Block, Expression, Expression_, LetDestination, MethodInfo, Pattern, ToplevelItem,
};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct OrValuePatternVisitor {
    diagnostics: Vec<Diagnostic>,
    /// True while we are walking the body of `or_value` itself, so we
    /// don't suggest rewriting its own definition.
    in_or_value: bool,
}

/// If `block` contains exactly one expression (ignoring parentheses)
/// that is a reference to a variable named `name`, return true.
fn block_is_just_variable(block: &Block, name: &str) -> bool {
    if block.exprs.len() != 1 {
        return false;
    }
    expr_is_variable(&block.exprs[0], name)
}

fn expr_is_variable(expr: &Expression, name: &str) -> bool {
    match &expr.expr_ {
        Expression_::Variable(sym) => sym.name.text == name,
        Expression_::Parentheses(paren) => expr_is_variable(&paren.expr, name),
        _ => false,
    }
}

/// If `pattern` is `Some(v)` for some variable `v`, return that
/// variable's name.
fn some_pattern_binding(pattern: &Pattern) -> Option<&str> {
    if pattern.variant_sym.name.text != "Some" {
        return None;
    }
    match &pattern.payload {
        Some(LetDestination::Symbol(sym)) => Some(sym.name.text.as_str()),
        _ => None,
    }
}

fn is_none_pattern(pattern: &Pattern) -> bool {
    pattern.variant_sym.name.text == "None" && pattern.payload.is_none()
}

/// Return true if `block` consists of a single expression that is a
/// "simple value": a literal, variable, or a tuple/list/parenthesised
/// expression made out of simple values. Calls and other complex
/// expressions are excluded because passing them to `.or_value()`
/// would evaluate them eagerly even when the option is `Some`, which
/// changes program behaviour.
fn block_is_simple_value(block: &Block) -> bool {
    if block.exprs.len() != 1 {
        return false;
    }
    expr_is_simple_value(&block.exprs[0])
}

fn expr_is_simple_value(expr: &Expression) -> bool {
    match &expr.expr_ {
        Expression_::Variable(_)
        | Expression_::IntLiteral(_)
        | Expression_::FloatLiteral(_)
        | Expression_::StringLiteral(_) => true,
        Expression_::Parentheses(paren) => expr_is_simple_value(&paren.expr),
        Expression_::TupleLiteral(items) => items.iter().all(|e| expr_is_simple_value(e)),
        Expression_::ListLiteral(items) => items.iter().all(|e| expr_is_simple_value(&e.expr)),
        _ => false,
    }
}

impl Visitor for OrValuePatternVisitor {
    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        let prev = self.in_or_value;
        if method_info.name_sym.name.text == "or_value" {
            self.in_or_value = true;
        }
        self.visit_method_info_default(method_info);
        self.in_or_value = prev;
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Block)]) {
        if !self.in_or_value && cases.len() == 2 {
            let (p1, b1) = &cases[0];
            let (p2, b2) = &cases[1];

            let none_block = if let Some(name) = some_pattern_binding(p1) {
                if is_none_pattern(p2) && block_is_just_variable(b1, name) {
                    Some(b2)
                } else {
                    None
                }
            } else if let Some(name) = some_pattern_binding(p2) {
                if is_none_pattern(p1) && block_is_just_variable(b2, name) {
                    Some(b1)
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(none_block) = none_block {
                if block_is_simple_value(none_block) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("This "),
                            msgcode!("match"),
                            msgtext!(" can be replaced with a call to "),
                            msgcode!(".or_value()"),
                            msgtext!("."),
                        ]),
                        position: scrutinee.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }
            }
        }

        // Continue recursing into the scrutinee and case bodies.
        self.visit_expr(scrutinee);
        for (pattern, block) in cases {
            self.visit_symbol(&pattern.variant_sym);
            if let Some(dest) = &pattern.payload {
                self.visit_dest(dest);
            }
            self.visit_block(block);
        }
    }
}

pub(crate) fn check_or_value_pattern(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = OrValuePatternVisitor {
        diagnostics: vec![],
        in_or_value: false,
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
