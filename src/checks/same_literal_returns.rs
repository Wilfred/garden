use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Block, Expression, Expression_, FunInfo, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

pub(crate) fn check_same_literal_returns(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    for item in items {
        match item {
            ToplevelItem::Fun(sym, fun_info, _) => {
                if let Some((literal, positions)) = check_fun_body(fun_info) {
                    let notes = positions
                        .into_iter()
                        .map(|pos| (ErrorMessage(vec![msgtext!("Same value here.")]), pos))
                        .collect();

                    diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("All code paths in "),
                            msgcode!("{}()", sym.name),
                            msgtext!(" return the same value "),
                            msgcode!("{}", literal),
                            msgtext!("."),
                        ]),
                        position: sym.position.clone(),
                        notes,
                        fixes: vec![],
                        severity: Severity::Warning,
                    });
                }
            }
            ToplevelItem::Method(method_info, _) => {
                if let Some(fun_info) = method_info.fun_info() {
                    if let Some((literal, positions)) = check_fun_body(fun_info) {
                        let notes = positions
                            .into_iter()
                            .map(|pos| (ErrorMessage(vec![msgtext!("Same value here.")]), pos))
                            .collect();

                        diagnostics.push(Diagnostic {
                            message: ErrorMessage(vec![
                                msgtext!("All code paths in "),
                                msgcode!("{}", method_info.full_name()),
                                msgtext!(" return the same value "),
                                msgcode!("{}", literal),
                                msgtext!("."),
                            ]),
                            position: method_info.name_sym.position.clone(),
                            notes,
                            fixes: vec![],
                            severity: Severity::Warning,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    diagnostics
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralValue {
    Int(i64),
    String(String),
    EnumVariant(String),
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Int(n) => write!(f, "{}", n),
            LiteralValue::String(s) => write!(f, "\"{}\"", s),
            LiteralValue::EnumVariant(name) => write!(f, "{}", name),
        }
    }
}

fn expr_as_constant(expr: &Expression) -> Option<LiteralValue> {
    match &expr.expr_ {
        Expression_::IntLiteral(n) => Some(LiteralValue::Int(*n)),
        Expression_::StringLiteral(s) => Some(LiteralValue::String(s.clone())),
        Expression_::Variable(sym) => {
            let name = &sym.name.text;
            if name.starts_with(|c: char| c.is_uppercase()) {
                Some(LiteralValue::EnumVariant(name.clone()))
            } else {
                None
            }
        }
        Expression_::Parentheses(_, inner, _) => expr_as_constant(inner),
        _ => None,
    }
}

/// A visitor that collects all explicit `return` statements,
/// skipping nested closures.
struct ExplicitReturnVisitor {
    literals: Vec<(LiteralValue, Position)>,
    has_non_literal_return: bool,
}

impl Visitor for ExplicitReturnVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::Return(Some(ret_expr)) => {
                if let Some(lit) = expr_as_constant(ret_expr) {
                    self.literals.push((lit, ret_expr.position.clone()));
                } else {
                    self.has_non_literal_return = true;
                }
            }
            Expression_::Return(None) => {
                self.has_non_literal_return = true;
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }

    // Don't recurse into nested closures.
    fn visit_expr_fun_literal(&mut self, _fun_info: &FunInfo) {}
}

/// Check if all code paths in a function body return the same literal.
/// Returns the literal value and positions of all return sites if so.
fn check_fun_body(fun_info: &FunInfo) -> Option<(LiteralValue, Vec<Position>)> {
    let mut visitor = ExplicitReturnVisitor {
        literals: vec![],
        has_non_literal_return: false,
    };
    visitor.visit_block(&fun_info.body);

    if visitor.has_non_literal_return {
        return None;
    }

    let mut literals = visitor.literals;
    let mut has_non_literal_exit = false;
    collect_tail_literals(&fun_info.body, &mut literals, &mut has_non_literal_exit);
    if has_non_literal_exit {
        return None;
    }

    if literals.len() >= 2 && literals.iter().all(|(l, _)| l == &literals[0].0) {
        let value = literals[0].0.clone();
        let positions = literals.into_iter().map(|(_, p)| p).collect();
        Some((value, positions))
    } else {
        None
    }
}

/// Collect literal values from the tail position of a block.
/// Sets `has_non_literal_exit` if some exit path is neither a literal
/// nor an explicit return.
fn collect_tail_literals(
    block: &Block,
    literals: &mut Vec<(LiteralValue, Position)>,
    has_non_literal_exit: &mut bool,
) {
    let Some(tail) = block.exprs.last() else {
        *has_non_literal_exit = true;
        return;
    };

    collect_tail_literals_expr(tail, literals, has_non_literal_exit);
}

fn collect_tail_literals_expr(
    expr: &Expression,
    literals: &mut Vec<(LiteralValue, Position)>,
    has_non_literal_exit: &mut bool,
) {
    if let Some(lit) = expr_as_constant(expr) {
        literals.push((lit, expr.position.clone()));
        return;
    }

    match &expr.expr_ {
        Expression_::Return(_) => {
            // Already counted in explicit returns pass.
        }
        Expression_::If(_, then_block, Some(else_block)) => {
            collect_tail_literals(then_block, literals, has_non_literal_exit);
            collect_tail_literals(else_block, literals, has_non_literal_exit);
        }
        Expression_::If(_, _, None) => {
            // if without else returns Unit implicitly on the else path.
            *has_non_literal_exit = true;
        }
        Expression_::Match(_, cases) => {
            for (_, case_block) in cases {
                collect_tail_literals(case_block, literals, has_non_literal_exit);
            }
        }
        _ => {
            *has_non_literal_exit = true;
        }
    }
}
