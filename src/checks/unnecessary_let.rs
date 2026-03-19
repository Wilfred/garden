//! Check for unnecessary `let` bindings at the end of blocks, such as
//! `let v = foo() v` which could be simplified to `foo()`.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::parser::ast::{Block, Expression_, LetDestination, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct UnnecessaryLetVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl UnnecessaryLetVisitor {
    fn check_block(&mut self, block: &Block) {
        let exprs = &block.exprs;
        if exprs.len() < 2 {
            return;
        }

        let second_last = &exprs[exprs.len() - 2];
        let last = &exprs[exprs.len() - 1];

        // Check: second-to-last is `let x = expr`, last is `x`.
        if let Expression_::Let(LetDestination::Symbol(let_sym), _, rhs) = &second_last.expr_ {
            if let Expression_::Variable(var_sym) = &last.expr_ {
                if let_sym.name == var_sym.name {
                    let combined_position = Position {
                        start_offset: second_last.position.start_offset,
                        end_offset: last.position.end_offset,
                        line_number: second_last.position.line_number,
                        end_line_number: last.position.end_line_number,
                        column: second_last.position.column,
                        end_column: last.position.end_column,
                        path: second_last.position.path.clone(),
                        vfs_path: second_last.position.vfs_path.clone(),
                    };

                    // Fix: remove `let v = ` prefix and the trailing `v`.
                    // We use two separate fixes to preserve the RHS source text.
                    let prefix_position = Position {
                        start_offset: second_last.position.start_offset,
                        end_offset: rhs.position.start_offset,
                        line_number: second_last.position.line_number,
                        end_line_number: rhs.position.line_number,
                        column: second_last.position.column,
                        end_column: rhs.position.column,
                        path: second_last.position.path.clone(),
                        vfs_path: second_last.position.vfs_path.clone(),
                    };

                    let suffix_position = Position {
                        start_offset: rhs.position.end_offset,
                        end_offset: last.position.end_offset,
                        line_number: rhs.position.end_line_number,
                        end_line_number: last.position.end_line_number,
                        column: rhs.position.end_column,
                        end_column: last.position.end_column,
                        path: last.position.path.clone(),
                        vfs_path: last.position.vfs_path.clone(),
                    };

                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("Unnecessary "),
                            msgcode!("let"),
                            msgtext!(": variable "),
                            msgcode!("{}", let_sym.name),
                            msgtext!(" is immediately returned."),
                        ]),
                        position: combined_position,
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![
                            Autofix {
                                description: "Remove unnecessary `let` binding".to_owned(),
                                position: prefix_position,
                                new_text: String::new(),
                            },
                            Autofix {
                                description: "Remove unnecessary `let` binding".to_owned(),
                                position: suffix_position,
                                new_text: String::new(),
                            },
                        ],
                    });
                }
            }
        }
    }
}

impl Visitor for UnnecessaryLetVisitor {
    fn visit_fun_info(&mut self, fun_info: &crate::parser::ast::FunInfo) {
        self.check_block(&fun_info.body);
        self.visit_fun_info_default(fun_info);
    }

    fn visit_expr(&mut self, expr: &crate::parser::ast::Expression) {
        // Also check blocks inside expressions (if, match, while, for).
        match &expr.expr_ {
            Expression_::If(_, then_block, else_block) => {
                self.check_block(then_block);
                if let Some(else_block) = else_block {
                    self.check_block(else_block);
                }
            }
            Expression_::Match(_, arms) => {
                for (_, block) in arms {
                    self.check_block(block);
                }
            }
            Expression_::While(_, body) => {
                self.check_block(body);
            }
            Expression_::ForIn(_, _, body) => {
                self.check_block(body);
            }
            _ => {}
        }

        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_unnecessary_let(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnnecessaryLetVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
