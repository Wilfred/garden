//! Check for unnecessary `return` at the end of function bodies.

use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::parser::ast::{Expression_, FunInfo, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct UnnecessaryReturnVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for UnnecessaryReturnVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        if let Some(last_expr) = fun_info.body.exprs.last() {
            if let Expression_::Return(Some(inner_expr)) = &last_expr.expr_ {
                let return_keyword_position = Position {
                    start_offset: last_expr.position.start_offset,
                    end_offset: inner_expr.position.start_offset,
                    line_number: last_expr.position.line_number,
                    end_line_number: inner_expr.position.line_number,
                    column: last_expr.position.column,
                    end_column: inner_expr.position.column,
                    path: last_expr.position.path.clone(),
                    vfs_path: last_expr.position.vfs_path.clone(),
                };

                self.diagnostics.push(Diagnostic {
                    message: ErrorMessage(vec![
                        msgtext!("Unnecessary "),
                        msgcode!("return"),
                        msgtext!(" at the end of a function body."),
                    ]),
                    position: last_expr.position.clone(),
                    notes: vec![],
                    severity: Severity::Warning,
                    fixes: vec![Autofix {
                        description: "Remove unnecessary `return`".to_owned(),
                        position: return_keyword_position,
                        new_text: String::new(),
                    }],
                });
            }
        }

        self.visit_fun_info_default(fun_info);
    }
}

pub(crate) fn check_unnecessary_return(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = UnnecessaryReturnVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
