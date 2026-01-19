use crate::diagnostics::{Autofix, Diagnostic, Severity};
use crate::env::Env;
use crate::parser::ast::{Expression, Expression_, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;

pub(crate) fn check_unused_literals(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut visitor = UnusedLiteralVisitor::new(env);
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics()
}

struct UnusedLiteralVisitor<'a> {
    unused_literals: Vec<Diagnostic>,
    env: &'a Env,
}

impl<'a> UnusedLiteralVisitor<'a> {
    fn new(env: &'a Env) -> UnusedLiteralVisitor<'a> {
        UnusedLiteralVisitor {
            unused_literals: vec![],
            env,
        }
    }

    fn diagnostics(mut self) -> Vec<Diagnostic> {
        self.unused_literals.sort_by_key(|d| d.position.clone());
        self.unused_literals
    }

    fn check_expression(&mut self, expr: &Expression) {
        // Only check expressions whose values are not used
        if expr.value_is_used {
            return;
        }

        let is_literal = matches!(
            &expr.expr_,
            Expression_::IntLiteral(_)
                | Expression_::StringLiteral(_)
                | Expression_::ListLiteral(_)
                | Expression_::DictLiteral(_)
                | Expression_::TupleLiteral(_)
                | Expression_::StructLiteral(_, _)
        );

        if is_literal {
            let fix = Autofix {
                description: "Remove unused value".to_owned(),
                position: self.get_line_position(&expr.position),
                new_text: String::new(),
            };

            self.unused_literals.push(Diagnostic {
                notes: vec![],
                severity: Severity::Warning,
                message: ErrorMessage(vec![Text("Unused value.".to_owned())]),
                position: expr.position.clone(),
                fixes: vec![fix],
            });
        }
    }

    fn get_line_position(&self, position: &Position) -> Position {
        let Some(src) = self.env.vfs.file_src(&position.vfs_path) else {
            // If we can't get the source, just return the original position
            return position.clone();
        };

        // Find the start of the line (including leading whitespace)
        let line_start = src[..position.start_offset]
            .rfind('\n')
            .map(|pos| pos + 1)
            .unwrap_or(0);

        // Find the end of the line (including the newline if present)
        let line_end = src[position.end_offset..]
            .find('\n')
            .map(|pos| position.end_offset + pos + 1)
            .unwrap_or(src.len());

        // Create a new position spanning the entire line
        let mut line_position = position.clone();
        line_position.start_offset = line_start;
        line_position.end_offset = line_end;
        line_position.column = 0;

        line_position
    }
}

impl<'a> Visitor for UnusedLiteralVisitor<'a> {
    fn visit_expr(&mut self, expr: &Expression) {
        self.check_expression(expr);
        self.visit_expr_(&expr.expr_);
    }
}
