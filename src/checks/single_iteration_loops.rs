use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::{Block, Expression, Expression_, FunInfo, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

#[derive(Debug, Clone, Copy, Default)]
struct ControlFlowOutcomes {
    fallthrough: bool,
    break_current_loop: bool,
    continue_current_loop: bool,
    other_exit: bool,
}

impl ControlFlowOutcomes {
    fn fallthrough() -> Self {
        Self {
            fallthrough: true,
            ..Self::default()
        }
    }

    fn break_current_loop() -> Self {
        Self {
            break_current_loop: true,
            ..Self::default()
        }
    }

    fn continue_current_loop() -> Self {
        Self {
            continue_current_loop: true,
            ..Self::default()
        }
    }

    fn other_exit() -> Self {
        Self {
            other_exit: true,
            ..Self::default()
        }
    }

    fn add(&mut self, other: Self) {
        self.fallthrough |= other.fallthrough;
        self.break_current_loop |= other.break_current_loop;
        self.continue_current_loop |= other.continue_current_loop;
        self.other_exit |= other.other_exit;
    }

    fn then(mut self, next: Self) -> Self {
        if self.fallthrough {
            self.fallthrough = false;
            self.add(next);
        }

        self
    }

    fn is_unconditional_break(self) -> bool {
        self.break_current_loop
            && !self.fallthrough
            && !self.continue_current_loop
            && !self.other_exit
    }
}

fn block_outcomes(block: &Block) -> ControlFlowOutcomes {
    let mut outcomes = ControlFlowOutcomes::fallthrough();

    for expr in &block.exprs {
        outcomes = outcomes.then(expr_outcomes(expr));
    }

    outcomes
}

fn expr_outcomes(expr: &Expression) -> ControlFlowOutcomes {
    match &expr.expr_ {
        Expression_::Match(scrutinee, cases) => {
            let mut outcomes = expr_outcomes(scrutinee);

            if outcomes.fallthrough {
                outcomes.fallthrough = false;

                let mut case_outcomes = ControlFlowOutcomes::default();
                for (_, case_block) in cases {
                    case_outcomes.add(block_outcomes(case_block));
                }

                outcomes.add(case_outcomes);
            }

            outcomes
        }
        Expression_::If(cond, then_body, else_body) => {
            let mut outcomes = expr_outcomes(cond);

            if outcomes.fallthrough {
                outcomes.fallthrough = false;

                let mut branch_outcomes = block_outcomes(then_body);
                if let Some(else_body) = else_body {
                    branch_outcomes.add(block_outcomes(else_body));
                } else {
                    branch_outcomes.add(ControlFlowOutcomes::fallthrough());
                }

                outcomes.add(branch_outcomes);
            }

            outcomes
        }
        Expression_::While(cond, body) => nested_loop_outcomes(expr_outcomes(cond), body),
        Expression_::ForIn(_, expr, body) => nested_loop_outcomes(expr_outcomes(expr), body),
        Expression_::Break => ControlFlowOutcomes::break_current_loop(),
        Expression_::Continue => ControlFlowOutcomes::continue_current_loop(),
        Expression_::Assign(_, expr) => {
            expr_outcomes(expr).then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::AssignUpdate(_, _, expr) => {
            expr_outcomes(expr).then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::Let(_, _, expr) => {
            expr_outcomes(expr).then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::Return(Some(expr)) => {
            expr_outcomes(expr).then(ControlFlowOutcomes::other_exit())
        }
        Expression_::Return(None) => ControlFlowOutcomes::other_exit(),
        Expression_::IntLiteral(_)
        | Expression_::StringLiteral(_)
        | Expression_::Variable(_)
        | Expression_::FunLiteral(_)
        | Expression_::Invalid => ControlFlowOutcomes::fallthrough(),
        Expression_::ListLiteral(exprs) => {
            let mut outcomes = ControlFlowOutcomes::fallthrough();
            for expr in exprs {
                outcomes = outcomes.then(expr_outcomes(&expr.expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::DictLiteral(entries) => {
            let mut outcomes = ControlFlowOutcomes::fallthrough();
            for (key_expr, _, value_expr) in entries {
                outcomes = outcomes.then(expr_outcomes(key_expr));
                outcomes = outcomes.then(expr_outcomes(value_expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::TupleLiteral(exprs) => {
            let mut outcomes = ControlFlowOutcomes::fallthrough();
            for expr in exprs {
                outcomes = outcomes.then(expr_outcomes(expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::StructLiteral(_, fields) => {
            let mut outcomes = ControlFlowOutcomes::fallthrough();
            for (_, expr) in fields {
                outcomes = outcomes.then(expr_outcomes(expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::BinaryOperator(lhs, _, rhs) => expr_outcomes(lhs)
            .then(expr_outcomes(rhs))
            .then(ControlFlowOutcomes::fallthrough()),
        Expression_::Call(recv, paren_args) => {
            let mut outcomes = expr_outcomes(recv);
            for arg in &paren_args.arguments {
                outcomes = outcomes.then(expr_outcomes(&arg.expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::MethodCall(recv, _, paren_args) => {
            let mut outcomes = expr_outcomes(recv);
            for arg in &paren_args.arguments {
                outcomes = outcomes.then(expr_outcomes(&arg.expr));
            }
            outcomes.then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::DotAccess(recv, _) | Expression_::NamespaceAccess(recv, _) => {
            expr_outcomes(recv).then(ControlFlowOutcomes::fallthrough())
        }
        Expression_::Assert(expr) => expr_outcomes(expr).then(ControlFlowOutcomes::fallthrough()),
        Expression_::Parentheses(_, expr, _) => expr_outcomes(expr),
    }
}

fn nested_loop_outcomes(
    mut prefix_outcomes: ControlFlowOutcomes,
    body: &Block,
) -> ControlFlowOutcomes {
    if prefix_outcomes.fallthrough {
        prefix_outcomes.fallthrough = false;

        let mut loop_outcomes = ControlFlowOutcomes::fallthrough();
        if block_contains_return(body) {
            loop_outcomes.other_exit = true;
        }

        prefix_outcomes.add(loop_outcomes);
    }

    prefix_outcomes
}

fn block_contains_return(block: &Block) -> bool {
    struct ReturnVisitor {
        found_return: bool,
    }

    impl Visitor for ReturnVisitor {
        fn visit_expr(&mut self, expr: &Expression) {
            if matches!(&expr.expr_, Expression_::Return(_)) {
                self.found_return = true;
                return;
            }

            self.visit_expr_(&expr.expr_);
        }

        fn visit_expr_fun_literal(&mut self, _: &FunInfo) {}
    }

    let mut visitor = ReturnVisitor {
        found_return: false,
    };
    visitor.visit_block(block);
    visitor.found_return
}

struct SingleIterationLoopVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for SingleIterationLoopVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.expr_ {
            Expression_::While(cond, body) => {
                self.visit_expr(cond);

                if block_outcomes(body).is_unconditional_break() {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("This loop always reaches "),
                            msgcode!("break"),
                            msgtext!(" on the first iteration."),
                        ]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }

                self.visit_block(body);
            }
            Expression_::ForIn(_, iterable, body) => {
                self.visit_expr(iterable);

                if block_outcomes(body).is_unconditional_break() {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            msgtext!("This loop always reaches "),
                            msgcode!("break"),
                            msgtext!(" on the first iteration."),
                        ]),
                        position: expr.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                        fixes: vec![],
                    });
                }

                self.visit_block(body);
            }
            _ => self.visit_expr_(&expr.expr_),
        }
    }
}

pub(crate) fn check_single_iteration_loops(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = SingleIterationLoopVisitor {
        diagnostics: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.diagnostics
}
