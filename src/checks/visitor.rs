use garden_lang_parser::ast::{Block, Expression, Expression_};

pub(crate) trait Checker {
    fn check_expr(&mut self, expr: &Expression);
}

pub(crate) trait Visitor {
    fn visit<C: Checker>(&self, checker: &mut C);
}

impl Visitor for Block {
    fn visit<C: Checker>(&self, checker: &mut C) {
        for expr in &self.exprs {
            expr.visit(checker);
        }
    }
}

impl<T: Visitor> Visitor for Option<T> {
    fn visit<C: Checker>(&self, checker: &mut C) {
        if let Some(inner) = self {
            inner.visit(checker);
        }
    }
}

impl<T: Visitor> Visitor for Box<T> {
    fn visit<C: Checker>(&self, checker: &mut C) {
        self.as_ref().visit(checker);
    }
}

impl Visitor for Expression {
    fn visit<C: Checker>(&self, checker: &mut C) {
        checker.check_expr(self);

        match &self.1 {
            Expression_::Match(scrutinee, cases) => {
                scrutinee.visit(checker);
                for (_, case_expr) in cases {
                    checker.check_expr(case_expr);
                }
            }
            Expression_::If(cond, then_body, else_body) => {
                cond.visit(checker);
                then_body.visit(checker);
                else_body.visit(checker);
            }
            Expression_::While(cond, body) => {
                cond.visit(checker);
                body.visit(checker);
            }
            Expression_::Assign(_, expr) => {
                expr.visit(checker);
            }
            Expression_::Let(_, expr) => {
                expr.visit(checker);
            }
            Expression_::Return(expr) => {
                expr.visit(checker);
            }
            Expression_::ListLiteral(exprs) => {
                for expr in exprs {
                    expr.visit(checker);
                }
            }
            Expression_::StructLiteral(_, field_exprs) => {
                for (_, expr) in field_exprs {
                    expr.visit(checker);
                }
            }
            Expression_::BinaryOperator(lhs, _, rhs) => {
                lhs.visit(checker);
                rhs.visit(checker);
            }
            Expression_::Variable(_) => {}
            Expression_::Call(recv, args) => {
                recv.visit(checker);
                for arg in args {
                    arg.visit(checker);
                }
            }
            Expression_::MethodCall(recv, _, args) => {
                recv.visit(checker);
                for arg in args {
                    arg.visit(checker);
                }
            }
            Expression_::FunLiteral(fun_info) => {
                fun_info.body.visit(checker);
            }
            Expression_::Block(b) => {
                b.visit(checker);
            }
            Expression_::IntLiteral(_) => {}
            Expression_::StringLiteral(_) => {}
        }
    }
}
