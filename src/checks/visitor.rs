use garden_lang_parser::ast::{Block, Expression, Expression_, FunInfo, Pattern, Symbol};

/// A checker inspects every expression in an AST.
///
/// This is separate from the `Visitor` trait, so the checker itself
/// does not need to worry about visiting. There's no super call to
/// forget.
pub(crate) trait Checker {
    fn check_expr(&mut self, _: &Expression) {}
    fn check_pattern(&mut self, _: &Pattern) {}

    fn enter_block(&mut self) {}
    fn leave_block(&mut self) {}
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
                for (pattern, case_expr) in cases {
                    checker.enter_block();
                    checker.check_pattern(pattern);
                    checker.check_expr(case_expr);
                    checker.leave_block();
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
                checker.enter_block();
                fun_info.body.visit(checker);
                checker.leave_block();
            }
            Expression_::Block(b) => {
                checker.enter_block();
                b.visit(checker);
                checker.leave_block();
            }
            Expression_::IntLiteral(_) => {}
            Expression_::StringLiteral(_) => {}
        }
    }
}

pub(crate) trait VisitorMut {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.visit_block(&fun_info.body);
    }

    fn visit_block(&mut self, block: &Block) {
        for expr in &block.exprs {
            self.visit_expr(expr);
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        match &expr.1 {
            Expression_::Match(scrutinee, cases) => {
                self.visit_expr_match(scrutinee.as_ref(), cases);
            }
            Expression_::If(cond, then_body, else_body) => {
                self.visit_expr_if(cond, then_body, else_body.as_ref());
            }
            Expression_::While(cond, body) => {
                self.visit_expr_while(cond.as_ref(), body);
            }
            Expression_::Assign(sym, expr) => {
                self.visit_expr_assign(sym, expr);
            }
            Expression_::Let(sym, expr) => {
                self.visit_expr_let(sym, expr);
            }
            Expression_::Return(expr) => {
                // TODO: custom method for this variant
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            Expression_::ListLiteral(exprs) => {
                // TODO: custom method for this variant
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            Expression_::StructLiteral(_, field_exprs) => {
                // TODO: custom method for this variant
                for (_, expr) in field_exprs {
                    self.visit_expr(expr);
                }
            }
            Expression_::BinaryOperator(lhs, _, rhs) => {
                // TODO: custom method for this variant
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Expression_::Variable(var) => {
                self.visit_expr_variable(var);
            }
            Expression_::Call(recv, args) => {
                // TODO: custom method for this variant
                self.visit_expr(recv);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expression_::MethodCall(recv, _, args) => {
                // TODO: custom method for this variant
                self.visit_expr(recv);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expression_::FunLiteral(fun_info) => {
                self.visit_fun_info(fun_info);
            }
            Expression_::Block(b) => {
                self.visit_block(b);
            }
            Expression_::IntLiteral(_) => {
                // TODO: custom method for this variant
            }
            Expression_::StringLiteral(_) => {
                // TODO: custom method for this variant
            }
        }
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Box<Expression>)]) {
        self.visit_expr(scrutinee);
        for (_, case_expr) in cases {
            self.visit_expr(case_expr);
        }
    }

    fn visit_expr_if(&mut self, cond: &Expression, then_body: &Block, else_body: Option<&Block>) {
        self.visit_expr(cond);
        self.visit_block(then_body);
        if let Some(else_body) = else_body {
            self.visit_block(else_body);
        }
    }

    fn visit_expr_while(&mut self, cond: &Expression, body: &Block) {
        self.visit_expr(cond);
        self.visit_block(body);
    }

    fn visit_expr_variable(&mut self, _: &Symbol) {}

    fn visit_expr_let(&mut self, _: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
    }

    fn visit_expr_assign(&mut self, _: &Symbol, expr: &Expression) {
        self.visit_expr(expr);
    }
}
