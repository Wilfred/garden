use garden_lang_parser::ast::{
    Block, Expression, Expression_, FunInfo, Pattern, Symbol, TypeSymbol,
};

/// A visitor for ASTs.
///
/// Unlike pattern matching, a visitor makes it easy to recurse but
/// hook in to specific variants. For example, looking for
/// occurrences of string literals anywhere in a function body.
pub(crate) trait Visitor {
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
            Expression_::StructLiteral(name_sym, field_exprs) => {
                self.visit_expr_struct_literal(name_sym, field_exprs);
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

    fn visit_expr_struct_literal(&mut self, _: &TypeSymbol, field_exprs: &[(Symbol, Expression)]) {
        for (_, expr) in field_exprs {
            self.visit_expr(expr);
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
