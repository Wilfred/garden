use crate::{
    Block, Definition, Definition_, EnumInfo, Expression, Expression_, FunInfo, MethodInfo,
    Pattern, StructInfo, Symbol, TestInfo, ToplevelItem, TypeHint, TypeSymbol,
};

/// A visitor for ASTs.
///
/// Unlike pattern matching, a visitor makes it easy to recurse but
/// hook in to specific variants. For example, looking for
/// occurrences of string literals anywhere in a function body.
pub trait Visitor {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        match item {
            ToplevelItem::Def(def) => self.visit_def(def),
            ToplevelItem::Expr(expr) => self.visit_expr(&expr.0),
        }
    }

    fn visit_def(&mut self, def: &Definition) {
        self.visit_def_(&def.2);
    }

    fn visit_def_(&mut self, def_: &Definition_) {
        match &def_ {
            Definition_::Fun(_, fun_info) => self.visit_fun_info(fun_info),
            Definition_::Method(method_info) => self.visit_method_info(method_info),
            Definition_::Test(test_info) => self.visit_test_info(test_info),
            Definition_::Enum(enum_info) => self.visit_enum_info(enum_info),
            Definition_::Struct(struct_info) => self.visit_struct_info(struct_info),
        }
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.visit_method_info_default(method_info);
    }

    fn visit_method_info_default(&mut self, method_info: &MethodInfo) {
        self.visit_type_hint(&method_info.receiver_hint);

        if let Some(fun_info) = method_info.fun_info() {
            self.visit_fun_info(fun_info);
        }
    }

    fn visit_test_info(&mut self, test_info: &TestInfo) {
        self.visit_block(&test_info.body);
    }

    fn visit_enum_info(&mut self, enum_info: &EnumInfo) {
        self.visit_enum_info_default(enum_info);
    }

    fn visit_enum_info_default(&mut self, enum_info: &EnumInfo) {
        for variant in &enum_info.variants {
            if let Some(hint) = &variant.payload_hint {
                self.visit_type_hint(hint);
            }
        }
    }

    fn visit_struct_info(&mut self, struct_info: &StructInfo) {
        self.visit_struct_info_default(struct_info);
    }

    fn visit_struct_info_default(&mut self, struct_info: &StructInfo) {
        for field in &struct_info.fields {
            self.visit_type_hint(&field.hint);
        }
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.visit_fun_info_default(fun_info);
    }

    // The default visit method for a `FunInfo`. This is split out as
    // a separate method because Rust does not have a notion of
    // calling a 'super method'.
    fn visit_fun_info_default(&mut self, fun_info: &FunInfo) {
        for type_param in &fun_info.type_params {
            self.visit_type_symbol(type_param);
        }

        for param in &fun_info.params {
            self.visit_symbol(&param.symbol);

            if let Some(param_hint) = &param.hint {
                self.visit_type_hint(param_hint);
            }
        }

        if let Some(return_hint) = &fun_info.return_hint {
            self.visit_type_hint(return_hint);
        }

        self.visit_block(&fun_info.body);
    }

    fn visit_type_hint(&mut self, type_hint: &TypeHint) {
        self.visit_type_symbol(&type_hint.sym);
        for arg in &type_hint.args {
            self.visit_type_hint(arg);
        }
    }

    fn visit_block(&mut self, block: &Block) {
        for expr in &block.exprs {
            self.visit_expr(expr);
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        self.visit_expr_(&expr.expr_);
    }

    fn visit_expr_(&mut self, expr_: &Expression_) {
        match expr_ {
            Expression_::Match(scrutinee, cases) => {
                self.visit_expr_match(scrutinee, cases);
            }
            Expression_::If(cond, then_body, else_body) => {
                self.visit_expr_if(cond, then_body, else_body.as_ref());
            }
            Expression_::While(cond, body) => {
                self.visit_expr_while(cond, body);
            }
            Expression_::ForIn(sym, expr, body) => {
                self.visit_expr_for_in(sym, expr, body);
            }
            Expression_::Break => {}
            Expression_::Continue => {}
            Expression_::Assign(sym, expr) => {
                self.visit_expr_assign(sym, expr);
            }
            Expression_::AssignUpdate(sym, _, expr) => {
                self.visit_expr_assign_update(sym, expr);
            }
            Expression_::Let(sym, hint, expr) => {
                self.visit_expr_let(sym, hint.as_ref(), expr);
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
            Expression_::TupleLiteral(exprs) => {
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
            Expression_::Call(recv, paren_args) => {
                // TODO: custom method for this variant
                self.visit_expr(recv);
                for arg in &paren_args.arguments {
                    self.visit_expr(arg);
                }
            }
            Expression_::MethodCall(recv, meth_name, paren_args) => {
                self.visit_symbol(meth_name);
                self.visit_expr(recv);
                for arg in &paren_args.arguments {
                    self.visit_expr(arg);
                }
            }
            Expression_::FunLiteral(fun_info) => {
                self.visit_expr_fun_literal(fun_info);
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
            Expression_::DotAccess(recv, _) => {
                // TODO: custom method for this variant
                self.visit_expr(recv);
            }
            Expression_::Invalid => {
                // TODO: custom method for this variant
            }
        }
    }

    fn visit_expr_struct_literal(
        &mut self,
        type_symbol: &TypeSymbol,
        field_exprs: &[(Symbol, Expression)],
    ) {
        self.visit_type_symbol(type_symbol);
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

    fn visit_expr_for_in(&mut self, symbol: &Symbol, expr: &Expression, body: &Block) {
        self.visit_symbol(symbol);
        self.visit_expr(expr);
        self.visit_block(body);
    }

    fn visit_expr_variable(&mut self, symbol: &Symbol) {
        self.visit_symbol(symbol);
    }

    fn visit_expr_let(&mut self, symbol: &Symbol, hint: Option<&TypeHint>, expr: &Expression) {
        self.visit_symbol(symbol);
        if let Some(hint) = hint {
            self.visit_type_hint(hint);
        }

        self.visit_expr(expr);
    }

    fn visit_expr_assign(&mut self, symbol: &Symbol, expr: &Expression) {
        self.visit_symbol(symbol);
        self.visit_expr(expr);
    }

    fn visit_expr_assign_update(&mut self, symbol: &Symbol, expr: &Expression) {
        self.visit_symbol(symbol);
        self.visit_expr(expr);
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &FunInfo) {
        self.visit_fun_info(fun_info);
    }

    fn visit_symbol(&mut self, _: &Symbol) {}

    fn visit_type_symbol(&mut self, _: &TypeSymbol) {}
}
