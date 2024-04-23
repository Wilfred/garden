use garden_lang_parser::ast::{
    Block, Definition, Definition_, EnumInfo, Expression, Expression_, FunInfo, MethodInfo,
    Pattern, StructInfo, Symbol, TestInfo, ToplevelItem, TypeHint, TypeSymbol,
};

/// A visitor for ASTs.
///
/// Unlike pattern matching, a visitor makes it easy to recurse but
/// hook in to specific variants. For example, looking for
/// occurrences of string literals anywhere in a function body.
pub(crate) trait Visitor {
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
        self.visit_type_hint(&method_info.receiver_type);

        if let Some(fun_info) = method_info.fun_info() {
            self.visit_fun_info(&fun_info);
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
        for param in &fun_info.params {
            if let Some(param_hint) = &param.type_ {
                self.visit_type_hint(param_hint);
            }
        }

        if let Some(return_hint) = &fun_info.return_hint {
            self.visit_type_hint(return_hint);
        }

        self.visit_block(&fun_info.body);
    }

    fn visit_type_hint(&mut self, _type_hint: &TypeHint) {}

    fn visit_block(&mut self, block: &Block) {
        for expr in &block.exprs {
            self.visit_expr(expr);
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        self.visit_expr_(&expr.1);
    }

    fn visit_expr_(&mut self, expr_: &Expression_) {
        match expr_ {
            Expression_::Match(scrutinee, cases) => {
                self.visit_expr_match(scrutinee.as_ref(), cases);
            }
            Expression_::If(cond, then_body, else_body) => {
                self.visit_expr_if(cond, then_body, else_body.as_ref());
            }
            Expression_::While(cond, body) => {
                self.visit_expr_while(cond.as_ref(), body);
            }
            Expression_::Break => {}
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
            Expression_::DotAccess(recv, _) => {
                // TODO: custom method for this variant
                self.visit_expr(recv);
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
