use std::rc::Rc;

use crate::parser::ast::ToplevelExpression;
use crate::parser::{
    Block, EnumInfo, Expression, Expression_, FunInfo, ImportInfo, LetDestination, MethodInfo,
    Pattern, StructInfo, Symbol, TestInfo, ToplevelItem, TypeHint, TypeSymbol,
};

/// A visitor for ASTs.
///
/// Unlike pattern matching, a visitor makes it easy to recurse but
/// hook in to specific variants. For example, looking for
/// occurrences of string literals anywhere in a function body.
pub(crate) trait Visitor {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        self.visit_toplevel_item_default(item);
    }

    fn visit_toplevel_item_default(&mut self, item: &ToplevelItem) {
        match item {
            ToplevelItem::Fun(_, fun_info, _) => self.visit_fun_info(fun_info),
            ToplevelItem::Method(method_info, _) => self.visit_method_info(method_info),
            ToplevelItem::Test(test_info) => self.visit_test_info(test_info),
            ToplevelItem::Enum(enum_info) => self.visit_enum_info(enum_info),
            ToplevelItem::Struct(struct_info) => self.visit_struct_info(struct_info),
            ToplevelItem::Expr(toplevel_expression) => {
                self.visit_toplevel_expr(toplevel_expression)
            }
            ToplevelItem::Block(block) => self.visit_block(block),
            ToplevelItem::Import(import_info) => self.visit_import_info(import_info),
        }
    }

    fn visit_toplevel_expr(&mut self, toplevel_expr: &ToplevelExpression) {
        self.visit_expr(&toplevel_expr.0);
    }

    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.visit_method_info_default(method_info);
    }

    fn visit_method_info_default(&mut self, method_info: &MethodInfo) {
        self.visit_type_hint(&method_info.receiver_hint);
        self.visit_symbol(&method_info.receiver_sym);

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

    fn visit_import_info(&mut self, _: &ImportInfo) {}

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

        for param in &fun_info.params.params {
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
            Expression_::Let(dest, hint, expr) => {
                self.visit_expr_let(dest, hint.as_ref(), expr);
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
                    self.visit_expr(&expr.expr);
                }
            }
            Expression_::DictLiteral(pairs) => {
                for (key_expr, _arrow_pos, value_expr) in pairs {
                    self.visit_expr(key_expr);
                    self.visit_expr(value_expr);
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
                    self.visit_expr(&arg.expr);
                }
            }
            Expression_::MethodCall(recv, meth_name, paren_args) => {
                self.visit_symbol(meth_name);
                self.visit_expr(recv);
                for arg in &paren_args.arguments {
                    self.visit_expr(&arg.expr);
                }
            }
            Expression_::FunLiteral(fun_info) => {
                self.visit_expr_fun_literal(fun_info);
            }
            Expression_::IntLiteral(_) => {
                // TODO: custom method for this variant
            }
            Expression_::StringLiteral(_) => {
                // TODO: custom method for this variant
            }
            Expression_::DotAccess(recv, field_sym) => {
                // TODO: custom method for this variant
                self.visit_symbol(field_sym);
                self.visit_expr(recv);
            }
            Expression_::NamespaceAccess(recv, sym) => {
                self.visit_symbol(sym);
                self.visit_expr(recv);
            }
            Expression_::Assert(expr) => {
                self.visit_expr(expr);
            }
            Expression_::Parentheses(_, expr, _) => {
                self.visit_expr(expr);
            }
            Expression_::Invalid => {
                // TODO: custom method for this variant
            }
        }
    }

    fn visit_expr_struct_literal(
        &mut self,
        type_symbol: &TypeSymbol,
        field_exprs: &[(Symbol, Rc<Expression>)],
    ) {
        self.visit_type_symbol(type_symbol);
        for (sym, expr) in field_exprs {
            self.visit_symbol(sym);
            self.visit_expr(expr);
        }
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(Pattern, Block)]) {
        self.visit_expr(scrutinee);
        for (pattern, case_expr) in cases {
            self.visit_symbol(&pattern.variant_sym);
            if let Some(dest) = &pattern.payload {
                self.visit_dest(dest);
            }

            self.visit_block(case_expr);
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

    fn visit_expr_for_in(&mut self, dest: &LetDestination, expr: &Expression, body: &Block) {
        self.visit_dest(dest);
        self.visit_expr(expr);
        self.visit_block(body);
    }

    fn visit_expr_variable(&mut self, symbol: &Symbol) {
        self.visit_symbol(symbol);
    }

    fn visit_dest(&mut self, dest: &LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => {
                self.visit_symbol(symbol);
            }
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.visit_symbol(symbol);
                }
            }
        }
    }

    fn visit_expr_let(
        &mut self,
        dest: &LetDestination,
        hint: Option<&TypeHint>,
        expr: &Expression,
    ) {
        // Visit the expression before the destination, so we're not
        // confused by cases like `let x = x`.
        self.visit_expr(expr);

        self.visit_dest(dest);

        if let Some(hint) = hint {
            self.visit_type_hint(hint);
        }
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
