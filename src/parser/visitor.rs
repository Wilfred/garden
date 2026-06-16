use std::rc::Rc;

use crate::parser::ast::ToplevelExpression;
use crate::parser::{
    Block, EnumInfo, Expression, Expression_, FunInfo, ImportInfo, LetDestination, MethodInfo,
    MethodKind, Pattern, StructInfo, Symbol, TestInfo, ToplevelItem, TypeHint, TypeSymbol,
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
        self.visit_symbol(&method_info.name_sym);

        if let Some(fun_info) = method_info.fun_info() {
            self.visit_fun_info(fun_info);
        }
    }

    fn visit_test_info(&mut self, test_info: &TestInfo) {
        self.visit_symbol(&test_info.name_sym);
        self.visit_block(&test_info.body);
    }

    fn visit_enum_info(&mut self, enum_info: &EnumInfo) {
        self.visit_enum_info_default(enum_info);
    }

    fn visit_enum_info_default(&mut self, enum_info: &EnumInfo) {
        self.visit_type_symbol(&enum_info.name_sym);
        for type_param in &enum_info.type_params {
            self.visit_type_symbol(type_param);
        }
        for variant in &enum_info.variants {
            self.visit_symbol(&variant.name_sym);
            if let Some(hint) = &variant.payload_hint {
                self.visit_type_hint(hint);
            }
        }
    }

    fn visit_import_info(&mut self, import_info: &ImportInfo) {
        self.visit_import_info_default(import_info);
    }

    fn visit_import_info_default(&mut self, import_info: &ImportInfo) {
        if let Some(namespace_sym) = &import_info.namespace_sym {
            self.visit_symbol(namespace_sym);
        }
    }

    fn visit_struct_info(&mut self, struct_info: &StructInfo) {
        self.visit_struct_info_default(struct_info);
    }

    fn visit_struct_info_default(&mut self, struct_info: &StructInfo) {
        self.visit_type_symbol(&struct_info.name_sym);
        for type_param in &struct_info.type_params {
            self.visit_type_symbol(type_param);
        }
        for field in &struct_info.fields {
            self.visit_symbol(&field.sym);
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
        if let Some(name_sym) = &fun_info.name_sym {
            self.visit_symbol(name_sym);
        }

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
            Expression_::Try(try_body, catch_sym, catch_body) => {
                self.visit_expr_try(try_body, catch_sym, catch_body);
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
                for pair in pairs {
                    self.visit_expr(&pair.key);
                    self.visit_expr(&pair.value);
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
            Expression_::FloatLiteral(_) => {
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
            Expression_::Parentheses(paren) => {
                self.visit_expr(&paren.expr);
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

    fn visit_expr_try(&mut self, try_body: &Block, catch_sym: &Symbol, catch_body: &Block) {
        self.visit_block(try_body);
        self.visit_symbol(catch_sym);
        self.visit_block(catch_body);
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

/// A mutable visitor for ASTs.
///
/// This mirrors `Visitor`, but walks mutable AST nodes. It is useful
/// for focused AST rewrites that still want the central traversal
/// logic to live in one place.
pub(crate) trait VisitorMut {
    fn visit_toplevel_item_mut(&mut self, item: &mut ToplevelItem) {
        self.visit_toplevel_item_mut_default(item);
    }

    fn visit_toplevel_item_mut_default(&mut self, item: &mut ToplevelItem) {
        match item {
            ToplevelItem::Fun(_, fun_info, _) => self.visit_fun_info_mut(fun_info),
            ToplevelItem::Method(method_info, _) => self.visit_method_info_mut(method_info),
            ToplevelItem::Test(test_info) => self.visit_test_info_mut(test_info),
            ToplevelItem::Enum(enum_info) => self.visit_enum_info_mut(enum_info),
            ToplevelItem::Struct(struct_info) => self.visit_struct_info_mut(struct_info),
            ToplevelItem::Expr(toplevel_expression) => {
                self.visit_toplevel_expr_mut(toplevel_expression)
            }
            ToplevelItem::Block(block) => self.visit_block_mut(block),
            ToplevelItem::Import(import_info) => self.visit_import_info_mut(import_info),
        }
    }

    fn visit_toplevel_expr_mut(&mut self, toplevel_expr: &mut ToplevelExpression) {
        self.visit_expr_mut(&mut toplevel_expr.0);
    }

    fn visit_method_info_mut(&mut self, method_info: &mut MethodInfo) {
        self.visit_method_info_mut_default(method_info);
    }

    fn visit_method_info_mut_default(&mut self, method_info: &mut MethodInfo) {
        self.visit_type_hint_mut(&mut method_info.receiver_hint);
        self.visit_symbol_mut(&mut method_info.receiver_sym);
        self.visit_symbol_mut(&mut method_info.name_sym);

        match &mut method_info.kind {
            MethodKind::BuiltinMethod(_, Some(fun_info))
            | MethodKind::UserDefinedMethod(fun_info) => self.visit_fun_info_mut(fun_info),
            MethodKind::BuiltinMethod(_, None) => {}
        }
    }

    fn visit_test_info_mut(&mut self, test_info: &mut TestInfo) {
        self.visit_symbol_mut(&mut test_info.name_sym);
        self.visit_block_mut(&mut test_info.body);
    }

    fn visit_enum_info_mut(&mut self, enum_info: &mut EnumInfo) {
        self.visit_enum_info_mut_default(enum_info);
    }

    fn visit_enum_info_mut_default(&mut self, enum_info: &mut EnumInfo) {
        self.visit_type_symbol_mut(&mut enum_info.name_sym);
        for type_param in &mut enum_info.type_params {
            self.visit_type_symbol_mut(type_param);
        }
        for variant in &mut enum_info.variants {
            self.visit_symbol_mut(&mut variant.name_sym);
            if let Some(hint) = &mut variant.payload_hint {
                self.visit_type_hint_mut(hint);
            }
        }
    }

    fn visit_import_info_mut(&mut self, import_info: &mut ImportInfo) {
        self.visit_import_info_mut_default(import_info);
    }

    fn visit_import_info_mut_default(&mut self, import_info: &mut ImportInfo) {
        if let Some(namespace_sym) = &mut import_info.namespace_sym {
            self.visit_symbol_mut(namespace_sym);
        }
    }

    fn visit_struct_info_mut(&mut self, struct_info: &mut StructInfo) {
        self.visit_struct_info_mut_default(struct_info);
    }

    fn visit_struct_info_mut_default(&mut self, struct_info: &mut StructInfo) {
        self.visit_type_symbol_mut(&mut struct_info.name_sym);
        for type_param in &mut struct_info.type_params {
            self.visit_type_symbol_mut(type_param);
        }
        for field in &mut struct_info.fields {
            self.visit_symbol_mut(&mut field.sym);
            self.visit_type_hint_mut(&mut field.hint);
        }
    }

    fn visit_fun_info_mut(&mut self, fun_info: &mut FunInfo) {
        self.visit_fun_info_mut_default(fun_info);
    }

    fn visit_fun_info_mut_default(&mut self, fun_info: &mut FunInfo) {
        if let Some(name_sym) = &mut fun_info.name_sym {
            self.visit_symbol_mut(name_sym);
        }

        for type_param in &mut fun_info.type_params {
            self.visit_type_symbol_mut(type_param);
        }

        for param in &mut fun_info.params.params {
            self.visit_symbol_mut(&mut param.symbol);

            if let Some(param_hint) = &mut param.hint {
                self.visit_type_hint_mut(param_hint);
            }
        }

        if let Some(return_hint) = &mut fun_info.return_hint {
            self.visit_type_hint_mut(return_hint);
        }

        self.visit_block_mut(&mut fun_info.body);
    }

    fn visit_type_hint_mut(&mut self, type_hint: &mut TypeHint) {
        self.visit_type_symbol_mut(&mut type_hint.sym);
        for arg in &mut type_hint.args {
            self.visit_type_hint_mut(arg);
        }
    }

    fn visit_block_mut(&mut self, block: &mut Block) {
        for expr in &mut block.exprs {
            self.visit_expr_mut(Rc::make_mut(expr));
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expression) {
        self.visit_expr_mut_default(expr);
    }

    fn visit_expr_mut_default(&mut self, expr: &mut Expression) {
        self.visit_expr_variant_mut(&mut expr.expr_);
    }

    fn visit_expr_variant_mut(&mut self, expr_: &mut Expression_) {
        match expr_ {
            Expression_::Match(scrutinee, cases) => {
                self.visit_expr_match_mut(Rc::make_mut(scrutinee), cases);
            }
            Expression_::If(cond, then_body, else_body) => {
                self.visit_expr_if_mut(Rc::make_mut(cond), then_body, else_body.as_mut());
            }
            Expression_::While(cond, body) => {
                self.visit_expr_while_mut(Rc::make_mut(cond), body);
            }
            Expression_::ForIn(sym, expr, body) => {
                self.visit_expr_for_in_mut(sym, Rc::make_mut(expr), body);
            }
            Expression_::Try(try_body, catch_sym, catch_body) => {
                self.visit_expr_try_mut(try_body, catch_sym, catch_body);
            }
            Expression_::Break => {}
            Expression_::Continue => {}
            Expression_::Assign(sym, expr) => {
                self.visit_expr_assign_mut(sym, Rc::make_mut(expr));
            }
            Expression_::AssignUpdate(sym, _, expr) => {
                self.visit_expr_assign_update_mut(sym, Rc::make_mut(expr));
            }
            Expression_::Let(dest, hint, expr) => {
                self.visit_expr_let_mut(dest, hint.as_mut(), Rc::make_mut(expr));
            }
            Expression_::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr_mut(Rc::make_mut(expr));
                }
            }
            Expression_::ListLiteral(exprs) => {
                for expr in exprs {
                    self.visit_expr_mut(Rc::make_mut(&mut expr.expr));
                }
            }
            Expression_::DictLiteral(pairs) => {
                for pair in pairs {
                    self.visit_expr_mut(Rc::make_mut(&mut pair.key));
                    self.visit_expr_mut(Rc::make_mut(&mut pair.value));
                }
            }
            Expression_::TupleLiteral(exprs) => {
                for expr in exprs {
                    self.visit_expr_mut(Rc::make_mut(expr));
                }
            }
            Expression_::StructLiteral(name_sym, field_exprs) => {
                self.visit_expr_struct_literal_mut(name_sym, field_exprs);
            }
            Expression_::BinaryOperator(lhs, _, rhs) => {
                self.visit_expr_mut(Rc::make_mut(lhs));
                self.visit_expr_mut(Rc::make_mut(rhs));
            }
            Expression_::Variable(var) => {
                self.visit_expr_variable_mut(var);
            }
            Expression_::Call(recv, paren_args) => {
                self.visit_expr_mut(Rc::make_mut(recv));
                for arg in &mut paren_args.arguments {
                    self.visit_expr_mut(Rc::make_mut(&mut arg.expr));
                }
            }
            Expression_::MethodCall(recv, meth_name, paren_args) => {
                self.visit_symbol_mut(meth_name);
                self.visit_expr_mut(Rc::make_mut(recv));
                for arg in &mut paren_args.arguments {
                    self.visit_expr_mut(Rc::make_mut(&mut arg.expr));
                }
            }
            Expression_::FunLiteral(fun_info) => {
                self.visit_expr_fun_literal_mut(fun_info);
            }
            Expression_::IntLiteral(_) => {}
            Expression_::FloatLiteral(_) => {}
            Expression_::StringLiteral(_) => {}
            Expression_::DotAccess(recv, field_sym) => {
                self.visit_symbol_mut(field_sym);
                self.visit_expr_mut(Rc::make_mut(recv));
            }
            Expression_::NamespaceAccess(recv, sym) => {
                self.visit_symbol_mut(sym);
                self.visit_expr_mut(Rc::make_mut(recv));
            }
            Expression_::Assert(expr) => {
                self.visit_expr_mut(Rc::make_mut(expr));
            }
            Expression_::Parentheses(paren) => {
                self.visit_expr_mut(Rc::make_mut(&mut paren.expr));
            }
            Expression_::Invalid => {}
        }
    }

    fn visit_expr_struct_literal_mut(
        &mut self,
        type_symbol: &mut TypeSymbol,
        field_exprs: &mut [(Symbol, Rc<Expression>)],
    ) {
        self.visit_type_symbol_mut(type_symbol);
        for (sym, expr) in field_exprs {
            self.visit_symbol_mut(sym);
            self.visit_expr_mut(Rc::make_mut(expr));
        }
    }

    fn visit_expr_match_mut(&mut self, scrutinee: &mut Expression, cases: &mut [(Pattern, Block)]) {
        self.visit_expr_mut(scrutinee);
        for (pattern, case_expr) in cases {
            self.visit_symbol_mut(&mut pattern.variant_sym);
            if let Some(dest) = &mut pattern.payload {
                self.visit_dest_mut(dest);
            }

            self.visit_block_mut(case_expr);
        }
    }

    fn visit_expr_if_mut(
        &mut self,
        cond: &mut Expression,
        then_body: &mut Block,
        else_body: Option<&mut Block>,
    ) {
        self.visit_expr_mut(cond);
        self.visit_block_mut(then_body);
        if let Some(else_body) = else_body {
            self.visit_block_mut(else_body);
        }
    }

    fn visit_expr_while_mut(&mut self, cond: &mut Expression, body: &mut Block) {
        self.visit_expr_mut(cond);
        self.visit_block_mut(body);
    }

    fn visit_expr_for_in_mut(
        &mut self,
        dest: &mut LetDestination,
        expr: &mut Expression,
        body: &mut Block,
    ) {
        self.visit_dest_mut(dest);
        self.visit_expr_mut(expr);
        self.visit_block_mut(body);
    }

    fn visit_expr_try_mut(
        &mut self,
        try_body: &mut Block,
        catch_sym: &mut Symbol,
        catch_body: &mut Block,
    ) {
        self.visit_block_mut(try_body);
        self.visit_symbol_mut(catch_sym);
        self.visit_block_mut(catch_body);
    }

    fn visit_expr_variable_mut(&mut self, symbol: &mut Symbol) {
        self.visit_symbol_mut(symbol);
    }

    fn visit_dest_mut(&mut self, dest: &mut LetDestination) {
        match dest {
            LetDestination::Symbol(symbol) => {
                self.visit_symbol_mut(symbol);
            }
            LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    self.visit_symbol_mut(symbol);
                }
            }
        }
    }

    fn visit_expr_let_mut(
        &mut self,
        dest: &mut LetDestination,
        hint: Option<&mut TypeHint>,
        expr: &mut Expression,
    ) {
        self.visit_expr_mut(expr);

        self.visit_dest_mut(dest);

        if let Some(hint) = hint {
            self.visit_type_hint_mut(hint);
        }
    }

    fn visit_expr_assign_mut(&mut self, symbol: &mut Symbol, expr: &mut Expression) {
        self.visit_symbol_mut(symbol);
        self.visit_expr_mut(expr);
    }

    fn visit_expr_assign_update_mut(&mut self, symbol: &mut Symbol, expr: &mut Expression) {
        self.visit_symbol_mut(symbol);
        self.visit_expr_mut(expr);
    }

    fn visit_expr_fun_literal_mut(&mut self, fun_info: &mut FunInfo) {
        self.visit_fun_info_mut(fun_info);
    }

    fn visit_symbol_mut(&mut self, _: &mut Symbol) {}

    fn visit_type_symbol_mut(&mut self, _: &mut TypeSymbol) {}
}
