use std::collections::HashMap;

use garden_lang_parser::ast::{Block, Expression, Expression_, FunInfo, ToplevelItem};

use crate::diagnostics::Warning;
use crate::runtime_type::RuntimeType;
use crate::visitor::Visitor;

pub(crate) fn check_types(items: &[ToplevelItem]) -> Vec<Warning> {
    let mut visitor = TypeCheckVisitor {};
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    vec![]
}

#[derive(Debug, Default, Clone)]
struct TypeCheckVisitor {}

impl Visitor for TypeCheckVisitor {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        // Skip typechecking builtins and prelude to help print debugging.
        if let Some(n) = &fun_info.name {
            let path = n.position.path.clone();
            if path.display().to_string().ends_with("prelude.gdn") {
                return;
            }
            if path.display().to_string().ends_with("builtins.gdn") {
                return;
            }
        }

        self.visit_fun_info_default(fun_info);
    }

    fn visit_block(&mut self, block: &Block) {
        assign_expr_ids(block);

        let mut inferred: HashMap<usize, RuntimeType> = HashMap::new();

        // check_block recurses, so don't recurse in the visitor
        check_block(block, &mut inferred);
    }
}

fn assign_expr_ids(block: &Block) {
    let mut visitor = AssignExprIds::default();
    visitor.visit_block(block);
}

#[derive(Debug, Default, Clone)]
struct AssignExprIds {
    next_id: usize,
}

impl Visitor for AssignExprIds {
    fn visit_expr(&mut self, expr: &Expression) {
        expr.2
            .set(self.next_id)
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.1)
    }
}

fn check_block(block: &Block, inferred: &mut HashMap<usize, RuntimeType>) {
    for expr in &block.exprs {
        check_expr(expr, inferred);
    }
}

fn check_expr(expr: &Expression, inferred: &mut HashMap<usize, RuntimeType>) {
    match &expr.1 {
        Expression_::Match(_, _) => {}
        Expression_::If(_, _, _) => {}
        Expression_::While(_, _) => {}
        Expression_::Assign(_sym, expr) => {
            check_expr(expr, inferred);
        }
        Expression_::Let(_sym, expr) => {
            check_expr(expr, inferred);
        }
        Expression_::Return(_) => {}
        Expression_::IntLiteral(_) => {}
        Expression_::StringLiteral(_) => {}
        Expression_::ListLiteral(_list) => {}
        Expression_::StructLiteral(_, _) => {}
        Expression_::BinaryOperator(_, _, _) => {}
        Expression_::Variable(_) => {}
        Expression_::Call(_, _) => {}
        Expression_::MethodCall(_, _, _) => {}
        Expression_::DotAccess(_, _) => {}
        Expression_::FunLiteral(_) => {}
        Expression_::Block(block) => check_block(block, inferred),
    }
}
