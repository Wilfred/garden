use std::collections::HashMap;

use garden_lang_parser::ast::{Block, Expression, Expression_, FunInfo, ToplevelItem};

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::Bindings;
use crate::runtime_type::RuntimeType;
use crate::visitor::Visitor;

pub(crate) fn check_types(items: &[ToplevelItem], env: &mut Env) -> Vec<Warning> {
    let mut visitor = TypeCheckVisitor {
        env,
        inferred: HashMap::new(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    vec![]
}

#[derive(Debug)]
struct TypeCheckVisitor<'a> {
    env: &'a mut Env,
    inferred: HashMap<usize, RuntimeType>,
}

impl Visitor for TypeCheckVisitor<'_> {
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

        let mut bindings = Bindings::default();

        // check_block recurses, so don't recurse in the visitor
        check_block(block, self.env, &mut self.inferred, &mut bindings);
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

fn check_block(
    block: &Block,
    env: &mut Env,
    inferred: &mut HashMap<usize, RuntimeType>,
    bindings: &mut Bindings,
) {
    bindings.push_block();

    for expr in &block.exprs {
        check_expr(expr, env, inferred, bindings);
    }

    bindings.pop_block();
}

fn check_expr(
    expr: &Expression,
    env: &mut Env,
    inferred: &mut HashMap<usize, RuntimeType>,
    bindings: &mut Bindings,
) -> Option<RuntimeType> {
    match &expr.1 {
        Expression_::Match(_, _) => None,
        Expression_::If(_, _, _) => None,
        Expression_::While(_, _) => None,
        Expression_::Assign(_sym, expr) => check_expr(expr, env, inferred, bindings),
        Expression_::Let(_sym, expr) => check_expr(expr, env, inferred, bindings),
        Expression_::Return(_) => None,
        Expression_::IntLiteral(_) => Some(RuntimeType::Int),
        Expression_::StringLiteral(_) => Some(RuntimeType::String),
        Expression_::ListLiteral(_list) => None,
        Expression_::StructLiteral(_, _) => None,
        Expression_::BinaryOperator(_, _, _) => None,
        Expression_::Variable(_) => None,
        Expression_::Call(_, _) => None,
        Expression_::MethodCall(recv, name, args) => {
            let recv_ty = check_expr(recv, env, inferred, bindings);

            None
        }
        Expression_::DotAccess(_, _) => None,
        Expression_::FunLiteral(_) => None,
        Expression_::Block(block) => {
            check_block(block, env, inferred, bindings);
            None
        }
    }
}
