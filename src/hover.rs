use std::path::Path;

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
};
use garden_lang_parser::{
    ast::{Definition_, Expression, Expression_, FunInfo, Symbol, SyntaxId, ToplevelItem},
    parse_toplevel_items,
};

pub fn show_type(src: &str, path: &Path, offset: usize) {
    match parse_toplevel_items(path, src) {
        Ok(items) => {
            let mut env = Env::default();

            let mut definitions = vec![];
            for item in &items {
                if let ToplevelItem::Def(def) = item {
                    definitions.push(def.clone());
                }
            }

            eval_defs(&definitions, &mut env);

            assign_toplevel_item_ids(&items);
            let (_, id_to_ty, id_to_doc_comment) = check_types(&items, &env);

            let hovered_ids = find_item_at(&items, offset);

            for id in hovered_ids.iter().rev() {
                if let Some(doc_comment) = id_to_doc_comment.get(id) {
                    println!("{}", doc_comment);
                    break;
                }
            }

            for id in hovered_ids.iter().rev() {
                if let Some(ty) = id_to_ty.get(id) {
                    println!("{}", ty);
                    break;
                }
            }
        }
        Err(_) => eprintln!("Parse error."),
    }
}

fn find_item_at(items: &[ToplevelItem], offset: usize) -> Vec<SyntaxId> {
    let mut ids = vec![];

    'found: for item in items {
        let pos = match item {
            ToplevelItem::Def(d) => &d.1,
            ToplevelItem::Expr(e) => &e.0.pos,
        };

        if !pos.contains_offset(offset) {
            continue;
        }

        match item {
            ToplevelItem::Def(d) => match &d.2 {
                Definition_::Fun(_, fun_info) => {
                    ids.extend(find_ids_fun_info(fun_info, offset));
                }
                Definition_::Method(meth_info) => {
                    if let Some(fun_info) = meth_info.fun_info() {
                        ids.extend(find_ids_fun_info(&fun_info, offset))
                    }
                }
                Definition_::Test(test_info) => {
                    ids.extend(find_ids_exprs(&test_info.body.exprs, offset));
                }
                Definition_::Enum(_) | Definition_::Struct(_) => {}
            },
            ToplevelItem::Expr(toplevel_expr) => {
                ids.extend(find_ids_expr(&toplevel_expr.0, offset));
                if !ids.is_empty() {
                    break 'found;
                }
            }
        }
    }

    ids
}

fn find_ids_fun_info(fun_info: &FunInfo, offset: usize) -> Vec<SyntaxId> {
    let mut ids = vec![];

    ids.extend(find_ids_exprs(&fun_info.body.exprs, offset));

    ids
}

fn find_ids_exprs(exprs: &[Expression], offset: usize) -> Vec<SyntaxId> {
    let mut ids = vec![];

    for expr in exprs {
        ids.extend(find_ids_expr(expr, offset));
        if !ids.is_empty() {
            break;
        }
    }

    ids
}

fn find_id_symbol(symbol: &Symbol, offset: usize) -> Option<SyntaxId> {
    if symbol.position.contains_offset(offset) {
        Some(*symbol.id.get().expect("Symbol ID should be set"))
    } else {
        None
    }
}

fn find_ids_expr(expr: &Expression, offset: usize) -> Vec<SyntaxId> {
    // Check `expr` includes this position.
    //
    // If so, see if any children incude it, and include the innermost matching expr.
    let pos = &expr.pos;
    if !pos.contains_offset(offset) {
        return vec![];
    }

    let mut expr_ids = vec![*expr.id.get().expect("ID should be set")];

    // If there's a inner expression that includes this position, return that.
    match &expr.expr_ {
        Expression_::Match(scrutinee_expr, cases) => {
            expr_ids.extend(find_ids_expr(scrutinee_expr, offset));

            for (_, case_expr) in cases {
                expr_ids.extend(find_ids_expr(case_expr, offset));
            }
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            expr_ids.extend(find_ids_expr(cond_expr, offset));

            expr_ids.extend(find_ids_exprs(&then_block.exprs, offset));
            if let Some(else_block) = else_block {
                expr_ids.extend(find_ids_exprs(&else_block.exprs, offset));
            }
        }
        Expression_::While(cond_expr, block) => {
            expr_ids.extend(find_ids_expr(cond_expr, offset));

            expr_ids.extend(find_ids_exprs(&block.exprs, offset));
        }
        Expression_::Assign(symbol, expr) => {
            if let Some(id) = find_id_symbol(symbol, offset) {
                expr_ids.push(id);
            }

            expr_ids.extend(find_ids_expr(expr, offset));
        }
        Expression_::Let(symbol, _, expr) => {
            if let Some(id) = find_id_symbol(symbol, offset) {
                expr_ids.push(id);
            }

            // TODO: support hover on the variable name in let expressions.
            expr_ids.extend(find_ids_expr(expr, offset));
        }
        Expression_::Return(value) => {
            if let Some(value) = value {
                expr_ids.extend(find_ids_expr(value, offset));
            }
        }
        Expression_::ListLiteral(items) => {
            expr_ids.extend(find_ids_exprs(items, offset));
        }
        Expression_::StructLiteral(_, fields) => {
            for (_, field_expr) in fields {
                expr_ids.extend(find_ids_expr(field_expr, offset));
            }
        }
        Expression_::BinaryOperator(lhs, _, rhs) => {
            expr_ids.extend(find_ids_expr(lhs, offset));
            expr_ids.extend(find_ids_expr(rhs, offset));
        }
        Expression_::Call(recv, args) | Expression_::MethodCall(recv, _, args) => {
            expr_ids.extend(find_ids_expr(recv, offset));

            expr_ids.extend(find_ids_exprs(&args.arguments, offset));
        }
        Expression_::FunLiteral(fun_info) => {
            // TODO: support hover types on parameters too.
            expr_ids.extend(find_ids_fun_info(fun_info, offset));
        }
        Expression_::Block(block) => {
            expr_ids.extend(find_ids_exprs(&block.exprs, offset));
        }
        Expression_::Variable(symbol) => {
            if let Some(id) = find_id_symbol(symbol, offset) {
                expr_ids.push(id);
            }
        }
        // These expression cases have no inner expression.
        Expression_::IntLiteral(_)
        | Expression_::StringLiteral(_)
        | Expression_::DotAccess(_, _)
        | Expression_::Break => {}
    };

    expr_ids
}
