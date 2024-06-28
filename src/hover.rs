use std::path::Path;

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
};
use garden_lang_parser::{
    ast::{Definition_, Expression, Expression_, Symbol, SyntaxId, ToplevelItem},
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
            ToplevelItem::Def(d) => {
                let exprs = match &d.2 {
                    Definition_::Fun(_, fun_info) => fun_info.body.exprs.clone(),
                    Definition_::Method(meth_info) => match meth_info.fun_info() {
                        Some(fun_info) => fun_info.body.exprs.clone(),
                        None => vec![],
                    },
                    Definition_::Test(test_info) => test_info.body.exprs.clone(),
                    Definition_::Enum(_) | Definition_::Struct(_) => vec![],
                };

                for expr in exprs {
                    ids.extend(find_expr_at(&expr, offset));
                    if !ids.is_empty() {
                        break 'found;
                    }
                }
            }
            ToplevelItem::Expr(toplevel_expr) => {
                ids.extend(find_expr_at(&toplevel_expr.0, offset));
                if !ids.is_empty() {
                    break 'found;
                }
            }
        }
    }

    ids
}

fn find_symbol_at(symbol: &Symbol, offset: usize) -> Option<SyntaxId> {
    if symbol.position.contains_offset(offset) {
        Some(*symbol.id.get().expect("Symbol ID should be set"))
    } else {
        None
    }
}

fn find_expr_at(expr: &Expression, offset: usize) -> Vec<SyntaxId> {
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
            let ids = find_expr_at(scrutinee_expr, offset);
            if !ids.is_empty() {
                return ids;
            }

            for (_, case_expr) in cases {
                let ids = find_expr_at(case_expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            let ids = find_expr_at(cond_expr, offset);
            if !ids.is_empty() {
                return ids;
            }
            for expr in &then_block.exprs {
                let ids = find_expr_at(expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
            if let Some(else_block) = else_block {
                for expr in &else_block.exprs {
                    let ids = find_expr_at(expr, offset);
                    if !ids.is_empty() {
                        return ids;
                    }
                }
            }
        }
        Expression_::While(cond_expr, block) => {
            let ids = find_expr_at(cond_expr, offset);
            if !ids.is_empty() {
                return ids;
            }
            for expr in &block.exprs {
                let ids = find_expr_at(expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::Assign(symbol, expr) => {
            if let Some(id) = find_symbol_at(symbol, offset) {
                expr_ids.push(id);
                return expr_ids;
            }

            let ids = find_expr_at(expr, offset);
            if !ids.is_empty() {
                return ids;
            }
        }
        Expression_::Let(symbol, _, expr) => {
            if let Some(id) = find_symbol_at(symbol, offset) {
                expr_ids.push(id);
                return expr_ids;
            }

            // TODO: support hover on the variable name in let expressions.
            let ids = find_expr_at(expr, offset);
            if !ids.is_empty() {
                return ids;
            }
        }
        Expression_::Return(value) => {
            if let Some(value) = value {
                let ids = find_expr_at(value, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::ListLiteral(items) => {
            for item in items {
                let ids = find_expr_at(item, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::StructLiteral(_, fields) => {
            for (_, field_expr) in fields {
                let ids = find_expr_at(field_expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::BinaryOperator(lhs, _, rhs) => {
            let ids = find_expr_at(lhs, offset);
            if !ids.is_empty() {
                return ids;
            }
            let ids = find_expr_at(rhs, offset);
            if !ids.is_empty() {
                return ids;
            }
        }
        Expression_::Call(recv, args) | Expression_::MethodCall(recv, _, args) => {
            let ids = find_expr_at(recv, offset);
            if !ids.is_empty() {
                return ids;
            }

            for arg in &args.arguments {
                let ids = find_expr_at(arg, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::FunLiteral(fun_info) => {
            // TODO: support hover types on parameters too.
            for expr in &fun_info.body.exprs {
                let ids = find_expr_at(expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::Block(block) => {
            for expr in &block.exprs {
                let ids = find_expr_at(expr, offset);
                if !ids.is_empty() {
                    return ids;
                }
            }
        }
        Expression_::Variable(symbol) => {
            if let Some(id) = find_symbol_at(symbol, offset) {
                return vec![id];
            }
        }
        // These expression cases have no inner expression.
        Expression_::IntLiteral(_)
        | Expression_::StringLiteral(_)
        | Expression_::DotAccess(_, _)
        | Expression_::Break => {}
    };

    vec![*expr.id.get().expect("ID should be set")]
}
