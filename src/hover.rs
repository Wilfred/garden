use std::path::Path;

use garden_lang_parser::{
    ast::{Expression, Expression_, ToplevelItem},
    parse_toplevel_items,
};

pub fn show_type(src: &str, path: &Path, line: usize, column: usize) {
    let lines: Vec<_> = src.lines().collect();
    println!("{}", &lines[line - 1]);

    match parse_toplevel_items(path, src) {
        Ok(items) => find_item_at(&items, line, column),
        Err(_) => eprintln!("Parse error."),
    }
}

fn find_item_at(items: &[ToplevelItem], line: usize, column: usize) {
    for item in items {
        let pos = match item {
            ToplevelItem::Def(d) => &d.1,
            ToplevelItem::Expr(e) => &e.0.pos,
        };

        if !pos.contains(line, column) {
            continue;
        }

        dbg!(pos);
        match item {
            ToplevelItem::Def(d) => {
                dbg!(d);
            }
            ToplevelItem::Expr(e) => {
                dbg!(find_expr_at(&e.0, line, column));
            }
        }
    }
}

fn find_expr_at(expr: &Expression, line: usize, column: usize) -> Option<Expression> {
    // Check `expr` includes this position.
    //
    // If so, see if any children incude it, and include the smallest matching expr.
    let pos = &expr.pos;
    if !pos.contains(line, column) {
        return None;
    }

    // If there's a inner expression that includes this position, return that.
    match &expr.expr_ {
        Expression_::Match(scrutinee_expr, cases) => {
            if let Some(e) = find_expr_at(scrutinee_expr, line, column) {
                return Some(e);
            }
            for (_, case_expr) in cases {
                if let Some(e) = find_expr_at(case_expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            if let Some(e) = find_expr_at(cond_expr, line, column) {
                return Some(e);
            }
            for expr in &then_block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
            if let Some(else_block) = else_block {
                for expr in &else_block.exprs {
                    if let Some(e) = find_expr_at(expr, line, column) {
                        return Some(e);
                    }
                }
            }
        }
        Expression_::While(cond_expr, block) => {
            if let Some(e) = find_expr_at(cond_expr, line, column) {
                return Some(e);
            }
            for expr in &block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::Assign(_var, expr) => {
            // TODO: support hover on the variable name in let expressions.
            if let Some(e) = find_expr_at(expr, line, column) {
                return Some(e);
            }
        }
        Expression_::Let(_var, _, expr) => {
            // TODO: support hover on the variable name in let expressions.
            if let Some(e) = find_expr_at(expr, line, column) {
                return Some(e);
            }
        }
        Expression_::Return(value) => {
            if let Some(value) = value {
                if let Some(e) = find_expr_at(value, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::ListLiteral(items) => {
            for item in items {
                if let Some(e) = find_expr_at(item, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::StructLiteral(_, fields) => {
            for (_, field_expr) in fields {
                if let Some(e) = find_expr_at(field_expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::BinaryOperator(lhs, _, rhs) => {
            if let Some(e) = find_expr_at(lhs, line, column) {
                return Some(e);
            }
            if let Some(e) = find_expr_at(rhs, line, column) {
                return Some(e);
            }
        }
        Expression_::Call(recv, args) | Expression_::MethodCall(recv, _, args) => {
            if let Some(e) = find_expr_at(recv, line, column) {
                return Some(e);
            }
            for arg in &args.arguments {
                if let Some(e) = find_expr_at(arg, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::FunLiteral(fun_info) => {
            // TODO: support hover types on parameters too.
            for expr in &fun_info.body.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::Block(block) => {
            for expr in &block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        // These expression cases have no inner expression.
        Expression_::IntLiteral(_)
        | Expression_::StringLiteral(_)
        | Expression_::Variable(_)
        | Expression_::DotAccess(_, _)
        | Expression_::Break => {}
    };

    Some(expr.clone())
}
