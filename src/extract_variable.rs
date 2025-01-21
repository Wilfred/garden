use std::path::Path;

use garden_lang_parser::{
    ast::{AstId, Expression, Expression_, IdGenerator},
    parse_toplevel_items,
};

use crate::{
    env::Env,
    eval::load_toplevel_items,
    pos_to_id::{find_expr_of_id, find_item_at},
};

pub(crate) fn extract_variable(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
    name: &str,
) {
    let mut id_gen = IdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(id_gen);
    load_toplevel_items(&items, &mut env);

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    let mut expr_id = None;
    for id in ids_at_pos.iter().rev() {
        if let AstId::Expr(syntax_id) = id {
            expr_id = Some(syntax_id);
            break;
        }
    }

    // The innermost block-level expression containing the target expression.
    let mut enclosing_block_level_expr: Option<Expression> = None;
    for id in ids_at_pos.iter().rev() {
        let AstId::Expr(expr_syntax_id) = id else {
            continue;
        };
        let Some(expr) = find_expr_of_id(&items, *expr_syntax_id) else {
            continue;
        };

        match &expr.expr_ {
            Expression_::FunLiteral(_) | Expression_::Block(_) => break,
            _ => {
                enclosing_block_level_expr = Some(expr.clone());
            }
        }
    }

    let Some(expr_id) = expr_id else {
        eprintln!("No expression found at this selected position.");
        return;
    };
    let Some(expr) = find_expr_of_id(&items, *expr_id) else {
        eprintln!("No expression found for the ID at the selected position.");
        return;
    };
    let Some(enclosing_block_level_expr) = enclosing_block_level_expr else {
        eprintln!("No enclosing block-level expression found for the selected position.");
        return;
    };

    for item in items {
        let item_pos = &item.1;
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            print!("{}", &src[..item_pos.start_offset]);

            print!(
                "{}",
                &src[item_pos.start_offset..enclosing_block_level_expr.position.start_offset]
            );
            print!(
                "let {} = {};\n{}",
                name,
                &src[expr.position.start_offset..expr.position.end_offset],
                " ".repeat(enclosing_block_level_expr.position.column)
            );

            print!(
                "{}",
                &src[enclosing_block_level_expr.position.start_offset..expr.position.start_offset]
            );
            print!("{}", name);
            print!("{}", &src[expr.position.end_offset..item_pos.end_offset]);

            // Items after.
            print!("{}", &src[item_pos.end_offset..]);

            break;
        }
    }
}
