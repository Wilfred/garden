use std::path::Path;

use garden_lang_parser::{
    ast::{AstId, SyntaxIdGenerator},
    parse_toplevel_items,
};

use crate::{
    checks::type_checker::check_types,
    env::Env,
    eval::load_toplevel_items,
    pos_to_id::{find_expr_of_id, find_item_at},
};

pub(crate) fn extract_function(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
    name: &str,
) {
    let mut id_gen = SyntaxIdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(&mut id_gen);
    load_toplevel_items(&items, &mut env);
    let (_, id_to_ty, _, _) = check_types(&items, &env);

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    let mut expr_id = None;
    for id in ids_at_pos.iter().rev() {
        if let AstId::Expr(syntax_id) = id {
            expr_id = Some(syntax_id);
            break;
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
    let return_ty = if let Some(ty) = id_to_ty.get(expr_id) {
        format!(": {}", ty)
    } else {
        "".to_owned()
    };

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            print!("{}", &src[..item_pos.start_offset]);

            // The extracted function.
            println!(
                "fun {}(){} {{\n  {}\n}}\n",
                name,
                return_ty,
                &src[expr.pos.start_offset..expr.pos.end_offset]
            );

            // The item, with the expression replaced by a call.
            print!("{}", &src[item_pos.start_offset..expr.pos.start_offset]);
            print!("{}()", name);
            print!("{}", &src[expr.pos.end_offset..item_pos.end_offset]);

            // Items after.
            print!("{}", &src[item_pos.end_offset..]);

            break;
        }
    }
}
