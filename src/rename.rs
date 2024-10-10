use std::path::Path;

use garden_lang_parser::{ast::Expression_, parse_toplevel_items, visitor::Visitor};

use crate::{
    checks::type_checker::check_types,
    env::Env,
    eval::eval_toplevel_defs,
    pos_to_id::{find_expr_of_id, find_item_at},
};

pub(crate) fn rename(src: &str, path: &Path, offset: usize, new_name: &str) {
    let mut env = Env::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut env.id_gen);

    eval_toplevel_defs(&items, &mut env);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    let ids_at_pos = find_item_at(&items, offset);

    let Some(id) = ids_at_pos.last() else {
        eprintln!("No item found at offset {}", offset);
        return;
    };
    let Some(def_pos) = id_to_pos.get(id) else {
        eprintln!("No definition found for id {:?}", id);
        return;
    };

    let Some(expr) = find_expr_of_id(&items, *id) else {
        eprintln!("No expression found for id {:?}", id);
        return;
    };

    match &expr.expr_ {
        Expression_::Variable(symbol) => todo!(),
        _ => {}
    }

    print!("{}", src);
}

struct RenameLocalVisitor {}

impl Visitor for RenameLocalVisitor {}
