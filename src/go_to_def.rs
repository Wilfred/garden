use std::path::Path;

use garden_lang_parser::{ast::ToplevelItem, parse_toplevel_items};

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
    pos_to_id::find_item_at,
};

pub(crate) fn print_pos(src: &str, path: &Path, offset: usize) {
    let (items, _errors) = parse_toplevel_items(path, src);

    let mut env = Env::default();

    let mut definitions = vec![];
    for item in &items {
        if let ToplevelItem::Def(def) = item {
            definitions.push(def.clone());
        }
    }

    eval_defs(&definitions, &mut env);

    assign_toplevel_item_ids(&items);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    let ids_at_query_pos = find_item_at(&items, offset);

    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = id_to_pos.get(id) {
            println!("{:?}", pos);
            return;
        }
    }

    println!("(no definition found)");
}
