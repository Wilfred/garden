use std::path::Path;

use garden_lang_parser::{ast::ToplevelItem, parse_toplevel_items};

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
    pos_to_id::{find_item_at, find_receiver_of_id},
};

pub(crate) fn complete(src: &str, path: &Path, offset: usize) {
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

    let ids_at_pos = find_item_at(&items, offset);

    if let Some(id) = ids_at_pos.last() {
        if let Some(recv_id) = find_receiver_of_id(&items, *id) {
            dbg!(recv_id);
        }
    }

    let (_, _, _, _) = check_types(&items, &env);

    todo!()
}
