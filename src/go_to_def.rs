use std::path::Path;

use garden_lang_parser::{
    ast::{SyntaxId, ToplevelItem},
    parse_toplevel_items,
};

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
    pos_to_id::find_item_at,
};

/// Print the position of the definition associated with the
/// expression at `offset`.
pub(crate) fn print_pos(src: &str, path: &Path, offset: usize) {
    let mut next_id2 = SyntaxId(0);
    let (items, _errors) = parse_toplevel_items(path, src, &mut next_id2);

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
            println!("{}", serde_json::to_string(pos).unwrap());
            return;
        }
    }

    println!("null");
}
