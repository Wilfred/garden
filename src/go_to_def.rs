use std::path::Path;

use garden_lang_parser::{ast::SyntaxIdGenerator, parse_toplevel_items};

use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

/// Print the position of the definition associated with the
/// expression at `offset`.
pub(crate) fn print_pos(src: &str, path: &Path, offset: usize) {
    let mut id_gen = SyntaxIdGenerator::default();
    let mut env = Env::new(&mut id_gen);

    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    load_toplevel_items(&items, &mut env);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    let ids_at_query_pos = find_item_at(&items, offset, offset);

    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = id_to_pos.get(&id.id()) {
            println!("{}", serde_json::to_string(pos).unwrap());
            return;
        }
    }

    println!("null");
}
