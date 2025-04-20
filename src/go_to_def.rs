use std::path::Path;

use crate::parser::{
    ast::{IdGenerator, Vfs},
    parse_toplevel_items,
};
use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

/// Print the position of the definition associated with the
/// expression at `offset`.
pub(crate) fn print_pos(src: &str, path: &Path, offset: usize) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_current_namespace(path);
    load_toplevel_items(&items, &mut env, ns);

    let summary = check_types(path, &items, &env);

    let ids_at_query_pos = find_item_at(&items, offset, offset);

    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = summary.id_to_def_pos.get(&id.id()) {
            println!("{}", serde_json::to_string(pos).unwrap());
            return;
        }
    }

    println!("null");
}
