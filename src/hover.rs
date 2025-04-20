use std::path::Path;

use crate::parser::{
    ast::{IdGenerator, Vfs},
    parse_toplevel_items,
};
use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

pub fn show_type(src: &str, path: &Path, offset: usize) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_current_namespace(path);
    load_toplevel_items(&items, &mut env, ns);

    let summary = check_types(path, &items, &env);

    let hovered_ids = find_item_at(&items, offset, offset);

    for id in hovered_ids.iter().rev() {
        if let Some(doc_comment) = summary.id_to_doc_comment.get(&id.id()) {
            println!("{}", doc_comment);
            break;
        }
    }

    for id in hovered_ids.iter().rev() {
        if let Some(ty) = summary.id_to_ty.get(&id.id()) {
            if !ty.is_error() {
                println!("{}", ty);
            }
            break;
        }
    }
}
