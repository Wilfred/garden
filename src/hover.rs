use std::path::Path;

use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};
use garden_lang_parser::{ast::SyntaxIdGenerator, parse_toplevel_items};

pub fn show_type(src: &str, path: &Path, offset: usize) {
    let mut id_gen = SyntaxIdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(&mut id_gen);
    load_toplevel_items(&items, &mut env);

    let (_, id_to_ty, id_to_doc_comment, _) = check_types(&items, &env);

    let hovered_ids = find_item_at(&items, offset, offset);

    for id in hovered_ids.iter().rev() {
        if let Some(doc_comment) = id_to_doc_comment.get(&id.id()) {
            println!("{}", doc_comment);
            break;
        }
    }

    for id in hovered_ids.iter().rev() {
        if let Some(ty) = id_to_ty.get(&id.id()) {
            if !ty.is_error() {
                println!("{}", ty);
            }
            break;
        }
    }
}
