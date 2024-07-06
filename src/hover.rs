use std::path::Path;

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
    pos_to_id::find_item_at,
};
use garden_lang_parser::{ast::ToplevelItem, parse_toplevel_items};

pub fn show_type(src: &str, path: &Path, offset: usize) {
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
    let (_, id_to_ty, id_to_doc_comment, _) = check_types(&items, &env);

    let hovered_ids = find_item_at(&items, offset);

    for id in hovered_ids.iter().rev() {
        if let Some(doc_comment) = id_to_doc_comment.get(id) {
            println!("{}", doc_comment);
            break;
        }
    }

    for id in hovered_ids.iter().rev() {
        if let Some(ty) = id_to_ty.get(id) {
            println!("{}", ty);
            break;
        }
    }
}
