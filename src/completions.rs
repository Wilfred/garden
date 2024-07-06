use std::path::Path;

use garden_lang_parser::{ast::ToplevelItem, parse_toplevel_items};

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
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
    let (_, id_to_ty, id_to_doc_comment, _) = check_types(&items, &env);

    todo!()
}
