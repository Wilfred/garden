use std::{collections::HashMap, path::Path};

use garden_lang_parser::{
    ast::{Symbol, SyntaxId},
    parse_toplevel_items,
    position::Position,
    visitor::Visitor,
};

use crate::{
    checks::type_checker::check_types, env::Env, eval::eval_toplevel_defs, pos_to_id::find_item_at,
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

    let mut visitor = RenameLocalVisitor {
        definition_pos: def_pos.clone(),
        id_to_pos: id_to_pos.clone(),
        replace_positions: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(&item);
    }

    print!("{:?}", visitor.replace_positions);
}

struct RenameLocalVisitor {
    definition_pos: Position,
    id_to_pos: HashMap<SyntaxId, Position>,
    replace_positions: Vec<Position>,
}

impl Visitor for RenameLocalVisitor {
    fn visit_symbol(&mut self, symbol: &Symbol) {
        let Some(sym_def_pos) = self.id_to_pos.get(&symbol.id) else {
            return;
        };
        if *sym_def_pos != self.definition_pos {
            return;
        }

        self.replace_positions.push(symbol.position.clone());
    }
}
