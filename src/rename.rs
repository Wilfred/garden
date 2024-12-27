use std::{collections::HashMap, path::Path};

use garden_lang_parser::{
    ast::{AstId, IdGenerator, Symbol, SyntaxId},
    parse_toplevel_items,
    position::Position,
    visitor::Visitor,
};

use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

/// Rename the symbol at `offset` to `new_name`, both the definition
/// and use sites, then print the new source code.
pub(crate) fn rename(src: &str, path: &Path, offset: usize, new_name: &str) {
    let mut id_gen = IdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(&mut id_gen);
    load_toplevel_items(&items, &mut env);
    let summary = check_types(&items, &env);

    let ids_at_pos = find_item_at(&items, offset, offset);

    let Some(AstId::Sym(id)) = ids_at_pos.last() else {
        eprintln!("No symbol found at offset {}", offset);
        return;
    };
    let Some(def_pos) = summary.id_to_def_pos.get(id) else {
        eprintln!("No definition found for id {:?}", id);
        return;
    };

    let mut visitor = RenameLocalVisitor {
        definition_pos: def_pos.clone(),
        id_to_pos: summary.id_to_def_pos.clone(),
        replace_positions: vec![],
    };

    for item in items {
        visitor.visit_toplevel_item(&item);
    }

    let new_src = apply_renames(src, new_name, &visitor.replace_positions);
    print!("{}", new_src);
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

fn apply_renames(src: &str, new_name: &str, positions: &[Position]) -> String {
    let mut positions = positions.to_vec();
    positions.sort_unstable_by_key(|pos| pos.start_offset);

    let mut new_src = String::with_capacity(src.len());
    let mut i = 0;
    for position in positions {
        new_src.push_str(&src[i..position.start_offset]);
        new_src.push_str(new_name);
        i = position.end_offset;
    }
    new_src.push_str(&src[i..]);

    new_src
}
