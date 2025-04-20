use std::path::Path;

use crate::parser::{
    ast::{AstId, IdGenerator, Symbol, SyntaxId, Vfs},
    parse_toplevel_items,
    position::Position,
    visitor::Visitor,
};
use rustc_hash::FxHashMap;

use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

/// Rename the symbol at `offset` to `new_name`, both the definition
/// and use sites, then print the new source code.
pub(crate) fn rename(src: &str, path: &Path, offset: usize, new_name: &str) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_current_namespace(path);
    load_toplevel_items(&items, &mut env, ns);

    let summary = check_types(path, &items, &env);

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
    id_to_pos: FxHashMap<SyntaxId, Position>,
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
