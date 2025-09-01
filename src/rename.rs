use std::path::Path;

use crate::parser::ast::{AstId, IdGenerator, Symbol, SyntaxId};
use crate::parser::parse_toplevel_items;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::parser::visitor::Visitor;
use rustc_hash::FxHashMap;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::pos_to_id::find_item_at;

/// Rename the symbol at `offset` to `new_name`, both the definition
/// and use sites, then print the new source code.
pub(crate) fn rename(
    src: &str,
    path: &Path,
    offset: usize,
    new_name: &str,
) -> Result<String, String> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let summary = check_types(&vfs_path, &items, &env, ns);

    let ids_at_pos = find_item_at(&items, offset, offset);

    let Some(AstId::Sym(id)) = ids_at_pos.last() else {
        return Err(format!("No symbol found at offset {offset}"));
    };
    let Some(def_pos) = summary.id_to_def_pos.get(id) else {
        return Err(format!("No definition found for id {id:?}"));
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
    Ok(new_src)
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
