//! Find all occurrences of the local variable at a given position,
//! used for semantic highlighting in editors.

use std::path::Path;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::parser::ast::{IdGenerator, Symbol, SyntaxId};
use crate::parser::parse_toplevel_items;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::parser::visitor::Visitor;
use crate::pos_to_id::find_item_at;

/// Find all occurrences of the variable at `offset`, including its
/// definition site.
///
/// Returns an empty vector if there is no variable at that offset.
pub(crate) fn highlight_occurrences(src: &str, path: &Path, offset: usize) -> Vec<Position> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, Rc::clone(&ns));

    let summary = check_types(&vfs_path, &items, &env, ns);

    // When the cursor sits between characters, e.g. `foo|()`, also
    // consider the symbol immediately to the left.
    let mut ids_at_query_pos = vec![];
    if offset > 0 {
        ids_at_query_pos.extend(find_item_at(&items, offset - 1, offset - 1));
    }
    ids_at_query_pos.extend(find_item_at(&items, offset, offset));

    let mut def_pos = None;
    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = summary.id_to_def_pos.get(&id.id()) {
            def_pos = Some(pos.clone());
            break;
        }
    }

    let Some(def_pos) = def_pos else {
        return vec![];
    };

    let mut visitor = HighlightVisitor {
        definition_pos: def_pos,
        id_to_pos: &summary.id_to_def_pos,
        positions: vec![],
    };

    for item in &items {
        visitor.visit_toplevel_item(item);
    }

    let mut positions = visitor.positions;
    positions.sort_unstable_by_key(|p| p.start_offset);
    positions
}

struct HighlightVisitor<'a> {
    definition_pos: Position,
    id_to_pos: &'a FxHashMap<SyntaxId, Position>,
    positions: Vec<Position>,
}

impl Visitor for HighlightVisitor<'_> {
    fn visit_symbol(&mut self, symbol: &Symbol) {
        let Some(sym_def_pos) = self.id_to_pos.get(&symbol.id) else {
            return;
        };
        if *sym_def_pos != self.definition_pos {
            return;
        }

        self.positions.push(symbol.position.clone());
    }
}
