use std::{collections::HashMap, path::Path};

use garden_lang_parser::{
    ast::{AstId, Symbol, SyntaxId},
    parse_toplevel_items,
    position::Position,
    visitor::Visitor,
};

use crate::{
    checks::type_checker::check_types, env::Env, eval::load_toplevel_items, pos_to_id::find_item_at,
};

pub(crate) fn rename(src: &str, path: &Path, offset: usize, new_name: &str) {
    let mut env = Env::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut env.id_gen);

    load_toplevel_items(&items, &mut env);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    let ids_at_pos = find_item_at(&items, offset);

    let Some(AstId::Sym(id)) = ids_at_pos.last() else {
        eprintln!("No symbol found at offset {}", offset);
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

    let new_src = apply_renames(src, new_name, &visitor.replace_positions);
    print!("{}", remove_testing_footer(&new_src));
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

/// Drop the `// args: ` and `// expected stdout:` footer, otherwise
/// we make the comment longer on every run of the test suite.
fn remove_testing_footer(src: &str) -> String {
    let mut new_src = String::with_capacity(src.len());
    for line in src.lines() {
        if line.starts_with("// args: rename ") {
            break;
        }
        new_src.push_str(line);
        new_src.push('\n');
    }

    new_src
}
