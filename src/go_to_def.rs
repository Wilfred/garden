use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::parser::ast::IdGenerator;
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::{to_project_relative, Vfs};
use crate::pos_to_id::find_item_at;

/// Print the position of the definition associated with the
/// expression at `offset`.
pub(crate) fn print_pos(src: &str, path: &Path, offset: usize, in_test_suite: bool) {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let summary = check_types(&vfs_path, &items, &env, ns);

    let mut ids_at_query_pos = vec![];
    if offset > 0 {
        // When the cursor is rendered between characters,
        // e.g. `foobar|()`, then we want to consider `foobar` as a
        // thing to do go-to-def on.
        ids_at_query_pos.extend(find_item_at(&items, offset - 1, offset - 1));
    }
    ids_at_query_pos.extend(find_item_at(&items, offset, offset));

    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = summary.id_to_def_pos.get(&id.id()) {
            let mut pos_s = serde_json::to_string(&pos).unwrap();

            // For Garden's test suite we don't want to use absolute
            // paths in the expected output.
            if in_test_suite {
                let mut placeholder_pos = pos.clone();
                // Positions in the prelude tend to change a lot. Use
                // a placeholder value so the tests don't change every
                // time we fix a typo in the prelude.
                //
                // TODO: the proper solution would be placeholders in
                // the golden test output, like LLVM's lit.
                if pos.path.ends_with("__prelude.gdn") {
                    placeholder_pos.start_offset = 12345;
                    placeholder_pos.end_offset = 12345;
                    placeholder_pos.line_number = 12345;
                    placeholder_pos.end_line_number = 12345;
                    placeholder_pos.column = 12345;
                    placeholder_pos.end_column = 12345;
                }

                let mut path = PathBuf::from("GDN_TEST_ROOT");
                path.push(to_project_relative(&pos.path, &env.project_root));
                placeholder_pos.path = Rc::new(path);

                pos_s = serde_json::to_string(&placeholder_pos).unwrap();
                pos_s = pos_s.replace("12345", "GDN_TEST_POS");
            }

            println!("{}", pos_s);
            return;
        }
    }

    println!("null");
}
