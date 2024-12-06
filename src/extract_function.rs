use std::path::Path;

use garden_lang_parser::{ast::SyntaxIdGenerator, parse_toplevel_items};

use crate::{checks::type_checker::check_types, env::Env, eval::load_toplevel_items};

pub(crate) fn extract_function(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
    name: &str,
) {
    let mut id_gen = SyntaxIdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(&mut id_gen);
    load_toplevel_items(&items, &mut env);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    println!("todo extract")
}
