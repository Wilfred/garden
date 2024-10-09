use std::path::Path;

use garden_lang_parser::parse_toplevel_items;

use crate::{checks::type_checker::check_types, env::Env, eval::eval_toplevel_defs};

pub(crate) fn rename(src: &str, path: &Path, offset: usize, new_name: &str) {
    let mut env = Env::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut env.id_gen);

    eval_toplevel_defs(&items, &mut env);
    let (_, _, _, id_to_pos) = check_types(&items, &env);

    print!("{}", src);
}
