use std::path::Path;

use garden_lang_parser::{
    ast::{Expression_, ToplevelItem},
    parse_toplevel_items,
};

use crate::{
    checks::{assign_ids::assign_toplevel_item_ids, type_checker::check_types},
    env::Env,
    eval::eval_defs,
    garden_type::Type,
    pos_to_id::{find_expr_of_id, find_item_at, find_receiver_of_id},
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

    let ids_at_pos = find_item_at(&items, offset);

    let (_, id_to_ty, _, _) = check_types(&items, &env);

    if let Some(id) = ids_at_pos.last() {
        if let Some(expr) = find_expr_of_id(&items, *id) {
            match &expr.expr_ {
                Expression_::DotAccess(recv, _) => {
                    let recv_id = recv.id.get().unwrap();
                    let recv_ty = &id_to_ty[recv_id];
                    print_methods(&env, recv_ty);
                }
                _ => {}
            }
        }
    }
}

fn print_methods(env: &Env, recv_ty: &Type) {
    let Some(type_name) = recv_ty.type_name() else {
        return;
    };

    let Some(methods) = env.methods.get(&type_name) else {
        return;
    };

    let mut method_names = vec![];

    for method_name in methods.keys() {
        method_names.push(&method_name.0);
    }

    method_names.sort();
    for method_name in method_names {
        println!("{method_name}");
    }
}
