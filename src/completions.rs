use std::path::Path;

use garden_lang_parser::{
    ast::{Expression_, ToplevelItem},
    parse_toplevel_items,
};
use serde::Serialize;

use crate::{
    checks::type_checker::check_types,
    env::Env,
    eval::eval_defs,
    garden_type::Type,
    pos_to_id::{find_expr_of_id, find_item_at},
};

pub(crate) fn complete(src: &str, path: &Path, offset: usize) {
    let mut env = Env::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut env.id_gen);

    let mut definitions = vec![];
    for item in &items {
        if let ToplevelItem::Def(def) = item {
            definitions.push(def.clone());
        }
    }

    eval_defs(&definitions, &mut env);

    let ids_at_pos = find_item_at(&items, offset);

    let (_, id_to_ty, _, _) = check_types(&items, &env);

    if let Some(id) = ids_at_pos.last() {
        if let Some(expr) = find_expr_of_id(&items, *id) {
            match &expr.expr_ {
                Expression_::DotAccess(recv, meth_sym) => {
                    let recv_id = recv.id;
                    let recv_ty = &id_to_ty[&recv_id];

                    let prefix = if meth_sym.name.is_placeholder() {
                        ""
                    } else {
                        &meth_sym.name.0
                    };
                    print_methods(&env, recv_ty, prefix);
                }
                _ => {}
            }
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct CompletionItem {
    name: String,
    suffix: String,
}

fn print_methods(env: &Env, recv_ty: &Type, prefix: &str) {
    let Some(type_name) = recv_ty.type_name() else {
        return;
    };

    let Some(methods) = env.methods.get(&type_name) else {
        return;
    };

    let mut items = vec![];

    for (method_name, meth_info) in methods.iter() {
        if !method_name.0.starts_with(prefix) {
            continue;
        }

        let Some(fun_info) = meth_info.fun_info() else {
            continue;
        };

        let params = &fun_info
            .params
            .iter()
            .map(|param| match &param.hint {
                Some(hint) => hint.as_src(),
                None => "_".to_owned(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_hint = match &fun_info.return_hint {
            Some(hint) => format!(": {}", hint.as_src()),
            None => "".to_owned(),
        };

        items.push(CompletionItem {
            name: method_name.0.clone(),
            suffix: format!("({}){}", params, return_hint),
        });
    }

    items.sort();
    for item in items {
        println!("{}", serde_json::to_string(&item).unwrap());
    }
}
