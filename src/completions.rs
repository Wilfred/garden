use std::path::Path;

use rustc_hash::FxHashMap;
use serde::Serialize;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::parser::ast::{AstId, Expression_, IdGenerator, SymbolName};
use crate::parser::parse_toplevel_items;
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;
use crate::Vfs;

pub(crate) fn complete(src: &str, path: &Path, offset: usize) {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let ids_at_pos = find_item_at(&items, offset, offset);
    let summary = check_types(&vfs_path, &items, &env, ns);

    for id in ids_at_pos.iter().rev() {
        let AstId::Expr(expr_id) = id else {
            continue;
        };
        let Some(expr) = find_expr_of_id(&items, *expr_id) else {
            break;
        };
        match &expr.expr_ {
            Expression_::DotAccess(recv, meth_sym) => {
                let recv_id = recv.id;
                let recv_ty = &summary.id_to_ty[&recv_id];

                let prefix = if meth_sym.name.is_placeholder() {
                    ""
                } else {
                    &meth_sym.name.text
                };
                print_methods(&env, recv_ty, prefix);
                return;
            }
            Expression_::Variable(sym) => {
                let prefix = if sym.name.is_placeholder() {
                    ""
                } else {
                    &sym.name.text
                };
                if let Some(bindings) = summary.id_to_bindings.get(expr_id) {
                    print_local_variables(bindings, prefix);
                }
                return;
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct CompletionItem {
    /// Shown as the name and inserted when the user chooses this item.
    name: String,
    /// Extra information shown immediately after the completion item.
    suffix: String,
}

fn print_local_variables(bindings: &[(SymbolName, Type)], prefix: &str) {
    let mut items: Vec<CompletionItem> = vec![];

    for (name, ty) in bindings {
        if !name.text.starts_with(prefix) {
            continue;
        }

        items.push(CompletionItem {
            name: name.text.clone(),
            suffix: format!(": {}", ty),
        });
    }

    items.sort();
    for item in items {
        println!("{}", serde_json::to_string(&item).unwrap());
    }
}

fn print_methods(env: &Env, recv_ty: &Type, prefix: &str) {
    let Some(type_name) = recv_ty.type_name() else {
        return;
    };

    let empty_hashmap = FxHashMap::default();
    let methods = env
        .types
        .get(&type_name)
        .map(|tdm| &tdm.methods)
        .unwrap_or(&empty_hashmap);

    let mut items: Vec<CompletionItem> = vec![];

    for (method_name, meth_info) in methods.iter() {
        if !method_name.text.starts_with(prefix) {
            continue;
        }

        let Some(fun_info) = meth_info.fun_info() else {
            continue;
        };

        let params = &fun_info
            .params
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

        let (name, suffix) = if params.is_empty() {
            // Complete parentheses too if there are zero parameters.
            (format!("{}()", method_name.text), return_hint)
        } else {
            (method_name.text.clone(), format!("({params}){return_hint}"))
        };

        items.push(CompletionItem { name, suffix });
    }

    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&type_name) {
        for field in &struct_info.fields {
            if !field.sym.name.text.starts_with(prefix) {
                continue;
            }

            items.push(CompletionItem {
                name: field.sym.name.text.clone(),
                suffix: format!(": {}", field.hint.as_src()),
            });
        }
    }

    items.sort();
    for item in items {
        println!("{}", serde_json::to_string(&item).unwrap());
    }
}
