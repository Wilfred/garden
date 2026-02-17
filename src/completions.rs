use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{AstId, Expression_, IdGenerator, SymbolName};
use crate::parser::parse_toplevel_items;
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;
use crate::values::Value_;
use crate::Vfs;

pub(crate) fn complete(src: &str, path: &Path, offset: usize) -> Vec<CompletionItem> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    // Check both offset and offset-1 to handle cursor positions at
    // the end of expressions (e.g., right after a dot in "foo.").
    let mut ids_at_pos = vec![];
    if offset > 0 {
        ids_at_pos.extend(find_item_at(&items, offset - 1, offset - 1));
    }
    ids_at_pos.extend(find_item_at(&items, offset, offset));
    let summary = check_types(&vfs_path, &items, &env, ns.clone());

    let mut seen_expr_ids = FxHashSet::default();
    for id in ids_at_pos.iter().rev() {
        let AstId::Expr(expr_id) = id else {
            continue;
        };
        // Skip duplicate expression IDs that can occur when checking both
        // offset and offset-1.
        if !seen_expr_ids.insert(*expr_id) {
            continue;
        }
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
                return get_methods(&env, recv_ty, prefix);
            }
            Expression_::NamespaceAccess(recv, sym) => {
                let prefix = if sym.name.is_placeholder() {
                    ""
                } else {
                    &sym.name.text
                };

                // The type checker requires the receiver of foo::bar
                // to be a variable we can statically identify, so we
                // can just assume variable here.
                if let Expression_::Variable(recv_symbol) = &recv.expr_ {
                    return get_namespace_functions(ns, &recv_symbol.name, prefix);
                }
                return vec![];
            }
            Expression_::Variable(sym) => {
                let prefix = if sym.name.is_placeholder() {
                    ""
                } else {
                    &sym.name.text
                };
                if let Some(bindings) = summary.id_to_bindings.get(expr_id) {
                    let mut items = get_local_variables(bindings, prefix);
                    items.extend_from_slice(&get_namespace_values(ns, prefix));
                    items.sort_by(|a, b| a.label.cmp(&b.label));

                    return items;
                }
                return vec![];
            }
            _ => {}
        }
    }
    vec![]
}

/// Values that are available in the toplevel of this namespace.
fn get_namespace_values(
    current_ns: Rc<RefCell<NamespaceInfo>>,
    prefix: &str,
) -> Vec<CompletionItem> {
    let current_ns = current_ns.borrow();

    let mut items: Vec<CompletionItem> = vec![];

    for (name, value) in current_ns.values.iter() {
        if !name.text.starts_with(prefix) {
            continue;
        }

        let ty = Type::from_value(value);
        items.push(CompletionItem {
            label: name.text.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(format!(": {}", ty)),
            ..Default::default()
        });
    }

    items
}

fn get_local_variables(bindings: &[(SymbolName, Type)], prefix: &str) -> Vec<CompletionItem> {
    let mut items: Vec<CompletionItem> = vec![];

    for (name, ty) in bindings {
        if !name.text.starts_with(prefix) {
            continue;
        }

        items.push(CompletionItem {
            label: name.text.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(format!(": {}", ty)),
            ..Default::default()
        });
    }

    items.sort_by(|a, b| a.label.cmp(&b.label));
    items
}

fn get_methods(env: &Env, recv_ty: &Type, prefix: &str) -> Vec<CompletionItem> {
    let Some(type_name) = recv_ty.type_name() else {
        return vec![];
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

        let (label, insert_text) = if params.is_empty() {
            // Complete parentheses too if there are zero parameters.
            let name = format!("{}()", method_name.text);
            (name.clone(), name)
        } else {
            (
                method_name.text.clone(),
                format!("{}($0)", method_name.text),
            )
        };

        let detail = format!("({params}){return_hint}");

        items.push(CompletionItem {
            label,
            kind: Some(CompletionItemKind::METHOD),
            detail: Some(detail),
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&type_name) {
        for field in &struct_info.fields {
            if !field.sym.name.text.starts_with(prefix) {
                continue;
            }

            items.push(CompletionItem {
                label: field.sym.name.text.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!(": {}", field.hint.as_src())),
                ..Default::default()
            });
        }
    }

    items.sort_by(|a, b| a.label.cmp(&b.label));
    items
}

fn get_namespace_functions(
    current_ns: Rc<RefCell<NamespaceInfo>>,
    ns_name: &SymbolName,
    prefix: &str,
) -> Vec<CompletionItem> {
    let current_ns = current_ns.borrow();
    let Some(value) = current_ns.values.get(ns_name) else {
        return vec![];
    };

    let Value_::Namespace { ns_info, .. } = value.as_ref() else {
        return vec![];
    };

    let ns_info = ns_info.borrow();
    let mut items: Vec<CompletionItem> = vec![];

    for (value_name, value) in ns_info.values.iter() {
        // Don't offer private symbols.
        if !ns_info.exported_syms.contains(value_name) {
            continue;
        }

        if !value_name.text.starts_with(prefix) {
            continue;
        }

        // Only complete functions, since that's all we can export
        // right now.
        let Some(fun_info) = value.fun_info() else {
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

        let (label, insert_text) = if params.is_empty() {
            let name = format!("{}()", value_name.text);
            (name.clone(), name)
        } else {
            (value_name.text.clone(), format!("{}($0)", value_name.text))
        };

        let detail = format!("({params}){return_hint}");

        items.push(CompletionItem {
            label,
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail),
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    items.sort_by(|a, b| a.label.cmp(&b.label));
    items
}
