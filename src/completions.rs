use std::path::Path;

use rustc_hash::{FxHashMap, FxHashSet};
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
    let summary = check_types(&vfs_path, &items, &env, ns);

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
            Expression_::Variable(sym) => {
                let prefix = if sym.name.is_placeholder() {
                    ""
                } else {
                    &sym.name.text
                };
                if let Some(bindings) = summary.id_to_bindings.get(expr_id) {
                    return get_local_variables(bindings, prefix);
                }
                return vec![];
            }
            _ => {}
        }
    }
    vec![]
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub(crate) struct CompletionItem {
    /// Shown as the name and inserted when the user chooses this item.
    name: String,
    /// Extra information shown immediately after the completion item.
    suffix: String,
    /// The kind of completion item (variable, method, field, etc.)
    #[serde(skip)]
    kind: lsp_types::CompletionItemKind,
}

impl PartialOrd for CompletionItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CompletionItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.name, &self.suffix).cmp(&(&other.name, &other.suffix))
    }
}

impl From<CompletionItem> for lsp_types::CompletionItem {
    fn from(item: CompletionItem) -> Self {
        lsp_types::CompletionItem {
            label: item.name.clone(),
            insert_text: Some(item.name),
            detail: if item.suffix.is_empty() {
                None
            } else {
                Some(item.suffix)
            },
            kind: Some(item.kind),
            ..Default::default()
        }
    }
}

fn get_local_variables(bindings: &[(SymbolName, Type)], prefix: &str) -> Vec<CompletionItem> {
    let mut items: Vec<CompletionItem> = vec![];

    for (name, ty) in bindings {
        if !name.text.starts_with(prefix) {
            continue;
        }

        items.push(CompletionItem {
            name: name.text.clone(),
            suffix: format!(": {}", ty),
            kind: lsp_types::CompletionItemKind::VARIABLE,
        });
    }

    items.sort();
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

        let (name, suffix) = if params.is_empty() {
            // Complete parentheses too if there are zero parameters.
            (format!("{}()", method_name.text), return_hint)
        } else {
            (method_name.text.clone(), format!("({params}){return_hint}"))
        };

        items.push(CompletionItem {
            name,
            suffix,
            kind: lsp_types::CompletionItemKind::METHOD,
        });
    }

    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&type_name) {
        for field in &struct_info.fields {
            if !field.sym.name.text.starts_with(prefix) {
                continue;
            }

            items.push(CompletionItem {
                name: field.sym.name.text.clone(),
                suffix: format!(": {}", field.hint.as_src()),
                kind: lsp_types::CompletionItemKind::FIELD,
            });
        }
    }

    items.sort();
    items
}
