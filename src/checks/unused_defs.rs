use std::collections::HashSet;

use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Severity};
use crate::msgtext;
use crate::parser::ast::{ToplevelItem, ToplevelItemId, Visibility};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;

use super::type_checker::TCSummary;

pub(crate) fn check_unused_defs(items: &[ToplevelItem], summary: &TCSummary) -> Vec<Diagnostic> {
    // All the definitions that are called from another definition, excluding self calls.
    let mut all_called_defs: HashSet<ToplevelItemId> = HashSet::new();

    for (def_id, called_defs) in &summary.callees {
        for called_def_id in called_defs {
            if Some(*called_def_id) != *def_id {
                all_called_defs.insert(*called_def_id);
            }
        }
    }

    let mut diagnostics = vec![];
    let mut already_covered_ids = HashSet::new();
    for item in items {
        let (visibility, symbol, item_id) = match &item {
            ToplevelItem::Fun(symbol, fun_info, visibility) => {
                (visibility, symbol, fun_info.item_id)
            }
            ToplevelItem::Method(method_info, visibility) => (
                visibility,
                &method_info.name_sym,
                method_info.fun_info().and_then(|fun_info| fun_info.item_id),
            ),
            _ => continue,
        };
        let Some(item_id) = item_id else {
            continue;
        };

        if matches!(visibility, Visibility::Public(_)) {
            already_covered_ids.insert(item_id);
            continue;
        }

        // Report unreachable functions that have no callers at all.
        if !all_called_defs.contains(&item_id) {
            diagnostics.push(Diagnostic {
                message: ErrorMessage(vec![
                    Code(format!("{}", symbol.name)),
                    msgtext!(" is never called."),
                ]),
                position: symbol.position.clone(),
                notes: vec![],
                severity: Severity::Warning,
            });
            already_covered_ids.insert(item_id);
        }
    }

    let transitively_reachable = transitive_closure(summary.callees.clone());
    let reachable_from = invert(transitively_reachable);

    for item in items {
        match &item {
            ToplevelItem::Fun(symbol, fun_info, visibility) => {
                if matches!(visibility, Visibility::Public(_)) {
                    continue;
                }

                let Some(item_id) = fun_info.item_id else {
                    continue;
                };
                let Some(reachable_from_item_ids) = reachable_from.get(&item_id) else {
                    continue;
                };

                // Reachable from a toplevel block.
                if reachable_from_item_ids.contains(&None) {
                    continue;
                }

                // Report unreachable functions that contain cycles.
                let reachable_from_item_ids: HashSet<ToplevelItemId> = reachable_from_item_ids
                    .iter()
                    .filter_map(|def_id| *def_id)
                    .collect();
                if reachable_from_item_ids.is_disjoint(&already_covered_ids) {
                    diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![
                            Code(format!("{}", &symbol.name)),
                            msgtext!(" is never called."),
                        ]),
                        position: symbol.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                }
            }
            _ => {}
        }
    }

    diagnostics
}

/// {A -> {B}, B -> {C}, C -> {D}}
///
/// {A -> {B, C, D}, B -> {C, D}, C -> {D}}
fn transitive_closure(
    mut reachable: FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>>,
) -> FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>> {
    let mut changed = true;

    while changed {
        changed = false;

        let reachable_copy = reachable.clone();
        for (def_id, reachable_def_ids) in reachable_copy.iter() {
            for reachable_def_id in reachable_def_ids {
                let next_step_ids = reachable_copy
                    .get(&Some(*reachable_def_id))
                    .cloned()
                    .unwrap_or_default();
                for next_step_id in next_step_ids {
                    if !reachable[def_id].contains(&next_step_id) {
                        reachable.get_mut(def_id).unwrap().insert(next_step_id);
                        changed = true;
                    }
                }
            }
        }
    }

    reachable
}

/// Useful for 'reachable from'.
fn invert(
    reachable: FxHashMap<Option<ToplevelItemId>, HashSet<ToplevelItemId>>,
) -> FxHashMap<ToplevelItemId, HashSet<Option<ToplevelItemId>>> {
    let mut inverted: FxHashMap<ToplevelItemId, HashSet<Option<ToplevelItemId>>> =
        FxHashMap::default();

    for (def_id, reachable_def_ids) in reachable {
        for reachable_def_id in reachable_def_ids {
            inverted.entry(reachable_def_id).or_default().insert(def_id);
        }
    }

    inverted
}
