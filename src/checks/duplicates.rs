//! Check for duplicate definitions within a file. It's fine to
//! redefine a definition that's currently in the environment, that's
//! normal program development. Repeated definitions in a file is a
//! mistake however.

use std::collections::hash_map::Entry;
use std::collections::HashSet;

use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Severity};
use crate::env::Env;
use crate::parser::ast::{SymbolName, ToplevelItem, TypeName};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::visitor::Visitor;

struct DuplicatesVisitor {
    funs_seen: FxHashMap<SymbolName, Position>,
    methods_seen: FxHashMap<TypeName, FxHashMap<SymbolName, Position>>,
    types_seen: HashSet<TypeName>,
    tests_seen: HashSet<SymbolName>,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for DuplicatesVisitor {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        match &item {
            ToplevelItem::Fun(sym, _, _) => {
                if self.funs_seen.contains_key(&sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "The function `{}` is already defined in this file.",
                            sym.name
                        ))]),
                        position: sym.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                } else {
                    self.funs_seen
                        .insert(sym.name.clone(), sym.position.clone());
                }
            }
            ToplevelItem::Method(method_info, _) => {
                let meth_sym = &method_info.name_sym;

                let mut is_repeat = false;
                let type_name = &method_info.receiver_hint.sym.name;
                match self.methods_seen.entry(type_name.clone()) {
                    Entry::Occupied(occupied) => {
                        is_repeat = occupied.get().contains_key(&meth_sym.name);
                    }
                    Entry::Vacant(vacant) => {
                        let mut items = FxHashMap::default();
                        items.insert(meth_sym.name.clone(), meth_sym.position.clone());

                        vacant.insert(items);
                    }
                }

                if is_repeat {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "The method `{}::{}` is already defined in this file.",
                            type_name, meth_sym.name
                        ))]),
                        position: meth_sym.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                }
            }
            ToplevelItem::Test(test_info) => {
                let sym = &test_info.name_sym;
                if self.tests_seen.contains(&sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "The test `{}` is already defined in this file.",
                            sym.name
                        ))]),
                        position: sym.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                } else {
                    self.tests_seen.insert(sym.name.clone());
                }
            }
            ToplevelItem::Enum(enum_info) => {
                let name_sym = &enum_info.name_sym;
                if self.types_seen.contains(&name_sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "The type `{}` is already defined in this file.",
                            &name_sym.name
                        ))]),
                        position: name_sym.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                } else {
                    self.types_seen.insert(name_sym.name.clone());
                }
            }
            ToplevelItem::Struct(struct_info) => {
                let name_sym = &struct_info.name_sym;
                if self.types_seen.contains(&name_sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: ErrorMessage(vec![Text(format!(
                            "The type `{}` is already defined in this file.",
                            &name_sym.name
                        ))]),
                        position: name_sym.position.clone(),
                        notes: vec![],
                        severity: Severity::Warning,
                    });
                } else {
                    self.types_seen.insert(name_sym.name.clone());
                }
            }
            ToplevelItem::Expr(_) => {}
            ToplevelItem::Block(_) => {}
            ToplevelItem::Import(_) => {}
        }

        self.visit_toplevel_item_default(item);
    }
}

pub(crate) fn check_duplicates(items: &[ToplevelItem], _env: &Env) -> Vec<Diagnostic> {
    let mut visitor = DuplicatesVisitor {
        diagnostics: vec![],
        funs_seen: FxHashMap::default(),
        methods_seen: FxHashMap::default(),
        types_seen: HashSet::default(),
        tests_seen: HashSet::default(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
