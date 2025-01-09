//! Check for duplicate definitions within a file. It's fine to
//! redefine a definition that's currently in the environment, that's
//! normal program development. Repeated definitions in a file is a
//! mistake however.

use std::collections::hash_map::Entry;
use std::collections::HashSet;

use garden_lang_parser::ast::{SymbolName, ToplevelItem, ToplevelItem_, TypeName};
use garden_lang_parser::position::Position;
use garden_lang_parser::visitor::Visitor;
use rustc_hash::FxHashMap;

use crate::diagnostics::Level;
use crate::{diagnostics::Diagnostic, env::Env};

struct DuplicatesVisitor {
    funs_seen: FxHashMap<SymbolName, Position>,
    methods_seen: FxHashMap<TypeName, FxHashMap<SymbolName, Position>>,
    types_seen: HashSet<TypeName>,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for DuplicatesVisitor {
    fn visit_toplevel_item(&mut self, def: &ToplevelItem) {
        match &def.2 {
            ToplevelItem_::Fun(sym, _, _) => {
                if self.funs_seen.contains_key(&sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: format!(
                            "The function `{}` is already defined in this file.",
                            sym.name
                        ),
                        position: sym.position.clone(),
                        level: Level::Warning,
                    });
                } else {
                    self.funs_seen
                        .insert(sym.name.clone(), sym.position.clone());
                }
            }
            ToplevelItem_::Method(method_info, _) => {
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
                        message: format!(
                            "The method `{}::{}` is already defined in this file.",
                            type_name, meth_sym.name
                        ),
                        position: meth_sym.position.clone(),
                        level: Level::Warning,
                    });
                }
            }
            ToplevelItem_::Test(_) => {}
            ToplevelItem_::Enum(enum_info) => {
                let name_sym = &enum_info.name_sym;
                if self.types_seen.contains(&name_sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: format!(
                            "The type `{}` is already defined in this file.",
                            &name_sym.name
                        ),
                        position: name_sym.position.clone(),
                        level: Level::Warning,
                    });
                } else {
                    self.types_seen.insert(name_sym.name.clone());
                }
            }
            ToplevelItem_::Struct(struct_info) => {
                let name_sym = &struct_info.name_sym;
                if self.types_seen.contains(&name_sym.name) {
                    self.diagnostics.push(Diagnostic {
                        message: format!(
                            "The type `{}` is already defined in this file.",
                            &name_sym.name
                        ),
                        position: name_sym.position.clone(),
                        level: Level::Warning,
                    });
                } else {
                    self.types_seen.insert(name_sym.name.clone());
                }
            }
            ToplevelItem_::Expr(_) => {}
            ToplevelItem_::Import(_) => todo!(),
        }

        self.visit_def_(&def.2);
    }
}

pub(crate) fn check_duplicates(items: &[ToplevelItem], _env: &Env) -> Vec<Diagnostic> {
    let mut visitor = DuplicatesVisitor {
        diagnostics: vec![],
        funs_seen: FxHashMap::default(),
        methods_seen: FxHashMap::default(),
        types_seen: HashSet::default(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
