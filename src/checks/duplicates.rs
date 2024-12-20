//! Check for duplicate definitions within a file. It's fine to
//! redefine a definition that's currently in the environment, that's
//! normal program development. Repeated definitions in a file is a
//! mistake however.

use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

use garden_lang_parser::ast::{Definition, Definition_, SymbolName, ToplevelItem, TypeName};
use garden_lang_parser::position::Position;
use garden_lang_parser::visitor::Visitor;

use crate::diagnostics::Level;
use crate::{diagnostics::Diagnostic, env::Env};

struct DuplicatesVisitor {
    funs_seen: HashMap<SymbolName, Position>,
    methods_seen: HashMap<TypeName, HashMap<SymbolName, Position>>,
    types_seen: HashSet<TypeName>,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for DuplicatesVisitor {
    fn visit_def(&mut self, def: &Definition) {
        match &def.2 {
            Definition_::Fun(sym, _, _, _) => {
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
            Definition_::Method(method_info, _) => {
                let meth_sym = &method_info.name_sym;

                let mut is_repeat = false;
                let type_name = &method_info.receiver_hint.sym.name;
                match self.methods_seen.entry(type_name.clone()) {
                    Entry::Occupied(occupied) => {
                        is_repeat = occupied.get().contains_key(&meth_sym.name);
                    }
                    Entry::Vacant(vacant) => {
                        let mut items = HashMap::default();
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
            Definition_::Test(_) => {}
            Definition_::Enum(enum_info) => {
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
            Definition_::Struct(struct_info) => {
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
        }

        self.visit_def_(&def.2);
    }
}

pub(crate) fn check_duplicates(items: &[ToplevelItem], _env: &Env) -> Vec<Diagnostic> {
    let mut visitor = DuplicatesVisitor {
        diagnostics: vec![],
        funs_seen: HashMap::default(),
        methods_seen: HashMap::default(),
        types_seen: HashSet::default(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
