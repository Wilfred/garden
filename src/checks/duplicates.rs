use std::collections::HashMap;

use garden_lang_parser::ast::{Definition, Definition_, SymbolName, ToplevelItem};
use garden_lang_parser::position::Position;

use crate::diagnostics::Level;
use crate::visitor::Visitor;
use crate::{diagnostics::Diagnostic, env::Env};

struct DuplicatesVisitor {
    funs_seen: HashMap<SymbolName, Position>,
    warnings: Vec<Diagnostic>,
}

impl Visitor for DuplicatesVisitor {
    fn visit_def(&mut self, def: &Definition) {
        match &def.2 {
            Definition_::Fun(sym, _) => {
                if self.funs_seen.contains_key(&sym.name) {
                    self.warnings.push(Diagnostic {
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
            Definition_::Method(_) => {}
            Definition_::Test(_) => {}
            Definition_::Enum(_) => {}
            Definition_::Struct(_) => {}
        }

        self.visit_def_(&def.2);
    }
}

pub(crate) fn check_duplicates(items: &[ToplevelItem], _env: &Env) -> Vec<Diagnostic> {
    let mut visitor = DuplicatesVisitor {
        warnings: vec![],
        funs_seen: HashMap::default(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.warnings
}
