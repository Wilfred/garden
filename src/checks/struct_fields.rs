use std::collections::HashSet;
use std::rc::Rc;

use crate::parser::ast::{Expression, Symbol, ToplevelItem, TypeSymbol};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};
use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::types::TypeDef;

struct StructFieldVisitor<'a> {
    env: &'a Env,
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for StructFieldVisitor<'_> {
    fn visit_expr_struct_literal(
        &mut self,
        name_sym: &TypeSymbol,
        field_exprs: &[(Symbol, Rc<Expression>)],
    ) {
        for (_, expr) in field_exprs {
            self.visit_expr(expr);
        }

        let Some(type_def) = self.env.get_type_def(&name_sym.name) else {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                level: Level::Error,
                message: ErrorMessage(vec![
                    msgtext!("No such type "),
                    msgcode!("{}", name_sym),
                    msgtext!("."),
                ]),
                position: name_sym.position.clone(),
            });
            return;
        };
        let TypeDef::Struct(struct_info) = type_def else {
            self.diagnostics.push(Diagnostic {
                notes: vec![],
                level: Level::Error,
                message: ErrorMessage(vec![
                    msgcode!("{}", name_sym),
                    msgtext!(" is not a struct."),
                ]),
                position: name_sym.position.clone(),
            });
            return;
        };

        let mut fields_by_name = FxHashMap::default();
        for field_info in &struct_info.fields {
            fields_by_name.insert(field_info.sym.name.clone(), field_info);
        }

        let mut seen_fields = HashSet::new();

        for (field_sym, _) in field_exprs {
            if seen_fields.contains(&field_sym.name) {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    level: Level::Warning,
                    message: ErrorMessage(vec![
                        msgtext!("Duplicate field "),
                        msgcode!("{}", field_sym.name),
                        msgtext!(" in struct literal."),
                    ]),
                    position: field_sym.position.clone(),
                });
            }

            seen_fields.insert(field_sym.name.clone());

            if !fields_by_name.contains_key(&field_sym.name) {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    level: Level::Error,
                    message: ErrorMessage(vec![
                        msgtext!("Struct "),
                        msgcode!("{}", name_sym.name),
                        msgtext!(" does not have a field named "),
                        msgcode!("{}", field_sym.name),
                        msgtext!("."),
                    ]),
                    position: field_sym.position.clone(),
                });
            }
        }

        for field_info in struct_info.fields.iter() {
            if !seen_fields.contains(&field_info.sym.name) {
                self.diagnostics.push(Diagnostic {
                    notes: vec![],
                    level: Level::Error,
                    message: ErrorMessage(vec![
                        msgtext!("This struct literal is missing the field "),
                        msgcode!("{}", field_info.sym.name),
                        msgtext!("."),
                    ]),
                    position: name_sym.position.clone(),
                });
            }
        }
    }
}

pub(crate) fn check_struct_fields(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut visitor = StructFieldVisitor {
        env,
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
