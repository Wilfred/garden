use std::collections::{HashMap, HashSet};

use garden_lang_parser::ast::{Expression, Symbol, ToplevelItem, TypeSymbol};

use crate::diagnostics::Level;
use crate::visitor::Visitor;
use crate::{diagnostics::Diagnostic, env::Env, types::TypeDef};

struct StructFieldVisitor<'a> {
    env: &'a Env,
    warnings: Vec<Diagnostic>,
}

impl Visitor for StructFieldVisitor<'_> {
    fn visit_expr_struct_literal(
        &mut self,
        name_sym: &TypeSymbol,
        field_exprs: &[(Symbol, Expression)],
    ) {
        for (_, expr) in field_exprs {
            self.visit_expr(expr);
        }

        let Some(TypeDef::Struct(struct_info)) = self.env.get_type_def(&name_sym.name) else {
            return;
        };

        let mut fields_by_name = HashMap::new();
        for field_info in &struct_info.fields {
            fields_by_name.insert(field_info.sym.name.clone(), field_info);
        }

        let mut seen_fields = HashSet::new();

        for (field_sym, _) in field_exprs {
            if seen_fields.contains(&field_sym.name) {
                self.warnings.push(Diagnostic {
                    level: Level::Warning,
                    message: format!("Duplicate field `{}` in struct literal.", field_sym.name),
                    position: field_sym.position.clone(),
                });
            }

            seen_fields.insert(field_sym.name.clone());

            if !fields_by_name.contains_key(&field_sym.name) {
                self.warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!(
                        "Struct `{}` has no field named `{}`",
                        name_sym.name, field_sym.name,
                    ),
                    position: field_sym.position.clone(),
                });
            }
        }

        for field_info in struct_info.fields.iter() {
            if !seen_fields.contains(&field_info.sym.name) {
                self.warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!("Missing field `{}` in struct literal.", field_info.sym.name,),
                    position: name_sym.position.clone(),
                });
            }
        }
    }
}

pub(crate) fn check_struct_fields(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut visitor = StructFieldVisitor {
        env,
        warnings: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.warnings
}
