use std::collections::{HashMap, HashSet};

use garden_lang_parser::ast::{Block, Expression, Symbol, TypeSymbol};

use crate::{diagnostics::Warning, env::Env, types::TypeDef};

use super::visitor::Visitor;

struct StructFieldVisitor<'a> {
    env: &'a Env,
    warnings: Vec<Warning>,
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
                self.warnings.push(Warning {
                    message: format!("Duplicate field `{}` in struct literal.", field_sym.name),
                    position: field_sym.position.clone(),
                });
            }

            seen_fields.insert(field_sym.name.clone());

            if !fields_by_name.contains_key(&field_sym.name) {
                self.warnings.push(Warning {
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
                self.warnings.push(Warning {
                    message: format!("Missing field `{}` in struct literal.", field_info.sym.name,),
                    position: name_sym.position.clone(),
                });
            }
        }
    }
}

pub(crate) fn check_struct_fields(block: &Block, env: &Env) -> Vec<Warning> {
    let mut visitor = StructFieldVisitor {
        env,
        warnings: vec![],
    };

    visitor.visit_block(block);

    visitor.warnings
}
