use garden_lang_parser::ast::{Block, Expression, Expression_};

use crate::{diagnostics::Warning, env::Env, types::TypeDef};

use super::visitor::{Checker, Visitor as _};

struct StructFieldChecker<'a> {
    env: &'a Env,
    warnings: Vec<Warning>,
}

impl Checker for StructFieldChecker<'_> {
    fn check_expr(&mut self, expr: &Expression) {
        if let Expression_::StructLiteral(name_sym, fields) = &expr.1 {
            let Some(def) = self.env.get_type_def(&name_sym.name) else {
                return;
            };
            let TypeDef::Struct(struct_info) = def else {
                return;
            };

            for (field_sym, _) in fields {
                let defined_field = struct_info
                    .fields
                    .iter()
                    .find(|field_info| field_info.sym.name == field_sym.name);

                if defined_field.is_none() {
                    self.warnings.push(Warning {
                        message: format!(
                            "Struct `{}` has no field named `{}`",
                            name_sym.name, field_sym.name,
                        ),
                        position: field_sym.position.clone(),
                    });
                }
            }
        }
    }
}

pub(crate) fn check_struct_fields(block: &Block, env: &Env) -> Vec<Warning> {
    let mut checker = StructFieldChecker {
        env,
        warnings: vec![],
    };

    block.visit(&mut checker);
    checker.warnings
}
