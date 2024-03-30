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

        for (field_sym, _) in field_exprs {
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

pub(crate) fn check_struct_fields(block: &Block, env: &Env) -> Vec<Warning> {
    let mut visitor = StructFieldVisitor {
        env,
        warnings: vec![],
    };

    visitor.visit_block(block);

    visitor.warnings
}
