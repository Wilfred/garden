use std::collections::HashSet;

use garden_lang_parser::ast::{EnumInfo, FunInfo, StructInfo, ToplevelItem, TypeHint, TypeName};

use crate::{
    diagnostics::Warning,
    env::Env,
    types::{BuiltinType, TypeDef},
    visitor::Visitor,
};

pub(crate) fn check_hints(items: &[ToplevelItem], env: &Env) -> Vec<Warning> {
    let mut visitor = HintVisitor {
        env,
        warnings: vec![],
        bound_type_params: HashSet::new(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.warnings
}

/// Check that every type hint mentions a defined type, and that it
/// has the correct number of type arguments.
struct HintVisitor<'a> {
    env: &'a Env,
    warnings: Vec<Warning>,
    bound_type_params: HashSet<TypeName>,
}

impl Visitor for HintVisitor<'_> {
    fn visit_method_info(&mut self, method_info: &garden_lang_parser::ast::MethodInfo) {
        let old_type_params = self.bound_type_params.clone();

        // Bind type parameters before visiting the hint of self, so
        // we handle e.g. `self: List<T>` correctly.
        if let Some(fun_info) = method_info.fun_info() {
            for type_param in &fun_info.type_params {
                self.bound_type_params.insert(type_param.name.clone());
            }
        }

        self.visit_method_info_default(method_info);

        self.bound_type_params = old_type_params;
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        let old_type_params = self.bound_type_params.clone();

        for type_param in &fun_info.type_params {
            self.bound_type_params.insert(type_param.name.clone());
        }

        self.visit_fun_info_default(fun_info);
        self.bound_type_params = old_type_params;
    }

    fn visit_enum_info(&mut self, enum_info: &EnumInfo) {
        let old_type_params = self.bound_type_params.clone();

        for type_param in &enum_info.type_params {
            self.bound_type_params.insert(type_param.name.clone());
        }

        self.visit_enum_info_default(enum_info);
        self.bound_type_params = old_type_params;
    }

    fn visit_struct_info(&mut self, struct_info: &StructInfo) {
        let old_type_params = self.bound_type_params.clone();

        for type_param in &struct_info.type_params {
            self.bound_type_params.insert(type_param.name.clone());
        }
        self.visit_struct_info_default(struct_info);
        self.bound_type_params = old_type_params;
    }

    fn visit_type_hint(&mut self, type_hint: &TypeHint) {
        match self.env.get_type_def(&type_hint.sym.name) {
            _ if self.bound_type_params.contains(&type_hint.sym.name) => {
                if let Some(first_arg) = type_hint.args.first() {
                    self.warnings.push(Warning {
                        message: "Generic type arguments cannot take parameters.".to_owned(),
                        position: first_arg.position.clone(),
                    });
                }
            }
            Some(type_) => {
                let type_args_pos = match type_hint.args.last() {
                    Some(arg) => arg.sym.position.clone(),
                    None => type_hint.position.clone(),
                };

                match type_ {
                    TypeDef::Builtin(b) => {
                        let num_expected = match b {
                            BuiltinType::Int => 0,
                            BuiltinType::String => 0,
                            BuiltinType::Fun => {
                                // TODO: define a syntax and arity for function types.
                                0
                            }
                            BuiltinType::List => 1,
                        };
                        if num_expected != type_hint.args.len() {
                            self.warnings.push(Warning {
                                message: format_type_arity_error(type_hint, num_expected),
                                position: type_args_pos,
                            });
                        }
                    }
                    TypeDef::Enum(enum_info) => {
                        if enum_info.type_params.len() != type_hint.args.len() {
                            self.warnings.push(Warning {
                                message: format_type_arity_error(
                                    type_hint,
                                    enum_info.type_params.len(),
                                ),
                                position: type_args_pos,
                            });
                        }
                    }
                    TypeDef::Struct(struct_info) => {
                        if struct_info.type_params.len() != type_hint.args.len() {
                            self.warnings.push(Warning {
                                message: format_type_arity_error(
                                    type_hint,
                                    struct_info.type_params.len(),
                                ),
                                position: type_args_pos,
                            });
                        }
                    }
                }
            }
            None => {
                self.warnings.push(Warning {
                    message: format!("No such type: {}", &type_hint.sym),
                    position: type_hint.position.clone(),
                });
            }
        }

        for type_arg in &type_hint.args {
            self.visit_type_hint(type_arg);
        }
    }
}

fn format_type_arity_error(type_hint: &TypeHint, num_expected: usize) -> String {
    let num_actual = type_hint.args.len();

    format!(
        "{} takes {} type argument{}, but got {} argument{}.",
        &type_hint.sym.name,
        num_expected,
        if num_expected == 1 { "" } else { "s" },
        num_actual,
        if num_actual == 1 { "" } else { "s" },
    )
}
