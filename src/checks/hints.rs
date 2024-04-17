use std::collections::HashSet;

use garden_lang_parser::ast::{FunInfo, TypeHint, TypeName};

use crate::{
    diagnostics::Warning,
    env::Env,
    types::{BuiltinType, TypeDef},
};

pub(crate) fn check_types_exist(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    let type_params: HashSet<_> = fun_info.type_params.iter().map(|p| &p.name).collect();

    for param in &fun_info.params {
        if let Some(param_hint) = &param.type_ {
            warnings.extend(check_type_hint(param_hint, &type_params, env));
        }
    }

    if let Some(return_hint) = &fun_info.return_type {
        warnings.extend(check_type_hint(return_hint, &type_params, env));
    }

    warnings
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

/// Check that `type_hint` mentions a defined type, and that it has
/// the correct number of type arguments.
pub(crate) fn check_type_hint(
    type_hint: &TypeHint,
    bound_type_params: &HashSet<&TypeName>,
    env: &Env,
) -> Vec<Warning> {
    let mut warnings = vec![];

    match env.get_type_def(&type_hint.sym.name) {
        _ if bound_type_params.contains(&type_hint.sym.name) => {
            if let Some(first_arg) = type_hint.args.first() {
                warnings.push(Warning {
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
                        warnings.push(Warning {
                            message: format_type_arity_error(type_hint, num_expected),
                            position: type_args_pos,
                        });
                    }
                }
                TypeDef::Enum(enum_info) => {
                    if enum_info.type_params.len() != type_hint.args.len() {
                        warnings.push(Warning {
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
                        warnings.push(Warning {
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
            warnings.push(Warning {
                message: format!("No such type: {}", &type_hint.sym),
                position: type_hint.position.clone(),
            });
        }
    }

    for type_arg in &type_hint.args {
        warnings.extend(check_type_hint(type_arg, bound_type_params, env));
    }

    warnings
}
