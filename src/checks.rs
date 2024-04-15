mod free_variables;
mod struct_fields;
mod visitor;

use std::collections::HashSet;

use crate::env::Env;
use crate::eval::eval_defs;
use crate::types::TypeDef;
use crate::{diagnostics::Warning, types::BuiltinType};
use garden_lang_parser::ast::{
    Definition, Definition_, FunInfo, MethodKind, Symbol, TypeHint, TypeName,
};

use self::{
    free_variables::{check_free_variables, check_free_variables_block},
    struct_fields::check_struct_fields,
};

pub(crate) fn check_defs(definitions: &[Definition]) -> Vec<Warning> {
    // TODO: define separate checks for things we can check without an
    // environment, and checks that are relative to a given environment
    // (e.g. type is defined).
    let mut env = Env::default();
    eval_defs(definitions, &mut env);

    let mut warnings = vec![];
    for def in definitions {
        warnings.extend(check_def(def, &env));
    }

    warnings
}

pub(crate) fn check_def(def: &Definition, env: &Env) -> Vec<Warning> {
    match &def.2 {
        Definition_::Fun(_, fun_info) => check_fun_info(fun_info, env, None),
        Definition_::Method(meth_info) => {
            let fun_info = match &meth_info.kind {
                MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
                MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
            };

            let mut warnings = vec![];

            let recv_type_params: HashSet<_> = meth_info
                .receiver_type
                .args
                .iter()
                .map(|p| &p.sym.name)
                .collect();
            warnings.extend(check_type_hint(
                &meth_info.receiver_type,
                &recv_type_params,
                env,
            ));

            if let Some(fun_info) = fun_info {
                warnings.extend(check_fun_info(fun_info, env, Some(&meth_info.receiver_sym)));
            }

            warnings
        }
        Definition_::Test(test_info) => check_free_variables_block(&test_info.body, env),
        Definition_::Enum(enum_info) => {
            let type_params: HashSet<_> = enum_info.type_params.iter().map(|p| &p.name).collect();

            let mut warnings = vec![];
            for variant in &enum_info.variants {
                if let Some(hint) = &variant.payload_hint {
                    warnings.extend(check_type_hint(hint, &type_params, env));
                }
            }

            warnings
        }
        Definition_::Struct(struct_info) => {
            let type_params: HashSet<_> = struct_info.type_params.iter().map(|p| &p.name).collect();

            let mut warnings = vec![];
            for field in &struct_info.fields {
                warnings.extend(check_type_hint(&field.hint, &type_params, env));
            }

            warnings
        }
    }
}

fn check_fun_info(fun_info: &FunInfo, env: &Env, receiver_sym: Option<&Symbol>) -> Vec<Warning> {
    let mut warnings = vec![];

    warnings.extend(check_types_exist(fun_info, env));
    warnings.extend(check_free_variables(fun_info, env, receiver_sym));
    warnings.extend(check_struct_fields(&fun_info.body, env));

    warnings
}

fn check_types_exist(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
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
fn check_type_hint(
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
                            position: type_hint.position.clone(),
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
                            position: type_hint.position.clone(),
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
                            position: type_hint.position.clone(),
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
