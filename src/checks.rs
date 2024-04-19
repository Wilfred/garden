mod free_variables;
mod hints;
mod struct_fields;
mod type_checker;

use std::collections::HashSet;

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::eval_defs;
use garden_lang_parser::ast::{Definition, Definition_, FunInfo, MethodKind, ToplevelItem};

use self::hints::{check_type_hint, check_types_exist};
use self::type_checker::check_types;
use self::{free_variables::check_free_variables, struct_fields::check_struct_fields};

pub(crate) fn check_toplevel_items(items: &[ToplevelItem]) -> Vec<Warning> {
    let mut warnings = vec![];

    let mut definitions: Vec<Definition> = vec![];
    for item in items {
        if let ToplevelItem::Def(def) = item {
            definitions.push(def.clone());
        }
    }

    // TODO: define separate checks for things we can check without an
    // environment, and checks that are relative to a given environment
    // (e.g. type is defined).
    let mut env = Env::default();
    eval_defs(&definitions, &mut env);

    warnings.extend(check_free_variables(items, &env));
    warnings.extend(check_struct_fields(items, &env));

    for def in &definitions {
        warnings.extend(check_def(def, &env));
    }

    warnings
}

pub(crate) fn check_def(def: &Definition, env: &Env) -> Vec<Warning> {
    match &def.2 {
        Definition_::Fun(_, fun_info) => check_fun_info(fun_info, env),
        Definition_::Method(meth_info) => {
            let fun_info = match &meth_info.kind {
                MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
                MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
            };

            let mut warnings = vec![];

            let bound_type_params: HashSet<_> = match fun_info {
                Some(fun_info) => fun_info.type_params.iter().map(|p| &p.name).collect(),
                None => HashSet::default(),
            };
            warnings.extend(check_type_hint(
                &meth_info.receiver_type,
                &bound_type_params,
                env,
            ));

            if let Some(fun_info) = fun_info {
                warnings.extend(check_fun_info(fun_info, env));
            }

            warnings
        }
        Definition_::Test(_) => vec![],
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

fn check_fun_info(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    warnings.extend(check_types_exist(fun_info, env));
    warnings.extend(check_types(fun_info));

    warnings
}
