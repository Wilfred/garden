mod free_variables;
mod hints;
mod struct_fields;
mod type_checker;

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::eval_defs;
use garden_lang_parser::ast::{Definition, Definition_, FunInfo, MethodKind, ToplevelItem};

use self::hints::check_hints;
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
    warnings.extend(check_hints(items, &env));
    warnings.extend(check_types(items));

    for def in &definitions {
        warnings.extend(check_def(def));
    }

    warnings
}

pub(crate) fn check_def(def: &Definition) -> Vec<Warning> {
    match &def.2 {
        Definition_::Fun(_, fun_info) => check_fun_info(fun_info),
        Definition_::Method(meth_info) => {
            let fun_info = match &meth_info.kind {
                MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
                MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
            };

            let mut warnings = vec![];

            if let Some(fun_info) = fun_info {
                warnings.extend(check_fun_info(fun_info));
            }

            warnings
        }
        Definition_::Test(_) => vec![],
        Definition_::Enum(_) => vec![],
        Definition_::Struct(_) => vec![],
    }
}

fn check_fun_info(fun_info: &FunInfo) -> Vec<Warning> {
    let mut warnings = vec![];

    // warnings.extend(check_types(fun_info));

    warnings
}
