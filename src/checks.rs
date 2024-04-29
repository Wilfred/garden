mod duplicates;
mod free_variables;
mod hints;
mod struct_fields;
mod type_checker;

use crate::diagnostics::Diagnostic;
use crate::env::Env;
use crate::eval::eval_defs;
use garden_lang_parser::ast::{Definition, ToplevelItem};

use self::duplicates::check_duplicates;
use self::hints::check_hints;
use self::type_checker::check_types;
use self::{free_variables::check_free_variables, struct_fields::check_struct_fields};

/// Check toplevel items in a fresh environment, without any definitions from the current session.
pub(crate) fn check_toplevel_items(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut definitions: Vec<Definition> = vec![];
    for item in items {
        if let ToplevelItem::Def(def) = item {
            definitions.push(def.clone());
        }
    }

    let mut env = Env::default();
    eval_defs(&definitions, &mut env);

    check_toplevel_items_in_env(items, &env)
}

/// Check toplevel items in this environment.
pub(crate) fn check_toplevel_items_in_env(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut warnings = vec![];

    let mut definitions: Vec<Definition> = vec![];
    for item in items {
        if let ToplevelItem::Def(def) = item {
            definitions.push(def.clone());
        }
    }

    warnings.extend(check_free_variables(items, env));
    warnings.extend(check_struct_fields(items, env));
    warnings.extend(check_hints(items, env));
    warnings.extend(check_types(items, env));
    warnings.extend(check_duplicates(items, env));

    warnings
}
