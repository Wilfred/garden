mod free_variables;
mod hints;
mod struct_fields;
mod type_checker;

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::eval_defs;
use garden_lang_parser::ast::{Definition, ToplevelItem};

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
    warnings.extend(check_types(items, &mut env));

    warnings
}
