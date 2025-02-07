mod duplicates;
mod free_variables;
mod hints;
mod loops;
mod struct_fields;
pub mod type_checker;
mod unreachable;
mod unused_defs;

use crate::diagnostics::Diagnostic;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use garden_lang_parser::ast::ToplevelItem;
use loops::check_loops;
use unreachable::check_unreachable;
use unused_defs::check_unused_defs;

use self::duplicates::check_duplicates;
use self::hints::check_hints;
use self::type_checker::check_types;
use self::{free_variables::check_free_variables, struct_fields::check_struct_fields};

/// Check toplevel items in a fresh environment, without any definitions from the current session.
pub(crate) fn check_toplevel_items(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut env: Env = env.clone();
    let (mut diagnostics, _) = load_toplevel_items(items, &mut env);

    diagnostics.extend(check_toplevel_items_in_env(items, &env));
    diagnostics
}

/// Check toplevel items in this environment.
pub(crate) fn check_toplevel_items_in_env(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    diagnostics.extend(check_free_variables(items, env));
    diagnostics.extend(check_struct_fields(items, env));
    diagnostics.extend(check_hints(items, env));

    let summary = check_types(items, env);
    diagnostics.extend(check_unused_defs(items, &summary));

    diagnostics.extend(summary.diagnostics);
    diagnostics.extend(check_duplicates(items, env));
    diagnostics.extend(check_loops(items));
    diagnostics.extend(check_unreachable(items));

    diagnostics
}
