mod duplicates;
mod hints;
mod loops;
mod recursion_variable;
mod same_literal_returns;
mod struct_fields;
mod unnecessary_return;
pub mod type_checker;
mod unreachable;
mod unused_defs;
mod unused_literals;
mod unused_vars;

use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::Diagnostic;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::ToplevelItem;
use crate::parser::vfs::VfsPathBuf;
use loops::check_loops;
use recursion_variable::check_recursion_variables;
use same_literal_returns::check_same_literal_returns;
use unnecessary_return::check_unnecessary_return;
use unreachable::check_unreachable;
use unused_defs::check_unused_defs;

use self::duplicates::check_duplicates;
use self::hints::check_hints;
use self::struct_fields::check_struct_fields;
use self::type_checker::check_types;
use self::unused_literals::check_unused_literals;
use self::unused_vars::check_unused_variables;

/// Check toplevel items in a fresh environment, without any
/// definitions from the current session.
///
/// Note that this creates a new Env and Vfs, so diagnostics returned
/// may refer to files that aren't present in `env.vfs`.
pub(crate) fn check_toplevel_items(
    vfs_path: &VfsPathBuf,
    items: &[ToplevelItem],
    env: &Env,
) -> Vec<Diagnostic> {
    let mut env: Env = env.clone();
    let ns = env.get_or_create_namespace(&vfs_path.path);
    let (mut diagnostics, _) = load_toplevel_items(items, &mut env, ns.clone());

    diagnostics.extend(check_toplevel_items_in_env(vfs_path, items, &env, ns));
    diagnostics
}

/// Check toplevel items in this environment.
pub(crate) fn check_toplevel_items_in_env(
    vfs_path: &VfsPathBuf,
    items: &[ToplevelItem],
    env: &Env,
    namespace: Rc<RefCell<NamespaceInfo>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    diagnostics.extend(check_unused_variables(items));
    diagnostics.extend(check_unused_literals(items, env));
    diagnostics.extend(check_struct_fields(items, env));
    diagnostics.extend(check_hints(items, env));

    let summary = check_types(vfs_path, items, env, namespace);
    diagnostics.extend(check_unused_defs(items, &summary));

    diagnostics.extend(summary.diagnostics);
    diagnostics.extend(check_duplicates(items, env));
    diagnostics.extend(check_loops(items));
    diagnostics.extend(check_unreachable(items));
    diagnostics.extend(check_same_literal_returns(items));
    diagnostics.extend(check_recursion_variables(items));
    diagnostics.extend(check_unnecessary_return(items));

    diagnostics
}
