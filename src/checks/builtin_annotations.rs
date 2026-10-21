//! Check that functions and methods in built-in namespaces have type
//! annotations on every parameter and the return type.

use std::path::Path;

use crate::diagnostics::{Diagnostic, Severity};
use crate::eval::BUILT_IN_FILES;
use crate::parser::ast::{FunInfo, Symbol, ToplevelItem};
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::vfs::VfsPathBuf;
use crate::{msgcode, msgtext};

/// Whether `path` refers to a built-in namespace file, such as
/// `__fs.gdn` or `__prelude.gdn`.
fn is_built_in_path(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
        return false;
    };
    BUILT_IN_FILES
        .iter()
        .any(|(file_name, _)| *file_name == name)
}

fn check_fun_info(name_sym: &Symbol, fun_info: &FunInfo, diagnostics: &mut Vec<Diagnostic>) {
    for param in &fun_info.params.params {
        if param.hint.is_none() {
            diagnostics.push(Diagnostic {
                message: ErrorMessage(vec![
                    msgtext!("Parameters in built-in namespaces must have a type annotation, but "),
                    msgcode!("{}", param.symbol.name),
                    msgtext!(" does not."),
                ]),
                position: param.symbol.position.clone(),
                notes: vec![],
                severity: Severity::Error,
                fixes: vec![],
            });
        }
    }

    if fun_info.return_hint.is_none() {
        diagnostics.push(Diagnostic {
            message: ErrorMessage(vec![
                msgtext!(
                    "Functions in built-in namespaces must have a return type annotation, but "
                ),
                msgcode!("{}", name_sym.name),
                msgtext!(" does not."),
            ]),
            position: name_sym.position.clone(),
            notes: vec![],
            severity: Severity::Error,
            fixes: vec![],
        });
    }
}

pub(crate) fn check_builtin_annotations(
    vfs_path: &VfsPathBuf,
    items: &[ToplevelItem],
) -> Vec<Diagnostic> {
    if !is_built_in_path(&vfs_path.path) {
        return vec![];
    }

    let mut diagnostics = vec![];
    for item in items {
        match item {
            ToplevelItem::Fun(name_sym, fun_info, _) => {
                check_fun_info(name_sym, fun_info, &mut diagnostics);
            }
            ToplevelItem::Method(method_info, _) => {
                if let Some(fun_info) = method_info.fun_info() {
                    check_fun_info(&method_info.name_sym, fun_info, &mut diagnostics);
                }
            }
            _ => {}
        }
    }

    diagnostics
}
