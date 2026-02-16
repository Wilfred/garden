use std::path::Path;

use rustc_hash::FxHashMap;

use crate::checks::type_checker::check_types;
use crate::diagnostics::Autofix;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::parser::ast::{
    Expression, Expression_, IdGenerator, LetDestination, SyntaxId, ToplevelItem, TypeHint,
};
use crate::parser::parse_toplevel_items;
use crate::parser::visitor::Visitor;
use crate::parser::vfs::Vfs;
use crate::pos_to_id::find_item_at;

/// Given a type from the type checker, return a source-level string
/// suitable for a type annotation, or `None` if the type isn't useful
/// to annotate (e.g. `Any` or an error type with no inferred type).
fn type_as_annotation(ty: &Type) -> Option<String> {
    match ty {
        Type::Any => None,
        Type::Error {
            inferred_type: Some(inner),
            ..
        } => type_as_annotation(inner),
        Type::Error { .. } => None,
        Type::TypeParameter(_) => None,
        other => Some(format!("{other}")),
    }
}

/// Return all possible "add type annotation" autofixes for let
/// bindings without explicit type hints.
pub(crate) fn get_type_annotation_actions(src: &str, path: &Path) -> Vec<Autofix> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());
    let summary = check_types(&vfs_path, &items, &env, ns);

    let mut finder = TypeAnnotationFinder {
        id_to_ty: &summary.id_to_ty,
        actions: vec![],
    };

    for item in &items {
        finder.visit_toplevel_item(item);
    }

    finder.actions
}

/// Apply the type annotation action for the let binding at the given
/// offset, returning the modified source.
pub(crate) fn add_type_annotation(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
) -> Result<String, String> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());
    let summary = check_types(&vfs_path, &items, &env, ns);

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    // Find the let expression containing the cursor position.
    for id in ids_at_pos.iter().rev() {
        let syntax_id = id.id();
        if let Some(expr) = find_let_expr(&items, syntax_id) {
            if let Expression_::Let(dest, None, _) = &expr.expr_ {
                if let LetDestination::Symbol(symbol) = dest {
                    if let Some(ty) = summary.id_to_ty.get(&symbol.id) {
                        if let Some(ty_str) = type_as_annotation(ty) {
                            let start = symbol.position.start_offset;
                            let end = symbol.position.end_offset;
                            let new_text = format!("{}: {}", symbol.name.text, ty_str);

                            let mut result = String::new();
                            result.push_str(&src[..start]);
                            result.push_str(&new_text);
                            result.push_str(&src[end..]);
                            return Ok(result);
                        }
                    }
                }
                return Err(
                    "Could not determine type for this let binding.".to_owned()
                );
            }
        }
    }

    Err("No let binding without a type annotation found at this position.".to_owned())
}

/// Find a `Let` expression at or containing the given syntax ID.
fn find_let_expr(items: &[ToplevelItem], id: SyntaxId) -> Option<Expression> {
    let mut finder = LetExprFinder {
        target_id: id,
        result: None,
    };
    for item in items {
        finder.visit_toplevel_item(item);
    }
    finder.result
}

struct LetExprFinder {
    target_id: SyntaxId,
    result: Option<Expression>,
}

impl Visitor for LetExprFinder {
    fn visit_expr(&mut self, expr: &Expression) {
        if self.result.is_some() {
            return;
        }

        if let Expression_::Let(..) = &expr.expr_ {
            if expr.id == self.target_id {
                self.result = Some(expr.clone());
                return;
            }
        }

        self.visit_expr_(&expr.expr_);
    }
}

struct TypeAnnotationFinder<'a> {
    id_to_ty: &'a FxHashMap<SyntaxId, Type>,
    actions: Vec<Autofix>,
}

impl Visitor for TypeAnnotationFinder<'_> {
    fn visit_expr_let(
        &mut self,
        dest: &LetDestination,
        hint: Option<&TypeHint>,
        expr: &Expression,
    ) {
        if hint.is_none() {
            if let LetDestination::Symbol(symbol) = dest {
                if let Some(ty) = self.id_to_ty.get(&symbol.id) {
                    if let Some(ty_str) = type_as_annotation(ty) {
                        self.actions.push(Autofix {
                            description: format!("Add type annotation `: {ty_str}`"),
                            position: symbol.position.clone(),
                            new_text: format!("{}: {}", symbol.name.text, ty_str),
                        });
                    }
                }
            }
        }

        // Continue visiting the RHS expression for nested let bindings.
        self.visit_expr(expr);
    }
}
