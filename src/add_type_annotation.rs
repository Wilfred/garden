use std::path::Path;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::parser::ast::{
    Block, Expression, FunInfo, IdGenerator, LetDestination, Symbol, SyntaxId, TypeHint,
};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::parser::visitor::Visitor;

/// Add a type annotation to the local, parameter or function return
/// type at the selected position.
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
    load_toplevel_items(&items, &mut env, Rc::clone(&ns));
    let summary = check_types(&vfs_path, &items, &env, ns);

    let mut finder = AnnotationFinder {
        offset,
        end_offset,
        id_to_ty: &summary.id_to_ty,
        candidates: vec![],
    };
    for item in &items {
        finder.visit_toplevel_item(item);
    }

    // Prefer the most specific candidate, i.e. the one with the
    // smallest trigger region. A parameter or local symbol is much
    // smaller than a whole function signature, so it wins when the
    // cursor is on it.
    let Some(candidate) = finder
        .candidates
        .into_iter()
        .min_by_key(|c| c.trigger_end - c.trigger_start)
    else {
        return Err(
            "No local, parameter or return type to annotate at the selected position.".to_owned(),
        );
    };

    let mut result = String::new();
    result.push_str(&src[..candidate.insert_offset]);
    result.push_str(&candidate.annotation);
    result.push_str(&src[candidate.insert_offset..]);

    Ok(result)
}

/// A place where a type annotation could be inserted.
struct Candidate {
    /// The region the cursor must fall within for this candidate to
    /// apply.
    trigger_start: usize,
    trigger_end: usize,
    /// The byte offset at which to insert the annotation.
    insert_offset: usize,
    /// The annotation text to insert, e.g. `": Int"`.
    annotation: String,
}

struct AnnotationFinder<'a> {
    offset: usize,
    end_offset: usize,
    id_to_ty: &'a FxHashMap<SyntaxId, Type>,
    candidates: Vec<Candidate>,
}

impl AnnotationFinder<'_> {
    /// Record a candidate for the symbol `sym`, inserting the
    /// annotation immediately after its name.
    fn consider_symbol(&mut self, sym: &Symbol) {
        if !sym.position.contains_region(self.offset, self.end_offset) {
            return;
        }
        if sym.name.is_underscore() {
            return;
        }

        let Some(ty) = self.id_to_ty.get(&sym.id) else {
            return;
        };
        let Some(ty_src) = annotation_src(ty) else {
            return;
        };

        self.candidates.push(Candidate {
            trigger_start: sym.position.start_offset,
            trigger_end: sym.position.end_offset,
            insert_offset: sym.position.end_offset,
            annotation: format!(": {ty_src}"),
        });
    }

    /// Record a candidate for the return type of `fun_info`, if it
    /// doesn't already have a return type hint.
    fn consider_return_type(&mut self, fun_info: &FunInfo) {
        if fun_info.return_hint.is_some() {
            return;
        }

        // The cursor triggers a return type annotation when it's on
        // the function name or anywhere up to the closing parenthesis
        // of the parameter list.
        let header_start = match &fun_info.name_sym {
            Some(name_sym) => name_sym.position.start_offset,
            None => fun_info.params.open_paren.start_offset,
        };
        let header_end = fun_info.params.close_paren.end_offset;
        if !region_contains(header_start, header_end, self.offset, self.end_offset) {
            return;
        }

        let Some(ty) = self.body_return_ty(&fun_info.body) else {
            return;
        };
        let Some(ty_src) = annotation_src(&ty) else {
            return;
        };

        self.candidates.push(Candidate {
            trigger_start: header_start,
            trigger_end: header_end,
            insert_offset: fun_info.params.close_paren.end_offset,
            annotation: format!(": {ty_src}"),
        });
    }

    /// The inferred return type of a function body: the type of its
    /// final expression, or `Unit` for an empty body.
    fn body_return_ty(&self, body: &Block) -> Option<Type> {
        match body.exprs.last() {
            Some(expr) => self.id_to_ty.get(&expr.id).cloned(),
            None => Some(Type::unit()),
        }
    }
}

impl Visitor for AnnotationFinder<'_> {
    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        self.consider_return_type(fun_info);

        for param in &fun_info.params.params {
            if param.hint.is_none() {
                self.consider_symbol(&param.symbol);
            }
        }

        self.visit_fun_info_default(fun_info);
    }

    fn visit_expr_let(
        &mut self,
        dest: &LetDestination,
        hint: Option<&TypeHint>,
        expr: &Expression,
    ) {
        if hint.is_none() {
            if let LetDestination::Symbol(symbol) = dest {
                self.consider_symbol(symbol);
            }
        }

        // Recurse into the bound expression, which may contain
        // lambdas or nested lets.
        self.visit_expr(expr);
    }
}

/// Render `ty` as a type annotation, or `None` if there's no useful
/// annotation to add (an unknown `Any` type, or an unrecoverable type
/// error).
fn annotation_src(ty: &Type) -> Option<String> {
    match ty {
        Type::Error {
            inferred_type: Some(inferred_type),
            ..
        } => annotation_src(inferred_type),
        Type::Error {
            inferred_type: None,
            ..
        } => None,
        Type::Any => None,
        _ if ty.is_no_value() => None,
        _ => Some(ty.to_string()),
    }
}

/// Does the region `start..end` contain the query region, using the
/// same semantics as `Position::contains_region`?
fn region_contains(start: usize, end: usize, q_start: usize, q_end: usize) -> bool {
    if q_start == q_end {
        start <= q_start && q_start < end
    } else {
        start <= q_start && q_end <= end
    }
}
