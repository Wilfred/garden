use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{
    self, AstId, Block, Expression, IdGenerator, SymbolName, SyntaxId, ToplevelItem,
};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::parser::visitor::Visitor;
use crate::pos_to_id::{find_expr_of_id, find_item_at};

pub(crate) fn extract_function(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
    name: &str,
) -> Result<String, String> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);

    load_toplevel_items(&items, &mut env, Rc::clone(&ns));
    let summary = check_types(&vfs_path, &items, &env, Rc::clone(&ns));

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    let mut expr_id = None;
    for id in ids_at_pos.iter().rev() {
        if let AstId::Expr(syntax_id) = id {
            expr_id = Some(syntax_id);
            break;
        }
    }

    if let Some(expr_id) = expr_id {
        let Some(expr) = find_expr_of_id(&items, *expr_id) else {
            return Err("No expression found for the ID at the selected position.".to_owned());
        };
        return extract_single_expr(
            src,
            &items,
            ns,
            &summary.id_to_ty,
            offset,
            *expr_id,
            &expr,
            name,
        );
    }

    if let Some(exprs) = find_block_selection(&items, offset, end_offset) {
        return extract_exprs(src, &items, ns, &summary.id_to_ty, &exprs, name);
    }

    Err("No expression found at this selected position.".to_owned())
}

fn extract_single_expr(
    src: &str,
    items: &[ToplevelItem],
    ns: Rc<RefCell<NamespaceInfo>>,
    id_to_ty: &FxHashMap<SyntaxId, Type>,
    offset: usize,
    expr_id: SyntaxId,
    expr: &Expression,
    name: &str,
) -> Result<String, String> {
    let params = locals_outside_exprs(ns, id_to_ty, std::slice::from_ref(expr));

    let mut result = String::new();

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            result.push_str(&src[..item_pos.start_offset]);

            result.push_str(&format!(
                "{}\n",
                extracted_fun_src(
                    src,
                    name,
                    id_to_ty.get(&expr_id),
                    expr.position.start_offset,
                    expr.position.end_offset,
                    &params,
                )
            ));

            // The item, with the expression replaced by a call.
            result.push_str(&src[item_pos.start_offset..expr.position.start_offset]);

            let arguments_src = params
                .iter()
                .map(|(param, _)| param.text.clone())
                .collect::<Vec<String>>()
                .join(", ");

            result.push_str(&format!("{name}({arguments_src})"));
            result.push_str(&src[expr.position.end_offset..item_pos.end_offset]);

            // Items after.
            result.push_str(&src[item_pos.end_offset..]);

            break;
        }
    }

    Ok(result)
}

fn extract_exprs(
    src: &str,
    items: &[ToplevelItem],
    ns: Rc<RefCell<NamespaceInfo>>,
    id_to_ty: &FxHashMap<SyntaxId, Type>,
    exprs: &[Rc<Expression>],
    name: &str,
) -> Result<String, String> {
    let first = exprs.first().expect("Selection must be non-empty");
    let last = exprs.last().expect("Selection must be non-empty");

    let body_start = first.position.start_offset;
    let body_end = last.position.end_offset;

    let exprs_owned: Vec<Expression> = exprs.iter().map(|e| (**e).clone()).collect();
    let params = locals_outside_exprs(ns, id_to_ty, &exprs_owned);

    let return_ty = id_to_ty.get(&last.id);

    let mut result = String::new();

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(body_start) {
            result.push_str(&src[..item_pos.start_offset]);

            result.push_str(&format!(
                "{}\n",
                extracted_fun_src(src, name, return_ty, body_start, body_end, &params),
            ));

            result.push_str(&src[item_pos.start_offset..body_start]);

            let arguments_src = params
                .iter()
                .map(|(param, _)| param.text.clone())
                .collect::<Vec<String>>()
                .join(", ");

            result.push_str(&format!("{name}({arguments_src})"));
            result.push_str(&src[body_end..item_pos.end_offset]);

            result.push_str(&src[item_pos.end_offset..]);

            break;
        }
    }

    Ok(result)
}

fn extracted_fun_src(
    src: &str,
    name: &str,
    return_ty: Option<&Type>,
    body_start: usize,
    body_end: usize,
    params: &[(SymbolName, Option<Type>)],
) -> String {
    let return_signature = match return_ty {
        Some(Type::Any) | None => "".to_owned(),
        Some(Type::Error { inferred_type, .. }) => match inferred_type {
            Some(ty) => format!(": {ty}"),
            None => "".to_owned(),
        },
        Some(ty) => format!(": {ty}"),
    };

    let params_signature = params
        .iter()
        .map(|(param, ty)| match ty {
            Some(ty) => format!("{}: {}", param.text, ty),
            None => param.text.to_owned(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "fun {}({}){} {{\n  {}\n}}\n",
        name,
        params_signature,
        return_signature,
        &src[body_start..body_end]
    )
}

fn locals_outside_exprs(
    namespace: Rc<RefCell<NamespaceInfo>>,
    id_to_ty: &FxHashMap<SyntaxId, Type>,
    exprs: &[Expression],
) -> Vec<(SymbolName, Option<Type>)> {
    let mut visitor = FreeVarsVisitor {
        namespace,
        id_to_ty: id_to_ty.clone(),
        local_bindings: vec![FxHashSet::default()],
        free_vars: vec![],
        free_vars_seen: FxHashSet::default(),
    };

    for expr in exprs {
        visitor.visit_expr(expr);
    }
    visitor.free_vars
}

struct FreeVarsVisitor {
    namespace: Rc<RefCell<NamespaceInfo>>,
    local_bindings: Vec<FxHashSet<SymbolName>>,
    /// Variables that are bound outside the current expression.
    free_vars: Vec<(SymbolName, Option<Type>)>,
    /// A hash set of variables in `free_vars`, to avoid duplicates.
    free_vars_seen: FxHashSet<SymbolName>,
    id_to_ty: FxHashMap<SyntaxId, Type>,
}

impl Visitor for FreeVarsVisitor {
    fn visit_expr_variable(&mut self, symbol: &ast::Symbol) {
        if self.namespace.borrow().values.contains_key(&symbol.name) {
            return;
        }
        if self.free_vars_seen.contains(&symbol.name) {
            return;
        }
        for scope in self.local_bindings.iter().rev() {
            if scope.contains(&symbol.name) {
                return;
            }
        }

        self.free_vars
            .push((symbol.name.clone(), self.id_to_ty.get(&symbol.id).cloned()));
        self.free_vars_seen.insert(symbol.name.clone());
    }

    fn visit_expr_match(&mut self, scrutinee: &Expression, cases: &[(ast::Pattern, ast::Block)]) {
        self.visit_expr(scrutinee);

        for (pattern, block) in cases {
            self.local_bindings.push(FxHashSet::default());
            if let ast::Pattern::Variant {
                payload: Some(payload_dest),
                ..
            } = pattern
            {
                self.insert_dest_bindings(payload_dest);
            }

            self.visit_block(block);
            self.local_bindings.pop();
        }
    }

    fn visit_expr_for_in(
        &mut self,
        dest: &ast::LetDestination,
        expr: &Expression,
        body: &ast::Block,
    ) {
        self.visit_expr(expr);

        let mut block_bindings = FxHashSet::default();
        match dest {
            ast::LetDestination::Symbol(symbol) => {
                block_bindings.insert(symbol.name.clone());
            }
            ast::LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    block_bindings.insert(symbol.name.clone());
                }
            }
        }

        self.local_bindings.push(block_bindings);
        self.visit_block(body);
        self.local_bindings.pop();
    }

    fn visit_expr_let(
        &mut self,
        dest: &ast::LetDestination,
        _: Option<&ast::TypeHint>,
        expr: &Expression,
    ) {
        self.visit_expr(expr);
        self.insert_dest_bindings(dest);
    }

    fn visit_expr_fun_literal(&mut self, fun_info: &ast::FunInfo) {
        let mut block_bindings = FxHashSet::default();
        for param in &fun_info.params.params {
            block_bindings.insert(param.symbol.name.clone());
        }

        self.local_bindings.push(block_bindings);
        self.visit_block(&fun_info.body);
        self.local_bindings.pop();
    }
}

/// Find a contiguous slice of sibling expressions inside the innermost
/// block that match the selection `[offset, end_offset]`.
///
/// Returns `None` if there is no such block, if no expression is fully
/// contained in the selection, or if the selection partially overlaps an
/// expression in the block (which would yield a broken extraction).
fn find_block_selection(
    items: &[ToplevelItem],
    offset: usize,
    end_offset: usize,
) -> Option<Vec<Rc<Expression>>> {
    let mut visitor = BlockSelectionFinder {
        offset,
        end_offset,
        found: None,
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.found
}

struct BlockSelectionFinder {
    offset: usize,
    end_offset: usize,
    /// The innermost matching block's selected expressions.
    found: Option<Vec<Rc<Expression>>>,
}

impl Visitor for BlockSelectionFinder {
    fn visit_block(&mut self, block: &Block) {
        // Recurse first so the innermost block wins.
        for expr in &block.exprs {
            self.visit_expr(expr);
        }

        if self.found.is_some() {
            return;
        }

        // The selection must fall strictly inside this block's braces.
        if self.offset < block.open_brace.end_offset
            || self.end_offset > block.close_brace.start_offset
        {
            return;
        }

        let mut selected: Vec<Rc<Expression>> = vec![];
        for expr in &block.exprs {
            let pos = &expr.position;
            let inside = self.offset <= pos.start_offset && pos.end_offset <= self.end_offset;
            let partial = pos.start_offset < self.end_offset && self.offset < pos.end_offset;
            if inside {
                selected.push(Rc::clone(expr));
            } else if partial {
                // Selection cuts through an expression; reject.
                return;
            }
        }

        if !selected.is_empty() {
            self.found = Some(selected);
        }
    }
}

impl FreeVarsVisitor {
    fn insert_dest_bindings(&mut self, dest: &ast::LetDestination) {
        let block_bindings = self
            .local_bindings
            .last_mut()
            .expect("Should never be empty");
        match dest {
            ast::LetDestination::Symbol(symbol) => {
                block_bindings.insert(symbol.name.clone());
            }
            ast::LetDestination::Destructure(symbols) => {
                for symbol in symbols {
                    block_bindings.insert(symbol.name.clone());
                }
            }
        }
    }
}
