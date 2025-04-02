use std::path::Path;

use garden_lang_parser::{
    ast::{self, AstId, Expression, IdGenerator, SymbolName, SyntaxId, Vfs},
    parse_toplevel_items,
    visitor::Visitor,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    checks::type_checker::check_types,
    env::Env,
    eval::load_toplevel_items,
    garden_type::Type,
    pos_to_id::{find_expr_of_id, find_item_at},
};

pub(crate) fn extract_function(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
    name: &str,
) {
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut vfs, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    load_toplevel_items(&items, &mut env);
    let summary = check_types(&items, &env);

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    let mut expr_id = None;
    for id in ids_at_pos.iter().rev() {
        if let AstId::Expr(syntax_id) = id {
            expr_id = Some(syntax_id);
            break;
        }
    }

    let Some(expr_id) = expr_id else {
        eprintln!("No expression found at this selected position.");
        return;
    };
    let Some(expr) = find_expr_of_id(&items, *expr_id) else {
        eprintln!("No expression found for the ID at the selected position.");
        return;
    };
    let params = locals_outside_expr(&env, &summary.id_to_ty, &expr);

    for item in items {
        let item_pos = &item.0;
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            print!("{}", &src[..item_pos.start_offset]);

            println!(
                "{}",
                extracted_fun_src(src, name, summary.id_to_ty.get(expr_id), &expr, &params)
            );

            // The item, with the expression replaced by a call.
            print!(
                "{}",
                &src[item_pos.start_offset..expr.position.start_offset]
            );

            let arguments_src = params
                .iter()
                .map(|(param, _)| param.name.clone())
                .collect::<Vec<String>>()
                .join(", ");

            print!("{}({})", name, arguments_src);
            print!("{}", &src[expr.position.end_offset..item_pos.end_offset]);

            // Items after.
            print!("{}", &src[item_pos.end_offset..]);

            break;
        }
    }
}

fn extracted_fun_src(
    src: &str,
    name: &str,
    return_ty: Option<&Type>,
    expr: &Expression,
    params: &[(SymbolName, Option<Type>)],
) -> String {
    let return_signature = match return_ty {
        Some(Type::Top | Type::Error(_)) | None => "".to_owned(),
        Some(ty) => format!(": {}", ty),
    };

    let params_signature = params
        .iter()
        .map(|(param, ty)| match ty {
            Some(ty) => format!("{}: {}", param.name, ty),
            None => param.name.to_owned(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "fun {}({}){} {{\n  {}\n}}\n",
        name,
        params_signature,
        return_signature,
        &src[expr.position.start_offset..expr.position.end_offset]
    )
}

fn locals_outside_expr(
    env: &Env,
    id_to_ty: &FxHashMap<SyntaxId, Type>,
    expr: &Expression,
) -> Vec<(SymbolName, Option<Type>)> {
    let mut visitor = FreeVarsVisitor {
        env,
        id_to_ty: id_to_ty.clone(),
        local_bindings: vec![FxHashSet::default()],
        free_vars: vec![],
        free_vars_seen: FxHashSet::default(),
    };

    visitor.visit_expr(expr);
    visitor.free_vars
}

struct FreeVarsVisitor<'a> {
    env: &'a Env,
    local_bindings: Vec<FxHashSet<SymbolName>>,
    /// Variables that are bound outside the current expression.
    free_vars: Vec<(SymbolName, Option<Type>)>,
    /// A hash set of variables in `free_vars`, to avoid duplicates.
    free_vars_seen: FxHashSet<SymbolName>,
    id_to_ty: FxHashMap<SyntaxId, Type>,
}

impl Visitor for FreeVarsVisitor<'_> {
    fn visit_expr_variable(&mut self, symbol: &ast::Symbol) {
        if self.env.file_scope.contains_key(&symbol.name) {
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
            if let Some(payload_dest) = &pattern.payload {
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

impl FreeVarsVisitor<'_> {
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
