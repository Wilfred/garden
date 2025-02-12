use std::path::Path;

use garden_lang_parser::{
    ast::{AstId, Expression, Expression_, IdGenerator, SymbolName, SyntaxId},
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
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(id_gen);
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
        let item_pos = &item.1;
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
    let mut visitor = OutsideVarsVisitor {
        env,
        id_to_ty: id_to_ty.clone(),
        vars_outside: vec![],
        vars_seen: FxHashSet::default(),
    };

    // TODO: this does not correctly handle lambdas or let
    // expressions, which can introduce new variables which aren't
    // defined outside of the current scope.
    visitor.visit_expr(expr);
    visitor.vars_outside
}

struct OutsideVarsVisitor<'a> {
    env: &'a Env,
    vars_outside: Vec<(SymbolName, Option<Type>)>,
    vars_seen: FxHashSet<SymbolName>,
    id_to_ty: FxHashMap<SyntaxId, Type>,
}

impl Visitor for OutsideVarsVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expression) {
        self.visit_expr_(&expr.expr_);

        if let Expression_::Variable(symbol) = &expr.expr_ {
            if !self.env.file_scope.contains_key(&symbol.name)
                && !self.vars_seen.contains(&symbol.name)
            {
                self.vars_outside
                    .push((symbol.name.clone(), self.id_to_ty.get(&symbol.id).cloned()));
                self.vars_seen.insert(symbol.name.clone());
            }
        }
    }
}
