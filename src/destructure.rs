use std::path::Path;

use garden_lang_parser::ast::{AstId, VariantInfo};
use garden_lang_parser::{ast::IdGenerator, parse_toplevel_items};
use line_numbers::LinePositions;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::{Type, TypeDefKind};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;

pub(crate) fn destructure(src: &str, path: &Path, offset: usize, end_offset: usize) {
    let mut id_gen = IdGenerator::default();
    let (items, _errors) = parse_toplevel_items(path, src, &mut id_gen);

    let mut env = Env::new(&mut id_gen);
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
        eprintln!("No expression found for the ID at this position.");
        return;
    };

    let Some(ty) = summary.id_to_ty.get(expr_id) else {
        eprintln!("Could not find the type of the expression at this position.");
        return;
    };

    let Some(variants) = enum_variants(&env, ty) else {
        eprintln!("No expression found for the ID at the selected position.");
        return;
    };

    let line_positions = LinePositions::from(src);

    let line = src
        .lines()
        .nth(line_positions.from_offset(offset).as_usize())
        .unwrap();
    let indent = line.len() - line.trim_start().len();
    let indent_str = " ".repeat(indent);

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            print!("{}", &src[..item_pos.start_offset]);

            // The item, with the expression replaced by a call.
            print!(
                "{}",
                &src[item_pos.start_offset..expr.position.start_offset]
            );

            println!(
                "match {} {{",
                &src[expr.position.start_offset..expr.position.end_offset]
            );

            for variant in variants {
                println!(
                    "{}  {}{} => {{}}",
                    indent_str,
                    &variant.name_sym.name,
                    if variant.payload_hint.is_some() {
                        "(_)"
                    } else {
                        ""
                    }
                );
            }

            print!("{}}}", indent_str);

            // Items after.
            print!("{}", &src[expr.position.end_offset..]);

            break;
        }
    }
}

/// If `ty` is an enum type, return its variants.
fn enum_variants(env: &Env, ty: &Type) -> Option<Vec<VariantInfo>> {
    let Type::UserDefined { kind, name, .. } = ty else {
        return None;
    };
    match kind {
        TypeDefKind::Enum => {}
        TypeDefKind::Struct => return None,
    }

    let type_def = env.types.get(name)?;
    let TypeDef::Enum(enum_info) = type_def else {
        return None;
    };

    Some(enum_info.variants.clone())
}
