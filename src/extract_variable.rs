use std::path::Path;
use std::rc::Rc;

use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::parser::ast::{AstId, Expression, Expression_, IdGenerator};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::pos_to_id::{block_contains_id, find_expr_of_id, find_item_at};

pub(crate) fn extract_variable(
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
    load_toplevel_items(&items, &mut env, ns);

    let ids_containing_pos = find_item_at(&items, offset, end_offset);

    // Find the smallest expression that includes offset and end_offset.
    let mut expr_id = None;
    for id in ids_containing_pos.iter().rev() {
        let AstId::Expr(syntax_id) = id else {
            continue;
        };

        if expr_id.is_some() {
            // Special case: if we're trying to extract an expression
            // wrapped in Parentheses, we want to remove the
            // parentheses too when extracting.
            if let Some(expr) = find_expr_of_id(&items, *syntax_id) {
                if let Expression_::Parentheses(_, _, _) = expr.expr_ {
                    expr_id = Some(syntax_id);
                }
            }

            break;
        }

        expr_id = Some(syntax_id);
    }

    let Some(expr_id) = expr_id else {
        return Err("No expression found at this selected position.".to_owned());
    };

    // The innermost expression that both contains the target expression
    // and is block level, so we can insert our variable before
    // this expression.
    let mut enclosing_block_level_expr: Option<Expression> = None;
    'outer: for id in ids_containing_pos.iter().rev() {
        let AstId::Expr(expr_syntax_id) = id else {
            continue;
        };
        let Some(expr) = find_expr_of_id(&items, *expr_syntax_id) else {
            continue;
        };

        match &expr.expr_ {
            Expression_::FunLiteral(_) => break,
            Expression_::ForIn(_, _, body) if block_contains_id(body, *expr_id) => {
                // If we're trying to extract a variable from an
                // expression inside the body, we want the variable to
                // also be inside the body. If the expression is
                // inside the loop header, we want it before the loop.
                //
                // ```
                // // Extracting `foo()` must be before the loop.
                // for x in foo() {
                //   // Extracting `bar()` can be inside the loop body.
                //   bar()
                // }
                // ```
                break;
            }
            Expression_::Match(_, cases) => {
                for (_, block) in cases {
                    if block_contains_id(block, *expr_id) {
                        break 'outer;
                    }
                }
                enclosing_block_level_expr = Some(expr.clone());
            }
            Expression_::While(_, body) if block_contains_id(body, *expr_id) => break,
            Expression_::If(_, then_block, None) if block_contains_id(then_block, *expr_id) => {
                break
            }
            Expression_::If(_, then_block, Some(else_block))
                if block_contains_id(then_block, *expr_id)
                    || block_contains_id(else_block, *expr_id) =>
            {
                break
            }
            Expression_::Let(_, _, _) => {
                enclosing_block_level_expr = Some(expr.clone());
                break;
            }
            _ => {
                enclosing_block_level_expr = Some(expr.clone());
            }
        }
    }

    let Some(expr) = find_expr_of_id(&items, *expr_id) else {
        return Err("No expression found for the ID at the selected position.".to_owned());
    };
    let var_init_expr = match expr.expr_ {
        Expression_::Parentheses(_, inner_expr, _) => inner_expr,
        _ => Rc::new(expr.clone()),
    };

    let Some(enclosing_block_level_expr) = enclosing_block_level_expr else {
        return Err(
            "No enclosing block-level expression found for the selected position.".to_owned(),
        );
    };

    let mut result = String::new();

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            result.push_str(&src[..item_pos.start_offset]);

            result.push_str(
                &src[item_pos.start_offset..enclosing_block_level_expr.position.start_offset],
            );
            result.push_str(&format!(
                "let {} = {}\n{}",
                name,
                &src[var_init_expr.position.start_offset..var_init_expr.position.end_offset],
                " ".repeat(enclosing_block_level_expr.position.column)
            ));

            result.push_str(
                &src[enclosing_block_level_expr.position.start_offset..expr.position.start_offset],
            );
            result.push_str(name);
            result.push_str(&src[expr.position.end_offset..item_pos.end_offset]);

            // Items after.
            result.push_str(&src[item_pos.end_offset..]);

            break;
        }
    }

    Ok(result)
}
