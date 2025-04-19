use std::{path::Path, rc::Rc};

use crate::parser::{
    ast::{AstId, Expression, Expression_, IdGenerator, Vfs},
    parse_toplevel_items,
};
use crate::{
    env::Env,
    eval::load_toplevel_items,
    pos_to_id::{block_contains_id, find_expr_of_id, find_item_at},
};

pub(crate) fn extract_variable(
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
    let ns = env.current_namespace();
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
        eprintln!("No expression found at this selected position.");
        return;
    };

    // The innermost block-level expression containing the target
    // expression.
    //
    // This is not the block itself, but the expression wihin the
    // block that we want to place our new variable before.
    let mut enclosing_block_level_expr: Option<Expression> = None;
    for id in ids_containing_pos.iter().rev() {
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
                        break;
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
        eprintln!("No expression found for the ID at the selected position.");
        return;
    };
    let var_init_expr = match expr.expr_ {
        Expression_::Parentheses(_, inner_expr, _) => inner_expr,
        _ => Rc::new(expr.clone()),
    };

    let Some(enclosing_block_level_expr) = enclosing_block_level_expr else {
        eprintln!("No enclosing block-level expression found for the selected position.");
        return;
    };

    for item in items {
        let item_pos = item.position();
        if item_pos.contains_offset(offset) {
            // All the items before this one.
            print!("{}", &src[..item_pos.start_offset]);

            print!(
                "{}",
                &src[item_pos.start_offset..enclosing_block_level_expr.position.start_offset]
            );
            print!(
                "let {} = {}\n{}",
                name,
                &src[var_init_expr.position.start_offset..var_init_expr.position.end_offset],
                " ".repeat(enclosing_block_level_expr.position.column)
            );

            print!(
                "{}",
                &src[enclosing_block_level_expr.position.start_offset..expr.position.start_offset]
            );
            print!("{}", name);
            print!("{}", &src[expr.position.end_offset..item_pos.end_offset]);

            // Items after.
            print!("{}", &src[item_pos.end_offset..]);

            break;
        }
    }
}
