use std::path::Path;

use crate::parser::ast::{AstId, IdGenerator};
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::pos_to_id::{find_expr_of_id, find_item_at};

pub(crate) fn wrap_in_dbg(
    src: &str,
    path: &Path,
    offset: usize,
    end_offset: usize,
) -> Result<String, String> {
    let mut id_gen = IdGenerator::default();
    let (_vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let ids_at_pos = find_item_at(&items, offset, end_offset);

    let mut expr_id = None;
    for id in ids_at_pos.iter().rev() {
        if let AstId::Expr(syntax_id) = id {
            expr_id = Some(syntax_id);
            break;
        }
    }

    let Some(expr_id) = expr_id else {
        return Err("No expression found at this selected position.".to_owned());
    };
    let Some(expr) = find_expr_of_id(&items, *expr_id) else {
        return Err("No expression found for the ID at this position.".to_owned());
    };

    let mut result = String::new();
    result.push_str(&src[..expr.position.start_offset]);
    result.push_str(&format!(
        "dbg({})",
        &src[expr.position.start_offset..expr.position.end_offset]
    ));
    result.push_str(&src[expr.position.end_offset..]);

    Ok(result)
}
