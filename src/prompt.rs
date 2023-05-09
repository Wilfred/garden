use crate::colors::bold_green;

pub fn prompt_symbol(depth: usize) -> String {
    if depth > 0 {
        return format!("[{}]{} ", depth, bold_green(">"));
    }
    format!("{} ", bold_green(">"))
}
