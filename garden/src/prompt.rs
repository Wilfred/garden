use owo_colors::OwoColorize;

pub fn prompt_symbol(depth: usize) -> String {
    if depth > 0 {
        return format!("[{}]{} ", depth, ">".green().bold());
    }
    format!("{} ", ">".green().bold())
}
