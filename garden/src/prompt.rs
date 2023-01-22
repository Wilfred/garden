use owo_colors::OwoColorize;
use std::io::Write;

pub fn prompt_symbol(depth: usize) {
    if depth > 0 {
        print!("[{}]", depth);
    }
    print!("{} ", ">".green().bold());
    std::io::stdout().flush().unwrap();
}
