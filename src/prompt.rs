use crate::colors::bold_green;

pub fn prompt_symbol(is_stopped: bool) -> String {
    if is_stopped {
        return format!("(stopped) {} ", bold_green(">"));
    }
    format!("{} ", bold_green(">"))
}
