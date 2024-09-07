use regex::Regex;

/// Given a string that contains `// ^`, return the byte offset of the
/// character indicated by the ^ (i.e. the character on the line
/// above).
pub(crate) fn find_caret_offset(src: &str) -> Option<usize> {
    let re = Regex::new("// *\\^").unwrap();

    let comment_match = re.find(src)?;
    let end_comment_offset = comment_match.end();

    let prev_line_end = src[..end_comment_offset].rfind('\n')?;
    let column = end_comment_offset - prev_line_end;

    let prev_line_start = src[..prev_line_end].rfind('\n').unwrap_or(0);
    Some(prev_line_start + column)
}
