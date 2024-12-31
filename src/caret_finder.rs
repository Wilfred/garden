use regex::Regex;

/// Given a string that contains `// ^`, return the byte offset of the
/// character indicated by the ^ (i.e. the character on the line
/// above).
pub(crate) fn find_caret_offset(src: &str) -> Option<usize> {
    let re = Regex::new("// *\\^").unwrap();

    let comment_match = re.find(src)?;
    let end_comment_offset = comment_match.end() - 1;

    let prev_line_end = src[..end_comment_offset].rfind('\n')?;
    let column = end_comment_offset - prev_line_end;

    let prev_line_start = src[..prev_line_end].rfind('\n').unwrap_or(0);
    Some(prev_line_start + column)
}

/// Given a string that contains `// ^^^`, return the byte offset of
/// the character indicated by the first ^ and the last ^ (i.e. the
/// characters on the line above).
pub(crate) fn find_caret_region(src: &str) -> Option<(usize, usize)> {
    let re = Regex::new(r"// *(\^+)").unwrap();

    let comment_caps = re.captures(src)?;

    let last_caret_offset = comment_caps.get(0).unwrap().end();
    let first_caret_offset = comment_caps.get(1).unwrap().start();

    let prev_line_end = src[..first_caret_offset].rfind('\n')?;
    let prev_line_start = src[..prev_line_end].rfind('\n').unwrap_or(0);

    let first_char_offset = prev_line_start + (first_caret_offset - prev_line_end);
    let last_char_offset = prev_line_start + (last_caret_offset - prev_line_end);

    Some((first_char_offset, last_char_offset))
}

/// Remove the comment containing `//^` to make test output more readable.
pub(crate) fn remove_caret(src: &str) -> String {
    let mut lines = vec![];
    for src_line in src.lines() {
        lines.push(src_line);

        if !src_line.trim_start().starts_with("//") {
            continue;
        }
        if !src_line.trim_end().ends_with("^") {
            continue;
        }
        lines.pop();
    }

    lines.join("\n")
}
