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

    let prev_line_start = match src[..prev_line_end].rfind('\n') {
        Some(i) => i as isize,
        None => -1,
    };
    Some((prev_line_start + column as isize) as usize)
}

/// Given a string that contains `// ^^^`, return the byte offset of
/// the character indicated by the first ^ and the last ^ (i.e. the
/// characters on the line above).
///
/// If multiple `// ^^^` comments are present, the region spans from
/// the first caret of the first comment to the last caret of the last
/// comment. Each caret comment is interpreted relative to the source
/// line immediately above it.
///
/// The returned offsets are positions in the source after caret
/// comments are removed (see `remove_caret`).
pub(crate) fn find_caret_region(src: &str) -> Option<(usize, usize)> {
    let caret_line_re = Regex::new(r"(?m)^[ \t]*//[ \t]*\^+[ \t]*$").unwrap();
    let caret_re = Regex::new(r"// *(\^+)").unwrap();

    let matches: Vec<_> = caret_re.captures_iter(src).collect();
    let first_caps = matches.first()?;
    let last_caps = matches.last()?;

    let first_caret_offset = first_caps.get(1).unwrap().start() as isize;
    let last_caret_offset = last_caps.get(0).unwrap().end() as isize;

    let first_char_offset = project_to_prev_line(src, first_caret_offset)?;
    let last_char_offset = project_to_prev_line(src, last_caret_offset - 1)?;

    // Adjust offsets to account for caret comment lines that will be
    // removed: any whole caret line that falls before a projected
    // offset shifts that offset up by the line's byte length (plus the
    // trailing newline).
    let mut first_shift = 0;
    let mut last_shift = 0;
    for m in caret_line_re.find_iter(src) {
        let line_start = m.start();
        let mut line_end = m.end();
        if src[line_end..].starts_with('\n') {
            line_end += 1;
        }
        let len = line_end - line_start;
        if (line_start as isize) < first_char_offset {
            first_shift += len;
        }
        if (line_start as isize) < last_char_offset {
            last_shift += len;
        }
    }

    Some((
        first_char_offset as usize - first_shift,
        last_char_offset as usize - last_shift,
    ))
}

/// Given an offset in a `// ^...` comment, project it onto the source
/// line immediately above. The returned offset is the byte offset in
/// `src` of the character at the same column on the previous line.
fn project_to_prev_line(src: &str, caret_offset: isize) -> Option<isize> {
    let prev_line_end = src[..caret_offset as usize].rfind('\n')? as isize;
    let prev_line_start = match src[..prev_line_end as usize].rfind('\n') {
        Some(i) => i as isize,
        None => -1,
    };
    Some(prev_line_start + (caret_offset - prev_line_end))
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
