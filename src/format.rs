use std::path::Path;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::{Block, Expression, Expression_, IdGenerator, ToplevelItem};
use crate::parser::lex::lex_between;
use crate::parser::parse_toplevel_items;
use crate::parser::vfs::Vfs;
use crate::parser::visitor::Visitor;

/// Format a Garden source file by fixing indentation, spacing, and type annotations.
///
/// This formatter fixes the indentation of top-level expressions in
/// blocks and function bodies, using 2 spaces per nesting level.
/// Single-line blocks are formatted to have exactly one space after
/// `{` and before `}`.
///
/// It also ensures there's always a space after `:` in type annotations,
/// e.g. `x:Int` becomes `x: Int`.
pub(crate) fn format(src: &str, path: &Path) -> String {
    let mut id_gen = IdGenerator::default();
    let (_vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    // Phase 1 & 2: Collect expression edits via visitor
    let mut visitor = IndentationVisitor {
        current_depth: 0,
        line_edits: vec![],
        processed_lines: FxHashSet::default(),
        span_edits: vec![],
        src: src.to_owned(),
        toplevel_start_lines: vec![],
    };

    for item in &items {
        visitor.visit_toplevel_item(item);
    }

    // Phase 3: Collect comment edits
    collect_comment_edits(
        src,
        &vfs_path,
        &mut visitor.line_edits,
        &mut visitor.processed_lines,
    );

    // Phase 4: Apply span edits first (single-line block spacing)
    let src_after_spans = apply_span_edits(src, &mut visitor.span_edits);

    // Phase 5: Apply indentation edits
    let src_after_indent = apply_indentation_edits(&src_after_spans, &visitor.line_edits);

    // Phase 6: Normalize blank lines
    let src_after_blanks = normalize_blank_lines(&src_after_indent, &visitor.toplevel_start_lines);

    // Phase 7: Fix type annotation spacing
    fix_type_annotation_spacing(&src_after_blanks, &vfs_path)
}

/// Represents an indentation edit for a specific line.
#[derive(Debug)]
struct LineEdit {
    line_number: usize,
    new_indent: usize,
}

/// Represents a span replacement edit for fixing spacing within a line.
#[derive(Debug)]
struct SpanEdit {
    start_offset: usize,
    end_offset: usize,
    replacement: String,
}

/// Visitor that tracks nesting depth and collects indentation edits.
struct IndentationVisitor {
    current_depth: usize,
    line_edits: Vec<LineEdit>,
    processed_lines: FxHashSet<usize>,
    span_edits: Vec<SpanEdit>,
    src: String,
    /// Line numbers where toplevel items start (for blank line enforcement).
    toplevel_start_lines: Vec<usize>,
}

impl IndentationVisitor {
    /// Fix spacing inside a single-line block to ensure exactly one space
    /// after `{` and before `}`.
    fn fix_single_line_block_spacing(&mut self, block: &Block) {
        if block.exprs.is_empty() {
            return;
        }

        let open_end = block.open_brace.end_offset;
        let close_start = block.close_brace.start_offset;

        // Some blocks (e.g., braceless match arms) don't have actual braces,
        // so skip them.
        if open_end >= close_start {
            return;
        }

        // Find first non-whitespace position after `{`
        let after_open = &self.src[open_end..close_start];
        let leading_ws_len = after_open.len() - after_open.trim_start().len();

        // Find last non-whitespace position before `}`
        let trailing_ws_len = after_open.len() - after_open.trim_end().len();

        // Fix leading space (after `{`)
        if leading_ws_len != 1 || !after_open.starts_with(' ') {
            self.span_edits.push(SpanEdit {
                start_offset: open_end,
                end_offset: open_end + leading_ws_len,
                replacement: " ".to_owned(),
            });
        }

        // Fix trailing space (before `}`)
        if trailing_ws_len != 1 || !after_open.ends_with(' ') {
            self.span_edits.push(SpanEdit {
                start_offset: close_start - trailing_ws_len,
                end_offset: close_start,
                replacement: " ".to_owned(),
            });
        }
    }
}

impl Visitor for IndentationVisitor {
    fn visit_toplevel_item(&mut self, item: &ToplevelItem) {
        // Track the starting line of toplevel definitions (not expressions) for blank line enforcement
        let start_line = match item {
            ToplevelItem::Fun(_, fun_info, _) => Some(fun_info.pos.line_number),
            ToplevelItem::Method(method_info, _) => Some(method_info.pos.line_number),
            ToplevelItem::Test(test_info) => Some(test_info.name_sym.position.line_number),
            ToplevelItem::Enum(enum_info) => Some(enum_info.name_sym.position.line_number),
            ToplevelItem::Struct(struct_info) => Some(struct_info.name_sym.position.line_number),
            ToplevelItem::Import(import_info) => Some(import_info.path_pos.line_number),
            // Toplevel expressions and blocks don't require blank lines between them
            ToplevelItem::Expr(_) | ToplevelItem::Block(_) => None,
        };
        if let Some(line) = start_line {
            self.toplevel_start_lines.push(line);
        }

        // Continue with default visitor behavior
        self.visit_toplevel_item_default(item);
    }

    fn visit_toplevel_expr(&mut self, toplevel_expr: &crate::parser::ast::ToplevelExpression) {
        let expr = &toplevel_expr.0;
        let line_num = expr.position.line_number;

        if !self.processed_lines.contains(&line_num) {
            let current_indent = expr.position.column;

            if current_indent != 0 {
                self.line_edits.push(LineEdit {
                    line_number: line_num,
                    new_indent: 0,
                });
            }

            self.processed_lines.insert(line_num);
        }

        // Continue visiting nested expressions (e.g., if blocks)
        self.visit_expr(expr);
    }

    fn visit_block(&mut self, block: &Block) {
        // Handle single-line blocks: enforce exactly one space inside
        if block.open_brace.line_number == block.close_brace.line_number {
            self.fix_single_line_block_spacing(block);
            // Still recurse into nested expressions within the block
            for expr in &block.exprs {
                self.visit_expr(expr);
            }
            return;
        }

        self.current_depth += 1;

        for expr in &block.exprs {
            let line_num = expr.position.line_number;

            // Avoid processing the same line twice
            if !self.processed_lines.contains(&line_num) {
                let target_indent = self.current_depth * 2;
                let current_indent = expr.position.column;

                if current_indent != target_indent {
                    self.line_edits.push(LineEdit {
                        line_number: line_num,
                        new_indent: target_indent,
                    });
                }

                self.processed_lines.insert(line_num);
            }

            self.visit_expr(expr);
        }

        self.current_depth -= 1;

        // Format the closing brace at the same level as the block
        let close_line = block.close_brace.line_number;
        if !self.processed_lines.contains(&close_line) {
            let target_indent = self.current_depth * 2;
            let current_indent = block.close_brace.column;

            if current_indent != target_indent {
                self.line_edits.push(LineEdit {
                    line_number: close_line,
                    new_indent: target_indent,
                });
            }

            self.processed_lines.insert(close_line);
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        // Handle match expressions specially to access the full expression position
        if let Expression_::Match(scrutinee, cases) = &expr.expr_ {
            let match_end_line = expr.position.end_line_number;

            // Visit the scrutinee expression first
            self.visit_expr(scrutinee);

            self.current_depth += 1;

            // Track the last case's end line
            let mut last_case_end_line = 0;

            // Process each match arm
            for (pattern, case_block) in cases {
                // Format the pattern line (e.g., "Some(v) => " or "None =>")
                let pattern_line = pattern.variant_sym.position.line_number;
                if !self.processed_lines.contains(&pattern_line) {
                    let target_indent = self.current_depth * 2;
                    let current_indent = pattern.variant_sym.position.column;

                    if current_indent != target_indent {
                        self.line_edits.push(LineEdit {
                            line_number: pattern_line,
                            new_indent: target_indent,
                        });
                    }

                    self.processed_lines.insert(pattern_line);
                }

                // Visit the block for this match arm
                self.visit_block(case_block);

                // Track the last case's end position
                last_case_end_line = case_block.close_brace.line_number;
            }

            self.current_depth -= 1;

            // Format the closing brace of the match expression
            // The closing brace is on a different line from the last case
            if last_case_end_line < match_end_line
                && !self.processed_lines.contains(&match_end_line)
            {
                let target_indent = self.current_depth * 2;
                self.line_edits.push(LineEdit {
                    line_number: match_end_line,
                    new_indent: target_indent,
                });
                self.processed_lines.insert(match_end_line);
            }
        } else {
            // For all other expressions, use the default visitor behavior
            self.visit_expr_(&expr.expr_);
        }
    }
}

/// Collect indentation edits for comments in the source.
///
/// Comments are not part of the AST, so we need to process them
/// separately from the token stream. Comments are aligned with the
/// indentation of the token they precede.
fn collect_comment_edits(
    src: &str,
    vfs_path: &crate::parser::vfs::VfsPathBuf,
    line_edits: &mut Vec<LineEdit>,
    processed_lines: &mut FxHashSet<usize>,
) {
    let (mut token_stream, _) = lex_between(vfs_path, src, 0, src.len());

    // Build a map of corrected indents for expressions
    let mut corrected_indents: FxHashMap<usize, usize> = FxHashMap::default();
    for edit in line_edits.iter() {
        corrected_indents.insert(edit.line_number, edit.new_indent);
    }

    // Process comments before each token by iterating through the stream
    while let Some(token) = token_stream.pop() {
        for (comment_pos, _comment_text) in &token.preceding_comments {
            let line_num = comment_pos.line_number;

            if !processed_lines.contains(&line_num) {
                // Use the corrected indent of the following token, or its original column
                let mut target_indent = corrected_indents
                    .get(&token.position.line_number)
                    .copied()
                    .unwrap_or(token.position.column);

                // Comments before a closing brace should be indented at the
                // block's inner level, not at the brace's level.
                if token.text == "}" {
                    target_indent += 2;
                }

                let current_indent = comment_pos.column;

                if current_indent != target_indent {
                    line_edits.push(LineEdit {
                        line_number: line_num,
                        new_indent: target_indent,
                    });
                }

                processed_lines.insert(line_num);
            }
        }
    }

    // Process trailing comments (at end of file)
    // Trailing comments should be at depth 0
    for (comment_pos, _) in &token_stream.trailing_comments {
        let line_num = comment_pos.line_number;

        if !processed_lines.contains(&line_num) {
            let current_indent = comment_pos.column;

            if current_indent != 0 {
                line_edits.push(LineEdit {
                    line_number: line_num,
                    new_indent: 0,
                });
            }

            processed_lines.insert(line_num);
        }
    }
}

/// Apply indentation edits to the source while preserving blank lines.
fn apply_indentation_edits(src: &str, line_edits: &[LineEdit]) -> String {
    let lines: Vec<&str> = src.lines().collect();
    let mut result = String::with_capacity(src.len());

    // Create a map for O(1) lookup
    let mut edits_map: FxHashMap<usize, &LineEdit> = FxHashMap::default();
    for edit in line_edits {
        edits_map.insert(edit.line_number, edit);
    }

    for (line_num, line) in lines.iter().enumerate() {
        if let Some(edit) = edits_map.get(&line_num) {
            // Strip existing indentation and add correct amount
            let trimmed = line.trim_start();

            // Empty line stays empty (preserves blank lines)
            if trimmed.is_empty() {
                result.push('\n');
                continue;
            }

            let new_indent = " ".repeat(edit.new_indent);
            result.push_str(&new_indent);
            result.push_str(trimmed);
        } else {
            // Keep line unchanged
            result.push_str(line);
        }

        // Add newline (except for last line if original didn't have one)
        if line_num < lines.len() - 1 {
            result.push('\n');
        }
    }

    // Preserve final newline if original had one
    if src.ends_with('\n') && !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

/// Apply span edits to the source string.
///
/// Span edits are applied in reverse order of start_offset to avoid
/// invalidating later offsets when earlier edits change the string length.
fn apply_span_edits(src: &str, span_edits: &mut [SpanEdit]) -> String {
    if span_edits.is_empty() {
        return src.to_owned();
    }

    // Sort by start_offset in descending order so we can apply from end to start
    span_edits.sort_by(|a, b| b.start_offset.cmp(&a.start_offset));

    let mut result = src.to_owned();
    for edit in span_edits.iter() {
        result.replace_range(edit.start_offset..edit.end_offset, &edit.replacement);
    }

    result
}

/// Normalize blank lines in the formatted source.
///
/// - Between toplevel definitions: exactly one blank line
/// - Inside blocks: at most one blank line between lines
fn normalize_blank_lines(src: &str, toplevel_start_lines: &[usize]) -> String {
    let lines: Vec<&str> = src.lines().collect();
    if lines.is_empty() {
        return src.to_owned();
    }

    let toplevel_lines: FxHashSet<usize> = toplevel_start_lines.iter().copied().collect();
    let mut result = String::with_capacity(src.len());

    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];

        // If this line is blank
        if line.trim().is_empty() {
            // Count consecutive blank lines
            while i < lines.len() && lines[i].trim().is_empty() {
                i += 1;
            }

            // If there's a next line, emit at most one blank line
            if i < lines.len() {
                result.push('\n');
            }
            // If we're at the end, don't add trailing blank lines
            continue;
        }

        // This line is not blank - add it
        result.push_str(line);
        result.push('\n');
        i += 1;

        // Check if the next line (after any blanks) starts a new toplevel definition
        // If so, we need exactly one blank line between them
        if i < lines.len() && !lines[i].trim().is_empty() && toplevel_lines.contains(&i) {
            // Next non-blank line is a toplevel definition, but there's no blank line
            // Insert one blank line
            result.push('\n');
        }
    }

    // Preserve final newline if original had one, but remove if we already have it
    if src.ends_with('\n') && !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

/// Fix spacing after type annotation colons.
///
/// Ensures there's always a space after `:` in type annotations like
/// `x: Int`, `fun foo(a: String): Bool`, etc.
///
/// This function works by:
/// 1. Tokenizing the source
/// 2. Finding `:` tokens that are not part of `::`
/// 3. Checking if they're followed by a type (uppercase letter)
/// 4. Adding a space after the `:` if there isn't one already
fn fix_type_annotation_spacing(src: &str, vfs_path: &crate::parser::vfs::VfsPathBuf) -> String {
    let (token_stream, _) = lex_between(vfs_path, src, 0, src.len());

    // Build a list of character positions where we need to insert spaces
    let mut insert_positions: Vec<usize> = vec![];

    // Convert source to a Vec<char> to handle multi-byte characters correctly
    let chars: Vec<char> = src.chars().collect();

    // Iterate through tokens to find type annotation colons
    let tokens: Vec<_> = {
        let mut ts = token_stream;
        let mut result = vec![];
        while let Some(token) = ts.pop() {
            result.push(token);
        }
        result
    };

    for i in 0..tokens.len() {
        let token = &tokens[i];

        // Check if this is a colon
        if token.text != ":" {
            continue;
        }

        // Skip if this is part of :: (namespace separator)
        if i + 1 < tokens.len() && tokens[i + 1].text == ":" {
            continue;
        }
        if i > 0 && tokens[i - 1].text == ":" {
            continue;
        }

        // Check if the next token looks like a type name
        // Type names start with uppercase letters or are known types
        if i + 1 < tokens.len() {
            let next_token = &tokens[i + 1];
            if is_likely_type_name(next_token.text) {
                // Use the end_offset from the token position (right after the colon)
                let byte_offset_after_colon = token.position.end_offset;

                // Check if there's already a space after the colon
                let char_after_offset = get_char_offset(&chars, byte_offset_after_colon);
                if char_after_offset < chars.len() {
                    let char_after = chars[char_after_offset];
                    if char_after != ' ' && !char_after.is_whitespace() {
                        // Need to insert a space after this colon
                        insert_positions.push(byte_offset_after_colon);
                    }
                }
            }
        }
    }

    // Sort positions in reverse order so we can insert from the end
    insert_positions.sort_unstable();
    insert_positions.reverse();
    insert_positions.dedup();

    // Apply insertions
    let mut result = src.to_owned();
    for pos in insert_positions {
        result.insert(pos, ' ');
    }

    result
}

/// Check if a token text looks like a type name.
/// Type names typically start with an uppercase letter or are tuple types (starting with '(').
fn is_likely_type_name(text: &str) -> bool {
    if text.is_empty() {
        return false;
    }

    // Type names start with uppercase or '(' for tuple types
    let first_char = text.chars().next().unwrap();
    first_char.is_uppercase() || first_char == '('
}

/// Get the character offset (index in Vec<char>) from a byte offset.
fn get_char_offset(chars: &[char], byte_offset: usize) -> usize {
    let mut current_byte = 0;

    for (i, &ch) in chars.iter().enumerate() {
        if current_byte == byte_offset {
            return i;
        }
        current_byte += ch.len_utf8();
    }

    chars.len()
}
