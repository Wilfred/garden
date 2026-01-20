//! Check code examples in markdown files and .gdn files.

use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

use crate::diagnostics::{format_exception_with_stack, Severity};
use crate::env::Env;
use crate::eval::{eval_toplevel_items, EvalError, Session, StdoutMode};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::position::Position;
use crate::parser::vfs::{to_abs_path, to_project_relative, Vfs};
use crate::parser::{parse_toplevel_items_from_span, ParseError};
use crate::BAD_CLI_REQUEST_EXIT_CODE;

/// A code block extracted from markdown.
struct CodeBlock {
    /// The byte offset where the code block content starts in the markdown source
    start_offset: usize,
    /// The byte offset where the code block content ends in the markdown source
    end_offset: usize,
}

/// Result of evaluating an expression in a code block.
struct ExpressionResult {
    value: crate::values::Value,
    expected_comment: Option<String>,
    /// Position in the source file
    position: crate::parser::position::Position,
}

/// Result of checking an expression value against an expected value.
enum CheckResult {
    Passed,
    Failed { expected: String, got: String },
    ExpectedException,
    MissingException { got: String },
    NoAssertion,
}

/// Extract code blocks from markdown source.
fn extract_code_blocks(markdown: &str) -> Vec<CodeBlock> {
    let parser = Parser::new(markdown).into_offset_iter();
    let mut blocks = vec![];
    let mut in_code_block = false;
    let mut current_content = String::new();
    let mut content_start_offset = 0;
    let mut content_end_offset = 0;
    let mut should_skip = false;
    let mut seen_first_text = false;

    for (event, range) in parser {
        match event {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(info))) => {
                in_code_block = true;
                current_content.clear();
                seen_first_text = false;

                let info_str = info.as_ref();
                should_skip = info_str.contains("nocheck") || !is_garden_block(info_str);
            }
            Event::End(TagEnd::CodeBlock) => {
                if in_code_block && !should_skip && !current_content.is_empty() {
                    blocks.push(CodeBlock {
                        start_offset: content_start_offset,
                        end_offset: content_end_offset,
                    });
                }
                in_code_block = false;
                should_skip = false;
            }
            Event::Text(text) if in_code_block => {
                if !seen_first_text {
                    content_start_offset = range.start;
                    seen_first_text = true;
                }
                current_content.push_str(text.as_ref());
                content_end_offset = range.end;
            }
            Event::SoftBreak | Event::HardBreak => {
                if in_code_block {
                    current_content.push('\n');
                    content_end_offset += 1;
                }
            }
            _ => {}
        }
    }

    blocks
}

/// Adjust a position that may have been created with Position::todo() to use
/// the correct offset and line number from the markdown source.
fn adjust_todo_position(pos: &Position, markdown_src: &str, block_start_offset: usize) -> Position {
    // If this is a todo position (all zeros), calculate the correct position
    // from the block start offset
    if pos.start_offset == 0 && pos.end_offset == 0 && pos.line_number == 0 {
        let line_number = markdown_src[..block_start_offset].matches('\n').count();
        let column = markdown_src[..block_start_offset]
            .rfind('\n')
            .map_or(block_start_offset, |last_newline| {
                block_start_offset - last_newline - 1
            });

        Position {
            start_offset: block_start_offset,
            end_offset: block_start_offset,
            line_number,
            end_line_number: line_number,
            column,
            end_column: column,
            path: pos.path.clone(),
            vfs_path: pos.vfs_path.clone(),
        }
    } else {
        pos.clone()
    }
}

fn is_garden_block(info: &str) -> bool {
    let mut words = info.trim().split(" ");
    if let Some(first_word) = words.next() {
        // Support both 'garden' and 'garden title:"foo"'.
        first_word == "garden"
    } else {
        // Assume Garden if no language specified.
        true
    }
}

/// Extract the expected value from a //-> comment on the same line as an expression.
fn extract_arrow_comment(src: &str, end_offset: usize) -> Option<String> {
    // Find the rest of the line after the expression
    let remaining = &src[end_offset..];
    if let Some(newline_pos) = remaining.find('\n') {
        let line = &remaining[..newline_pos];
        if let Some(arrow_pos) = line.find("//->") {
            let comment = &line[arrow_pos + 4..];
            return Some(comment.trim().to_owned());
        }
    } else {
        // No newline, check the rest of the file
        if let Some(arrow_pos) = remaining.find("//->") {
            let comment = &remaining[arrow_pos + 4..];
            return Some(comment.trim().to_owned());
        }
    }
    None
}

/// Evaluate expressions in a code block and return results with expected values.
fn eval_code_block(
    block: &CodeBlock,
    markdown_src: &str,
    markdown_path: &Path,
    block_idx: usize,
    interrupted: Arc<AtomicBool>,
    project_root: &Path,
) -> Result<Vec<ExpressionResult>, String> {
    // Create isolated environment for this block
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();

    // Use a synthetic path for the VFS entry so namespace lookup works correctly
    let synthetic_path = markdown_path
        .with_file_name(format!("block_{}", block_idx + 1))
        .with_extension("gdn");

    let vfs_path = vfs.insert(Rc::new(synthetic_path.clone()), markdown_src.to_owned());

    // Parse the code block using offsets into the markdown source
    let (items, parse_errors) = parse_toplevel_items_from_span(
        &vfs_path,
        markdown_src,
        &mut id_gen,
        block.start_offset,
        block.end_offset,
    );

    if !parse_errors.is_empty() {
        let mut error_messages = vec![];
        for error in parse_errors {
            match error {
                ParseError::Invalid {
                    position,
                    message,
                    notes: _,
                } => {
                    let adjusted_pos =
                        adjust_todo_position(&position, markdown_src, block.start_offset);
                    // Use the markdown path, not the synthetic path
                    let relative_path = to_project_relative(markdown_path, project_root);
                    error_messages.push(format!(
                        "{}:{}: {}",
                        relative_path.display(),
                        adjusted_pos.line_number + 1, // Convert 0-indexed to 1-indexed
                        message.as_string()
                    ));
                }
                ParseError::Incomplete { message, position } => {
                    let adjusted_pos =
                        adjust_todo_position(&position, markdown_src, block.start_offset);
                    // Use the markdown path, not the synthetic path
                    let relative_path = to_project_relative(markdown_path, project_root);
                    error_messages.push(format!(
                        "{}:{}: Parse error (incomplete): {}",
                        relative_path.display(),
                        adjusted_pos.line_number + 1, // Convert 0-indexed to 1-indexed
                        message.as_string()
                    ));
                }
            }
        }
        return Err(error_messages.join("\n"));
    }

    // Create environment
    let mut env = Env::new(id_gen, vfs);

    // Set up namespace for this code block (using the same synthetic path as VFS)
    let ns = env.get_or_create_namespace(&synthetic_path);

    // Load all items (definitions and expressions) into the environment
    // This registers functions, types, etc. and runs type checking
    let (load_diagnostics, _) = crate::eval::load_toplevel_items(&items, &mut env, ns);

    // Check for errors during loading
    for diagnostic in load_diagnostics {
        if matches!(diagnostic.severity, Severity::Error) {
            return Err(diagnostic.message.as_string());
        }
    }

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs: false,
        pretty_print_json: false,
    };

    // Separate definitions from expressions
    let mut defs = vec![];
    let mut exprs = vec![];
    for item in items.iter() {
        match item {
            ToplevelItem::Expr(expr) => {
                exprs.push(expr.clone());
            }
            _ => {
                defs.push(item.clone());
            }
        }
    }

    match eval_toplevel_items(&vfs_path, &defs, &mut env, &session) {
        Ok(_) => {}
        Err(EvalError::Exception(exception_info)) => {
            let error_msg = format_exception_with_stack(
                &exception_info.message,
                &exception_info.position,
                &env.stack.0,
                &env.vfs,
                &env.project_root,
            );
            return Err(error_msg);
        }
        Err(e) => {
            return Err(format!("Evaluation error: {:?}", e));
        }
    }

    // Now evaluate each expression individually so we can get its value
    let mut results = vec![];
    for expr in exprs {
        // Check for //-> comment
        let expected_comment = extract_arrow_comment(markdown_src, expr.0.position.end_offset);

        // Evaluate this single expression
        match eval_toplevel_items(
            &vfs_path,
            &[ToplevelItem::Expr(expr.clone())],
            &mut env,
            &session,
        ) {
            Ok(summary) => {
                if !summary.values.is_empty() {
                    let value = summary.values[0].clone();
                    results.push(ExpressionResult {
                        value,
                        expected_comment,
                        position: expr.0.position.clone(),
                    });
                }
            }
            Err(EvalError::Exception(_)) => {
                // Check if exception was expected
                if let Some(comment) = &expected_comment {
                    if comment.trim() == "*exception*" {
                        // Exception was expected, this is fine
                        results.push(ExpressionResult {
                            value: crate::values::Value::unit(),
                            expected_comment: expected_comment.clone(),
                            position: expr.0.position.clone(),
                        });
                        continue;
                    }
                }
                // Exception was not expected, propagate error
                // Use the markdown path, not the synthetic path
                let relative_path = to_project_relative(markdown_path, project_root);
                return Err(format!(
                    "{}:{}: Unexpected exception",
                    relative_path.display(),
                    expr.0.position.line_number + 1, // Convert 0-indexed to 1-indexed
                ));
            }
            Err(e) => {
                return Err(format!("Evaluation error: {:?}", e));
            }
        }
    }

    Ok(results)
}

/// Check if an expression's value matches the expected value.
fn check_expression(value: &str, expected: Option<&str>) -> CheckResult {
    match expected {
        Some(expected) => {
            let expected = expected.trim();
            if expected == "*exception*" {
                // Exception was expected - check if it was unit (meaning exception occurred)
                // or a real value (meaning no exception occurred)
                if value == "Unit" {
                    CheckResult::ExpectedException
                } else {
                    CheckResult::MissingException {
                        got: value.to_owned(),
                    }
                }
            } else if value == expected {
                CheckResult::Passed
            } else {
                CheckResult::Failed {
                    expected: expected.to_owned(),
                    got: value.to_owned(),
                }
            }
        }
        None => CheckResult::NoAssertion,
    }
}

fn run_blocks_in_file(file_path: &Path, interrupted: Arc<AtomicBool>, project_root: &Path) -> (bool, usize) {
    let src = match std::fs::read_to_string(file_path) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error reading {}: {}", file_path.display(), e);
            return (false, 0);
        }
    };

    let is_gdn = file_path.extension().is_some_and(|ext| ext == "gdn");

    // Extract code blocks and the source to use for parsing
    let (code_blocks, parse_src) = if is_gdn {
        // For .gdn files, extract doc comments as markdown
        let mut doc_markdown = String::new();
        for line in src.lines() {
            let trimmed = line.trim_start();
            if let Some(comment_content) = trimmed.strip_prefix("///") {
                doc_markdown.push_str(comment_content);
                doc_markdown.push('\n');
            }
        }

        // Extract code blocks from the markdown
        let blocks = extract_code_blocks(&doc_markdown);
        (blocks, doc_markdown)
    } else {
        // For markdown files, use the original source
        let blocks = extract_code_blocks(&src);
        (blocks, src.clone())
    };

    let block_count = code_blocks.len();

    if code_blocks.is_empty() {
        return (true, 0); // No code blocks is fine
    }

    // Track failures
    let mut had_error = false;

    // Create a temporary env for displaying values
    let display_env = Env::new(IdGenerator::default(), Vfs::default());

    for (block_idx, block) in code_blocks.iter().enumerate() {
        // Evaluate the code block using the appropriate source
        match eval_code_block(
            block,
            &parse_src,
            file_path,
            block_idx,
            interrupted.clone(),
            project_root,
        ) {
            Ok(results) => {
                for result in results {
                    // Get string representation of value
                    let value_str = result.value.display(&display_env);

                    // Check against expected value
                    let check_result =
                        check_expression(&value_str, result.expected_comment.as_deref());

                    match check_result {
                        CheckResult::Passed => {
                            // Silent on success
                        }
                        CheckResult::ExpectedException => {
                            // Expected exception occurred, silent on success
                        }
                        CheckResult::Failed { expected, got } => {
                            had_error = true;
                            // Use the actual path
                            let relative_path = to_project_relative(file_path, project_root);
                            eprintln!(
                                "{}:{}: Check failed",
                                relative_path.display(),
                                result.position.line_number + 1 // Convert 0-indexed to 1-indexed
                            );
                            eprintln!("  Expected: {}", expected);
                            eprintln!("  Got:      {}", got);
                            eprintln!();
                        }
                        CheckResult::MissingException { got } => {
                            had_error = true;
                            // Use the actual path
                            let relative_path = to_project_relative(file_path, project_root);
                            eprintln!(
                                "{}:{}: Expected exception but expression succeeded",
                                relative_path.display(),
                                result.position.line_number + 1 // Convert 0-indexed to 1-indexed
                            );
                            eprintln!("  Got:      {}", got);
                            eprintln!();
                        }
                        CheckResult::NoAssertion => {
                            // Don't print values by default (less verbose)
                        }
                    }
                }
            }
            Err(error) => {
                had_error = true;
                // Errors from eval_code_block already include path and line number
                eprintln!("{}", error);
            }
        }
    }

    (!had_error, block_count)
}

pub(crate) fn run_code_blocks(paths: &[std::path::PathBuf], interrupted: Arc<AtomicBool>) {
    if paths.is_empty() {
        eprintln!("No files specified");
        std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
    }

    let project_root = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("/"));

    let mut all_success = true;
    let mut total_files = 0;
    let mut total_blocks = 0;

    for path in paths {
        let abs_path = to_abs_path(path);
        let (success, block_count) = run_blocks_in_file(&abs_path, interrupted.clone(), &project_root);
        if !success {
            all_success = false;
        }
        total_files += 1;
        total_blocks += block_count;
    }

    // Print summary
    let block_word = if total_blocks == 1 { "block" } else { "blocks" };
    let file_word = if total_files == 1 { "file" } else { "files" };
    eprintln!("Checked {} {} in {} {}.", total_blocks, block_word, total_files, file_word);

    if !all_success {
        std::process::exit(1);
    }
}
