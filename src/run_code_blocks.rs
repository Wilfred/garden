//! Check code examples in markdown files and .gdn files.

use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

use crate::diagnostics::{format_diagnostic, format_exception_with_stack, Diagnostic};
use crate::env::Env;
use crate::eval::{eval_toplevel_items, load_toplevel_items, EvalError, Session, StdoutMode};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::position::Position;
use crate::parser::vfs::{to_abs_path, to_project_relative, Vfs};
use crate::parser::{parse_toplevel_items, parse_toplevel_items_from_span, ParseError};
use crate::values::Value;
use crate::BAD_CLI_REQUEST_EXIT_CODE;

/// A code block extracted from markdown.
#[derive(Debug)]
struct CodeBlock {
    /// The byte offset where the code block content starts in the
    /// markdown source.
    start_offset: usize,
    /// The byte offset where the code block content ends in the
    /// markdown source.
    end_offset: usize,
}

/// Result of evaluating an expression in a code block.
#[derive(Debug)]
struct ExpressionResult {
    value: Value,
    expected_comment: Option<String>,
    position: Position,
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

fn is_garden_block(info: &str) -> bool {
    let info = info.trim();
    if info.is_empty() {
        return true;
    }

    let mut words = info.split(" ");
    words.next() == Some("garden")
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
    src: &str,
    markdown_src: &str,
    file_path: &Path,
    interrupted: Arc<AtomicBool>,
    project_root: &Path,
) -> Result<Vec<ExpressionResult>, String> {
    let is_gdn = match file_path.extension() {
        Some(ext) => ext == "gdn",
        None => false,
    };

    let mut vfs = Vfs::default();

    let src_vfs_file_path = file_path.to_path_buf();
    let src_vfs_path = vfs.insert(Rc::new(src_vfs_file_path.clone()), src.to_owned());

    // We need a separate source file because the original source file
    // has all the code blocks in comments.
    let md_vfs_path = vfs.insert(Rc::new(src_vfs_file_path.clone()), markdown_src.to_owned());

    let mut file_env = Env::new(IdGenerator::default(), vfs);
    let ns = file_env.get_or_create_namespace(&src_vfs_file_path);

    if is_gdn {
        // Parse and load the definitions in the .gdn file, outside of
        // the blocks.
        let (file_items, file_parse_errors) =
            parse_toplevel_items(&src_vfs_path, src, &mut file_env.id_gen);

        if !file_parse_errors.is_empty() {
            for error in file_parse_errors {
                eprintln!("{}", error.message().as_string());
            }

            return Err("Parsing failed.".to_owned());
        }

        let (file_load_diagnostics, _) =
            load_toplevel_items(&file_items, &mut file_env, ns.clone());
        print_diagnostics(project_root, &file_env.vfs, &file_load_diagnostics);
    }

    // Switch the current namespace to the current file. Otherwise, we
    // end up not being able to call functions defined in the current
    // file.
    let stack_frame = file_env
        .stack
        .0
        .first_mut()
        .expect("Should always have at least one frame");
    stack_frame.namespace = ns.clone();

    let mut block_env = file_env.clone();

    // Parse the code block using offsets into the markdown source
    let (items, parse_errors) = parse_toplevel_items_from_span(
        &md_vfs_path,
        markdown_src,
        &mut block_env.id_gen,
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
                    let adjusted_pos = position.clone();
                    // Use the actual file path
                    let relative_path = to_project_relative(file_path, project_root);
                    error_messages.push(format!(
                        "{}:{}: {}",
                        relative_path.display(),
                        adjusted_pos.line_number + 1, // Convert 0-indexed to 1-indexed
                        message.as_string()
                    ));
                }
                ParseError::Incomplete { message, position } => {
                    let adjusted_pos = position.clone();
                    // Use the actual file path
                    let relative_path = to_project_relative(file_path, project_root);
                    // TODO: shouldn't this be using existing diag printing?
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

    let (load_diagnostics, _) = load_toplevel_items(&items, &mut block_env, ns.clone());
    print_diagnostics(project_root, &block_env.vfs, &load_diagnostics);

    let session = Session {
        interrupted,
        stdout_mode: StdoutMode::DoNotWrite,
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

    let (load_diagnostics, _) = load_toplevel_items(&items, &mut block_env, ns.clone());
    print_diagnostics(project_root, &block_env.vfs, &load_diagnostics);

    // Evaluate definitions within the block.
    match eval_toplevel_items(&md_vfs_path, &defs, &mut block_env, &session) {
        Ok(_) => {}
        Err(EvalError::Exception(exception_info)) => {
            let error_msg = format_exception_with_stack(
                &exception_info.message,
                &exception_info.position,
                &file_env.stack.0,
                &file_env.vfs,
                &file_env.project_root,
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
            &src_vfs_path,
            &[ToplevelItem::Expr(expr.clone())],
            &mut block_env,
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
            Err(EvalError::Exception(exception_info)) => {
                // Check if exception was expected
                if let Some(comment) = &expected_comment {
                    if comment.trim() == "*exception*" {
                        // Exception was expected, this is fine
                        results.push(ExpressionResult {
                            value: Value::unit(),
                            expected_comment: expected_comment.clone(),
                            position: expr.0.position.clone(),
                        });
                        continue;
                    }
                }
                // Exception was not expected, propagate error with full stack trace
                let error_msg = format_exception_with_stack(
                    &exception_info.message,
                    &exception_info.position,
                    &block_env.stack.0,
                    &block_env.vfs,
                    &block_env.project_root,
                );
                return Err(error_msg);
            }
            Err(e) => {
                return Err(format!("Evaluation error: {:?}", e));
            }
        }
    }

    Ok(results)
}

fn print_diagnostics(project_root: &Path, vfs: &Vfs, diagnostics: &[Diagnostic]) {
    for diagnostic in diagnostics {
        let formatted = format_diagnostic(
            &diagnostic.message,
            &diagnostic.position,
            project_root,
            diagnostic.severity,
            &diagnostic.notes,
            vfs,
        );
        eprintln!("{}", formatted);
    }
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

fn run_blocks_in_file(
    file_path: &Path,
    interrupted: Arc<AtomicBool>,
    project_root: &Path,
) -> (bool, usize) {
    let src = match std::fs::read_to_string(file_path) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error reading {}: {}", file_path.display(), e);
            return (false, 0);
        }
    };

    let is_gdn = file_path.extension().is_some_and(|ext| ext == "gdn");

    // Extract code blocks and the source to use for parsing
    let (code_blocks, markdown_src) = if is_gdn {
        // For .gdn files, extract doc comments as markdown, taking
        // care to use the same number of lines.
        let mut doc_markdown = String::new();
        for line in src.lines() {
            let trimmed = line.trim_start();
            if let Some(comment_content) = trimmed.strip_prefix("///") {
                doc_markdown.push_str(comment_content);
            }
            doc_markdown.push('\n');
        }

        // Extract code blocks from the markdown
        let blocks = extract_code_blocks(&doc_markdown);
        (blocks, doc_markdown)
    } else {
        // For markdown files, use the original source
        let blocks = extract_code_blocks(&src);
        (blocks, src.clone())
    };

    let mut had_error = false;

    // Create a temporary env for displaying values
    let display_env = Env::new(IdGenerator::default(), Vfs::default());

    for block in code_blocks.iter() {
        // Evaluate the code block using the appropriate source
        match eval_code_block(
            block,
            &src,
            &markdown_src,
            file_path,
            interrupted.clone(),
            project_root,
        ) {
            Ok(results) => {
                for result in results {
                    let value_str = result.value.display(&display_env);

                    let check_result =
                        check_expression(&value_str, result.expected_comment.as_deref());

                    match check_result {
                        CheckResult::Passed => {}
                        CheckResult::ExpectedException => {}
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
                        CheckResult::NoAssertion => {}
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

    (!had_error, code_blocks.len())
}

pub(crate) fn run_code_blocks(paths: &[PathBuf], interrupted: Arc<AtomicBool>) {
    if paths.is_empty() {
        eprintln!("No files specified");
        std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
    }

    let project_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));

    let mut all_success = true;
    let mut total_files = 0;
    let mut total_blocks = 0;

    for path in paths {
        let abs_path = to_abs_path(path);
        let (success, block_count) =
            run_blocks_in_file(&abs_path, interrupted.clone(), &project_root);
        if !success {
            all_success = false;
        }
        total_files += 1;
        total_blocks += block_count;
    }

    let block_word = if total_blocks == 1 { "block" } else { "blocks" };
    let file_word = if total_files == 1 { "file" } else { "files" };
    eprintln!(
        "Checked {} {} in {} {}.",
        total_blocks, block_word, total_files, file_word
    );

    if !all_success {
        std::process::exit(1);
    }
}
