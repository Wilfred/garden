//! Check code examples in markdown files.

use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

use crate::diagnostics::{format_diagnostic, format_exception_with_stack, Severity};
use crate::env::Env;
use crate::eval::{eval_toplevel_items, EvalError, Session, StdoutMode};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::vfs::Vfs;
use crate::parser::{parse_toplevel_items, ParseError};
use crate::BAD_CLI_REQUEST_EXIT_CODE;

/// A code block extracted from markdown.
struct CodeBlock {
    content: String,
    line_number: usize,
}

/// Result of evaluating an expression in a code block.
struct ExpressionResult {
    value: crate::values::Value,
    expected_comment: Option<String>,
}

/// Result of checking an expression value against an expected value.
enum CheckResult {
    Passed,
    Failed { expected: String, got: String },
    NoAssertion { value: String },
}

/// Extract code blocks from markdown source.
fn extract_code_blocks(markdown: &str) -> Vec<CodeBlock> {
    let parser = Parser::new(markdown);
    let mut blocks = vec![];
    let mut in_code_block = false;
    let mut current_content = String::new();
    let mut current_line = 1;
    let mut block_start_line = 1;
    let mut should_skip = false;

    for event in parser {
        match event {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(info))) => {
                in_code_block = true;
                current_content.clear();
                block_start_line = current_line;

                // Check if we should skip this block
                let info_str = info.as_ref();
                // Process blocks marked as "garden" or with no language
                // Skip blocks with "nocheck" attribute
                should_skip = info_str.contains("nocheck");
            }
            Event::End(TagEnd::CodeBlock) => {
                if in_code_block && !should_skip && !current_content.is_empty() {
                    blocks.push(CodeBlock {
                        content: current_content.clone(),
                        line_number: block_start_line,
                    });
                }
                in_code_block = false;
                should_skip = false;
            }
            Event::Text(text) if in_code_block => {
                current_content.push_str(text.as_ref());
            }
            Event::SoftBreak | Event::HardBreak => {
                current_line += 1;
                if in_code_block {
                    current_content.push('\n');
                }
            }
            _ => {}
        }
    }

    blocks
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
    markdown_path: &Path,
    block_idx: usize,
    interrupted: Arc<AtomicBool>,
) -> Result<Vec<ExpressionResult>, String> {
    // Create isolated environment for this block
    let mut id_gen = IdGenerator::default();
    let mut vfs = Vfs::default();
    let vfs_path = vfs.insert(
        Rc::new(
            markdown_path
                .with_file_name(format!("block_{}", block_idx + 1))
                .with_extension("gdn"),
        ),
        block.content.clone(),
    );

    // Parse the code block
    let (items, parse_errors) = parse_toplevel_items(&vfs_path, &block.content, &mut id_gen);

    if !parse_errors.is_empty() {
        let mut error_messages = vec![];
        for error in parse_errors {
            match error {
                ParseError::Invalid {
                    position,
                    message,
                    notes,
                } => {
                    let formatted = format_diagnostic(
                        &message,
                        &position,
                        markdown_path,
                        Severity::Error,
                        &notes,
                        &vfs,
                    );
                    error_messages.push(formatted);
                }
                ParseError::Incomplete { message, .. } => {
                    error_messages
                        .push(format!("Parse error (incomplete): {}", message.as_string()));
                }
            }
        }
        return Err(error_messages.join("\n"));
    }

    // Create environment
    let mut env = Env::new(id_gen, vfs);

    // Create session
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

    // First evaluate just the definitions (functions, types, etc)
    if !defs.is_empty() {
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
    }

    // Now evaluate each expression individually so we can get its value
    let mut results = vec![];
    for expr in exprs {
        // Check for //-> comment
        let expected_comment = extract_arrow_comment(&block.content, expr.0.position.end_offset);

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
                    });
                }
            }
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
    }

    Ok(results)
}

/// Check if an expression's value matches the expected value.
fn check_expression(value: &str, expected: Option<&str>) -> CheckResult {
    match expected {
        Some(expected) => {
            let expected = expected.trim();
            if value == expected {
                CheckResult::Passed
            } else {
                CheckResult::Failed {
                    expected: expected.to_owned(),
                    got: value.to_owned(),
                }
            }
        }
        None => CheckResult::NoAssertion {
            value: value.to_owned(),
        },
    }
}

/// Main entry point for checking markdown files.
pub(crate) fn check_markdown(src: &str, markdown_path: &Path, interrupted: Arc<AtomicBool>) {
    // Extract code blocks
    let code_blocks = extract_code_blocks(src);

    if code_blocks.is_empty() {
        eprintln!("No code blocks found in {}", markdown_path.display());
        std::process::exit(BAD_CLI_REQUEST_EXIT_CODE);
    }

    // Track statistics
    let mut passed_checks = 0;
    let mut failed_checks = 0;
    let mut total_expressions = 0;
    let mut had_error = false;

    // Create a temporary env for displaying values
    let display_env = Env::new(IdGenerator::default(), Vfs::default());

    // Process each code block
    for (block_idx, block) in code_blocks.iter().enumerate() {
        // Evaluate the code block
        match eval_code_block(block, markdown_path, block_idx, interrupted.clone()) {
            Ok(results) => {
                for result in results {
                    total_expressions += 1;

                    // Get string representation of value
                    let value_str = result.value.display(&display_env);

                    // Check against expected value
                    let check_result =
                        check_expression(&value_str, result.expected_comment.as_deref());

                    match check_result {
                        CheckResult::Passed => {
                            passed_checks += 1;
                        }
                        CheckResult::Failed { expected, got } => {
                            failed_checks += 1;
                            had_error = true;
                            eprintln!(
                                "{}:{}: Check failed",
                                markdown_path.display(),
                                block.line_number
                            );
                            eprintln!("  Expected: {}", expected);
                            eprintln!("  Got:      {}", got);
                            eprintln!();
                        }
                        CheckResult::NoAssertion { value } => {
                            // Print the value for expressions without //-> comments
                            println!("{}", value);
                        }
                    }
                }
            }
            Err(error) => {
                had_error = true;
                eprintln!(
                    "{}:{}: {}",
                    markdown_path.display(),
                    block.line_number,
                    error
                );
            }
        }
    }

    // Print summary
    if had_error {
        if failed_checks > 0 {
            eprintln!("{} checks failed, {} passed", failed_checks, passed_checks);
        }
        std::process::exit(1);
    } else {
        println!(
            "All checks passed in {} ({} code blocks, {} expressions)",
            markdown_path.file_name().unwrap().to_string_lossy(),
            code_blocks.len(),
            total_expressions
        );
    }
}
