use serde::Serialize;
use std::io::IsTerminal;
use std::path::Path;

use crate::checks::check_toplevel_items_in_env;
use crate::diagnostics::{format_diagnostic, Autofix, Diagnostic, Severity};
use crate::parser::ast::IdGenerator;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::diagnostics::MessagePart::*;
use crate::parser::position::Position;
use crate::parser::vfs::{Vfs, VfsPathBuf};
use crate::parser::{parse_toplevel_items_from_span, ParseError};
use crate::run_code_blocks::extract_code_blocks;
use crate::{load_toplevel_items, Env};

/// A diagnostic that can be serialized. Messages and positions are
/// stored in an IDE friendly format.
///
/// See also `Diagnostic`.
#[derive(Debug, Serialize)]
struct CheckDiagnostic {
    #[serde(skip)]
    position: Position,
    #[serde(skip)]
    notes: Vec<(ErrorMessage, Position)>,
    // TODO: Could we just use Position here? The only downside of
    // using Position directly is that a file with a large number of
    // errors will repeat the same file name many times in the JSON
    // output.
    /// 1-indexed.
    line_number: usize,
    end_line_number: usize,
    column: usize,
    end_column: usize,
    message: String,
    severity: Severity,
}

/// Apply fixes to the source code, returning the modified source.
fn apply_fixes(src: &str, fixes: &[Autofix]) -> String {
    let mut fixes = fixes.to_vec();

    // Sort fixes by start offset in descending order so we can apply
    // them from the end without invalidating earlier offsets.
    fixes.sort_by_key(|b| std::cmp::Reverse(b.position.start_offset));

    let mut result = src.to_owned();
    for fix in fixes {
        let start = fix.position.start_offset;
        let end = fix.position.end_offset;
        result = format!("{}{}{}", &result[..start], fix.new_text, &result[end..]);
    }
    result
}

fn parse_error_to_diagnostic(error: ParseError, use_color: bool) -> CheckDiagnostic {
    let (position, message, notes) = match error {
        ParseError::Invalid {
            position,
            message,
            notes,
        } => (position, message, notes),
        ParseError::Incomplete { position, message } => (position, message, vec![]),
    };

    CheckDiagnostic {
        line_number: position.line_number + 1,
        end_line_number: position.end_line_number + 1,
        column: position.column,
        end_column: position.end_column,
        message: if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        },
        severity: Severity::Error,
        notes,
        position,
    }
}

fn diagnostic_to_check_diagnostic(diagnostic: Diagnostic, use_color: bool) -> CheckDiagnostic {
    let Diagnostic {
        message,
        position,
        severity,
        notes,
        fixes: _,
    } = diagnostic;

    CheckDiagnostic {
        line_number: position.line_number + 1,
        end_line_number: position.end_line_number + 1,
        column: position.column,
        end_column: position.end_column,
        message: if use_color {
            message.as_styled_string()
        } else {
            message.as_string()
        },
        severity,
        notes,
        position,
    }
}

/// Parse and check a span of `src`, appending any diagnostics and
/// fixes found.
fn check_span(
    env: &mut Env,
    vfs_path: &VfsPathBuf,
    src: &str,
    start: usize,
    end: usize,
    namespace_path: &Path,
    use_color: bool,
    diagnostics: &mut Vec<CheckDiagnostic>,
    all_fixes: &mut Vec<Autofix>,
) {
    let (items, errors) =
        parse_toplevel_items_from_span(vfs_path, src, &mut env.id_gen, start, end);

    let had_parse_errors = !errors.is_empty();
    for e in errors {
        diagnostics.push(parse_error_to_diagnostic(e, use_color));
    }

    // If we have syntax errors, don't bother checking anything else,
    // as it has a high false positive rate and it's hard to see what
    // needs fixing next.
    if had_parse_errors {
        return;
    }

    let ns = env.get_or_create_namespace(namespace_path);
    let (mut raw_diagnostics, _) = load_toplevel_items(&items, env, ns.clone());
    raw_diagnostics.extend(check_toplevel_items_in_env(vfs_path, &items, env, ns));

    for diagnostic in raw_diagnostics {
        all_fixes.extend(diagnostic.fixes.clone());
        diagnostics.push(diagnostic_to_check_diagnostic(diagnostic, use_color));
    }
}

fn report_and_exit(diagnostics: &[CheckDiagnostic], all_fixes: &[Autofix], env: &Env, json: bool) {
    for (i, diagnostic) in diagnostics.iter().enumerate() {
        let s = if json {
            serde_json::to_string(diagnostic).expect("TODO: can this ever fail?")
        } else {
            format_diagnostic(
                &ErrorMessage(vec![Text(diagnostic.message.clone())]),
                &diagnostic.position,
                &env.project_root,
                diagnostic.severity,
                &diagnostic.notes,
                &env.vfs,
            )
        };

        println!("{}{}", if i == 0 { "" } else { "\n" }, s);
    }

    if !diagnostics.is_empty() {
        if !json && !all_fixes.is_empty() {
            let num_fixable = all_fixes.len();
            eprintln!(
                "\n{} {} can be fixed automatically (use `--fix`).",
                num_fixable,
                if num_fixable == 1 { "issue" } else { "issues" }
            );
        }
        std::process::exit(1);
    }
}

fn apply_and_write_fixes(src: &str, all_fixes: &[Autofix], stdout: bool, original_path: &Path) {
    if all_fixes.is_empty() {
        if stdout {
            print!("{}", src);
        }
        return;
    }

    let fixed_src = apply_fixes(src, all_fixes);
    if stdout {
        print!("{}", fixed_src);
    } else {
        std::fs::write(original_path, &fixed_src).expect("Failed to write fixed file");
    }
}

pub(crate) fn check(
    path: &Path,
    src: &str,
    json: bool,
    fix: bool,
    stdout: bool,
    original_path: &Path,
) {
    let use_color = std::io::stdout().is_terminal() && !json && !fix;

    let mut diagnostics = vec![];
    let mut all_fixes: Vec<Autofix> = vec![];

    let id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let mut env = Env::new(id_gen, vfs);

    check_span(
        &mut env,
        &vfs_path,
        src,
        0,
        src.len(),
        path,
        use_color,
        &mut diagnostics,
        &mut all_fixes,
    );

    if fix {
        apply_and_write_fixes(src, &all_fixes, stdout, original_path);
        return;
    }

    report_and_exit(&diagnostics, &all_fixes, &env, json);
}

/// Check all Garden code blocks in a markdown file.
pub(crate) fn check_markdown(
    path: &Path,
    src: &str,
    json: bool,
    fix: bool,
    stdout: bool,
    original_path: &Path,
) {
    let use_color = std::io::stdout().is_terminal() && !json && !fix;

    let mut diagnostics = vec![];
    let mut all_fixes: Vec<Autofix> = vec![];

    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());
    let base_env = Env::new(IdGenerator::default(), vfs);

    let blocks = extract_code_blocks(src);

    // Each code block is checked independently in a fresh env clone,
    // so that definitions in one example don't leak into another and
    // produce duplicate-definition errors.
    for block in &blocks {
        let mut env = base_env.clone();
        check_span(
            &mut env,
            &vfs_path,
            src,
            block.start_offset,
            block.end_offset,
            path,
            use_color,
            &mut diagnostics,
            &mut all_fixes,
        );
    }

    if fix {
        apply_and_write_fixes(src, &all_fixes, stdout, original_path);
        return;
    }

    report_and_exit(&diagnostics, &all_fixes, &base_env, json);
}
