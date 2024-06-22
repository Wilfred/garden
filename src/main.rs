// Looks like a false positive from rust 1.71.
// TODO: File an issue.
#![allow(suspicious_double_ref_op)]
#![warn(clippy::todo)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::str_to_string)]
// Garden is too much of a prototype for this to be an issue.
#![allow(clippy::too_many_arguments)]
// Occurs in WIP code, and it's too obvious to be worth linting
// against.
#![allow(clippy::needless_if)]

mod checks;
mod cli_session;
mod colors;
mod commands;
mod diagnostics;
mod env;
mod eval;
mod garden_type;
mod json_session;
mod prompt;
mod syntax_check;
mod types;
mod values;
mod version;
mod visitor;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use clap::{Parser, Subcommand};
use owo_colors::OwoColorize as _;

use crate::diagnostics::{format_error_with_stack, format_parse_error, Level};
use crate::env::Env;
use crate::eval::eval_toplevel_tests;
use crate::eval::{eval_all_toplevel_items, eval_toplevel_defs, EvalError, Session};
use crate::values::escape_string_literal;
use garden_lang_parser::ast::{Expression, Expression_, SourceString, ToplevelItem};
use garden_lang_parser::diagnostics::ErrorMessage;
use garden_lang_parser::{parse_toplevel_item, parse_toplevel_items, ParseError};

#[derive(Debug, Parser)]
#[command(author, version=version::VERSION.as_str(), name="Garden", about = "A programming language for growing programs", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

// TODO: if a user accidentally writes `garden foo.gdn`, suggest
// `garden run foo.gdn`.
#[derive(Debug, Subcommand)]
enum Commands {
    /// Start a session directly in the CLI.
    Repl,
    /// Start a session over JSON RPC.
    Json,
    /// Print an example JSON request that's valid in JSON sessions.
    JsonExample,
    /// Execute a Garden program at the path specified. Additional
    /// arguments are passed as a list to the `main()` function.
    Run {
        path: PathBuf,
        arguments: Vec<String>,
    },
    /// Run all the tests in the Garden program at the path specified.
    Test { path: PathBuf },
    /// Check the Garden program at the path specified for issues.
    Check {
        path: PathBuf,
        #[clap(long, action)]
        json: bool,
    },
    /// Show the type of the expression at the position given.
    ShowType {
        path: PathBuf,
        line: usize,
        column: usize,
    },
    /// Parse the Garden program at the path specified and print the
    /// AST.
    DumpAst { path: PathBuf },
}

fn main() {
    let interrupted = Arc::new(AtomicBool::new(false));

    let i = interrupted.clone();
    ctrlc::set_handler(move || {
        i.store(true, Ordering::SeqCst);
    })
    .expect("Error setting Ctrl-C handler");

    let args = Cli::parse();
    match args.command {
        Commands::Repl => cli_session::repl(&interrupted),
        Commands::Json => json_session::json_session(&interrupted),
        Commands::Run { path, arguments } => match std::fs::read(&path) {
            Ok(src_bytes) => run_file(src_bytes, &path, &arguments, &interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::JsonExample => {
            println!("{}", json_session::sample_request_as_json());
        }
        Commands::Check { path, json } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = String::from_utf8(src_bytes).expect("TODO: handle invalid bytes");
                syntax_check::check(&path, &src, json);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::Test { path } => match std::fs::read(&path) {
            Ok(src_bytes) => run_tests_in_file(src_bytes, &path, &interrupted),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::DumpAst { path } => match std::fs::read(&path) {
            Ok(src_bytes) => dump_ast(src_bytes, &path),
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
        Commands::ShowType { path, line, column } => match std::fs::read(&path) {
            Ok(src_bytes) => {
                let src = String::from_utf8(src_bytes).expect("TODO: handle invalid bytes");
                show_type(&src, &path, line, column);
            }
            Err(e) => {
                eprintln!("Error: Could not read file {}: {}", path.display(), e);
            }
        },
    }
}

fn show_type(src: &str, path: &Path, line: usize, column: usize) {
    let lines: Vec<_> = src.lines().collect();
    println!("{}", &lines[line - 1]);

    match parse_toplevel_items(path, src) {
        Ok(items) => find_item_at(&items, line, column),
        Err(_) => eprintln!("Parse error."),
    }
}

fn find_item_at(items: &[ToplevelItem], line: usize, column: usize) {
    for item in items {
        let pos = match item {
            ToplevelItem::Def(d) => &d.1,
            ToplevelItem::Expr(e) => &e.0.pos,
        };

        if !pos.contains(line, column) {
            continue;
        }

        dbg!(pos);
        match item {
            ToplevelItem::Def(d) => {
                dbg!(d);
            }
            ToplevelItem::Expr(e) => {
                dbg!(find_expr_at(&e.0, line, column));
            }
        }
    }
}

fn find_expr_at(expr: &Expression, line: usize, column: usize) -> Option<Expression> {
    // Check `expr` includes this position.
    //
    // If so, see if any children incude it, and include the smallest matching expr.
    let pos = &expr.pos;
    if !pos.contains(line, column) {
        return None;
    }

    // If there's a inner expression that includes this position, return that.
    match &expr.expr_ {
        Expression_::Match(scrutinee_expr, cases) => {
            if let Some(e) = find_expr_at(scrutinee_expr, line, column) {
                return Some(e);
            }
            for (_, case_expr) in cases {
                if let Some(e) = find_expr_at(case_expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            if let Some(e) = find_expr_at(cond_expr, line, column) {
                return Some(e);
            }
            for expr in &then_block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
            if let Some(else_block) = else_block {
                for expr in &else_block.exprs {
                    if let Some(e) = find_expr_at(expr, line, column) {
                        return Some(e);
                    }
                }
            }
        }
        Expression_::While(cond_expr, block) => {
            if let Some(e) = find_expr_at(cond_expr, line, column) {
                return Some(e);
            }
            for expr in &block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::Assign(_var, expr) => {
            // TODO: support hover on the variable name in let expressions.
            if let Some(e) = find_expr_at(expr, line, column) {
                return Some(e);
            }
        }
        Expression_::Let(_var, _, expr) => {
            // TODO: support hover on the variable name in let expressions.
            if let Some(e) = find_expr_at(expr, line, column) {
                return Some(e);
            }
        }
        Expression_::Return(value) => {
            if let Some(value) = value {
                if let Some(e) = find_expr_at(value, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::ListLiteral(items) => {
            for item in items {
                if let Some(e) = find_expr_at(item, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::StructLiteral(_, fields) => {
            for (_, field_expr) in fields {
                if let Some(e) = find_expr_at(field_expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::BinaryOperator(lhs, _, rhs) => {
            if let Some(e) = find_expr_at(lhs, line, column) {
                return Some(e);
            }
            if let Some(e) = find_expr_at(rhs, line, column) {
                return Some(e);
            }
        }
        Expression_::Call(recv, args) | Expression_::MethodCall(recv, _, args) => {
            if let Some(e) = find_expr_at(recv, line, column) {
                return Some(e);
            }
            for arg in &args.arguments {
                if let Some(e) = find_expr_at(arg, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::FunLiteral(fun_info) => {
            // TODO: support hover types on parameters too.
            for expr in &fun_info.body.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        Expression_::Block(block) => {
            for expr in &block.exprs {
                if let Some(e) = find_expr_at(expr, line, column) {
                    return Some(e);
                }
            }
        }
        // These expression cases have no inner expression.
        Expression_::IntLiteral(_)
        | Expression_::StringLiteral(_)
        | Expression_::Variable(_)
        | Expression_::DotAccess(_, _)
        | Expression_::Break => {}
    };

    Some(expr.clone())
}

fn dump_ast(src_bytes: Vec<u8>, path: &Path) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                for item in items {
                    match item {
                        ToplevelItem::Def(d) => {
                            println!("{:#?}", d.2);
                        }
                        ToplevelItem::Expr(e) => {
                            println!("{:#?}", e.0.expr_);
                        }
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}

// TODO: Much of this logic is duplicated with run_file.
fn run_tests_in_file(src_bytes: Vec<u8>, path: &Path, interrupted: &Arc<AtomicBool>) {
    let mut succeeded = false;

    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                    trace_exprs: false,
                };

                eval_toplevel_defs(&items, &mut env);

                match eval_toplevel_tests(&items, &mut env, &mut session) {
                    Ok(summary) => {
                        // TODO: should we allow tests to keep going
                        // after the first failure?
                        // TODO: print incremental progress as tests run.
                        println!("All {} test(s) {}.", summary.tests_passed, "passed".green());
                        succeeded = true;
                    }
                    Err(EvalError::ResumableError(position, e)) => {
                        eprintln!("{}", &format_error_with_stack(&e, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }

    if !succeeded {
        std::process::exit(1);
    }
}

fn run_file(src_bytes: Vec<u8>, path: &Path, arguments: &[String], interrupted: &Arc<AtomicBool>) {
    match String::from_utf8(src_bytes) {
        Ok(src) => match parse_toplevel_items(path, &src) {
            Ok(items) => {
                let mut env = Env::default();
                let mut session = Session {
                    interrupted,
                    has_attached_stdout: true,
                    start_time: Instant::now(),
                    trace_exprs: false,
                };

                eval_toplevel_defs(&items, &mut env);

                let call_src = call_to_main_src(arguments);
                let call_exprs =
                    vec![parse_toplevel_item(&PathBuf::from("__main_fun__"), &call_src).unwrap()];
                match eval_all_toplevel_items(&call_exprs, &mut env, &mut session) {
                    Ok(_) => {}
                    Err(EvalError::ResumableError(position, msg)) => {
                        eprintln!("{}", &format_error_with_stack(&msg, &position, &env.stack));
                    }
                    Err(EvalError::Interrupted) => {
                        eprintln!("Interrupted");
                    }
                }
            }
            Err(ParseError::Invalid {
                position,
                message: e,
                additional: _,
            }) => {
                eprintln!(
                    "{}",
                    &format_parse_error(
                        &ErrorMessage(format!("Parse error: {}", e.0)),
                        &position,
                        Level::Error,
                        &SourceString { src, offset: 0 }
                    )
                );
            }
            Err(ParseError::Incomplete { message: e, .. }) => {
                eprintln!("Parse error (incomplete input): {}", e.0);
            }
        },
        Err(e) => {
            eprintln!("Error: {} is not valid UTF-8: {}", path.display(), e);
        }
    }
}

fn call_to_main_src(cli_args: &[String]) -> String {
    let arg_literals: Vec<_> = cli_args.iter().map(|s| escape_string_literal(s)).collect();
    format!("main([{}]);", arg_literals.join(", "))
}

#[cfg(test)]
mod tests {
    use assert_cmd::prelude::*;
    use std::process::Command;

    use goldentests::{TestConfig, TestResult};

    #[test]
    fn test_parser() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/parser_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_checks() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/check_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_runtime() -> TestResult<()> {
        let mut config = TestConfig::new("target/debug/garden", "src/runtime_test_files", "// ")?;
        config.overwrite_tests = std::env::var("REGENERATE").is_ok();
        config.run_tests()
    }

    #[test]
    fn test_prelude_unit_tests() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("test").arg("src/prelude.gdn");
        cmd.assert().success();
    }

    #[test]
    fn test_prelude_check() {
        let path = assert_cmd::cargo::cargo_bin("garden");
        let mut cmd = Command::new(path);

        cmd.arg("check").arg("src/prelude.gdn");
        cmd.assert().success();
    }
}
