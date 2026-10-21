use std::collections::VecDeque;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;

use crate::commands::{
    print_available_commands, run_command, Command, CommandError, CommandParseError, EvalAction,
};
use crate::diagnostics::format_exception_with_stack;
use crate::env::Env;
use crate::eval::{
    eval, load_toplevel_items_with_stubs, push_test_stackframe, EvalError, ExceptionInfo,
    ExpressionState, Session, StdoutStderrMode,
};
use crate::parser::ast::{IdGenerator, ToplevelItem};
use crate::parser::{parse_toplevel_items, ParseError};
use crate::prompt::prompt_symbol;
use crate::syntax_highlighter::GardenHighlighter;
use crate::Vfs;

use owo_colors::OwoColorize;
use rustyline::history::DefaultHistory;
use rustyline::Editor;
use xdg::BaseDirectories;

enum ReadError {
    NeedsEval(EvalAction),
    ReadlineError,
}

/// A source of input lines for a REPL session.
///
/// The interactive REPL reads from the terminal via rustyline, while
/// reftests replay a fixed script. Abstracting over the line source
/// lets both share the same session logic.
trait LineReader {
    /// Read a single line, displaying `prompt`. Returns `None` when
    /// there is no more input.
    fn read_line(&mut self, prompt: &str) -> Option<String>;

    /// Record `line` in the input history. No-op by default.
    fn add_history(&mut self, _line: &str) {}

    /// Persist the accepted source `src` to the session log. No-op by
    /// default.
    fn log_src(&mut self, _src: &str) {}
}

/// A [`LineReader`] backed by rustyline, used for interactive sessions.
struct RustylineReader {
    editor: Editor<GardenHighlighter, DefaultHistory>,
}

impl RustylineReader {
    fn new() -> Self {
        let mut editor = Editor::new().unwrap();
        editor.set_helper(Some(GardenHighlighter::new()));
        if let Some(path) = BaseDirectories::with_prefix("garden").get_state_file("history") {
            let _ = editor.load_history(&path);
        }
        Self { editor }
    }
}

impl LineReader for RustylineReader {
    fn read_line(&mut self, prompt: &str) -> Option<String> {
        self.editor.readline(prompt).ok()
    }

    fn add_history(&mut self, line: &str) {
        let _ = self.editor.add_history_entry(line);
        if let Ok(path) = BaseDirectories::with_prefix("garden").place_state_file("history") {
            let _ = self.editor.save_history(&path);
        }
    }

    fn log_src(&mut self, src: &str) {
        let _ = log_src(src);
    }
}

/// A [`LineReader`] that replays a fixed list of input lines, used by
/// reftests.
struct ScriptedReader {
    lines: VecDeque<String>,
}

impl LineReader for ScriptedReader {
    fn read_line(&mut self, _prompt: &str) -> Option<String> {
        self.lines.pop_front()
    }
}

/// Read toplevel items from the reader. If the user gives us a command,
/// execute it and prompt again.
fn read_expr(
    env: &mut Env,
    session: &mut Session,
    reader: &mut dyn LineReader,
    is_stopped: bool,
) -> Result<(String, Vec<ToplevelItem>), ReadError> {
    loop {
        match reader.read_line(&prompt_symbol(is_stopped)) {
            Some(input) => {
                reader.add_history(&input);

                match Command::from_string(&input) {
                    Ok(cmd) => match run_command(&mut std::io::stdout(), cmd, env, session) {
                        Ok(()) => {
                            println!();
                            println!();
                            continue;
                        }
                        Err(e) => match e {
                            CommandError::Io(e) => {
                                panic!("Unexpected write error during command output: {e:?}")
                            }
                            CommandError::Action(eval_action) => {
                                return Err(ReadError::NeedsEval(eval_action));
                            }
                        },
                    },
                    Err(CommandParseError::NoSuchCommand(s)) => {
                        print_available_commands(&s, &mut std::io::stdout()).unwrap();
                        println!();
                        continue;
                    }
                    Err(CommandParseError::NotCommandSyntax) => {
                        // Continue with expression parsing.
                    }
                }

                match read_multiline_syntax(&input, reader, &mut env.vfs, &mut env.id_gen) {
                    Ok((src, items)) => {
                        reader.log_src(&src);
                        return Ok((src, items));
                    }
                    Err(ParseError::Incomplete { message, .. }) => {
                        println!("Parsing failed (incomplete): {}", message.as_string());
                    }
                    Err(ParseError::Invalid { message, .. }) => {
                        println!("Parsing failed: {}", message.as_string());
                    }
                }
            }
            None => return Err(ReadError::ReadlineError),
        }

        println!();
    }
}

pub(crate) fn repl(interrupted: Arc<AtomicBool>, trace_exprs: bool) {
    print_repl_header();

    let mut reader = RustylineReader::new();
    run_repl(&mut reader, interrupted, trace_exprs);
}

/// Replay a scripted REPL session and print its output, for reftests.
///
/// Input lines are read from `src`. Lines beginning with `//` are
/// ignored so the same file can carry goldentests directives, and the
/// header is omitted to keep output stable across version bumps.
pub(crate) fn reftest_repl(src: &str, interrupted: Arc<AtomicBool>) {
    let mut lines: VecDeque<String> = src
        .lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .map(|line| line.to_owned())
        .collect();

    // Drop trailing blank lines so we don't feed empty prompts after
    // the script's final input.
    while lines.back().is_some_and(|line| line.trim().is_empty()) {
        lines.pop_back();
    }

    let mut reader = ScriptedReader { lines };
    run_repl(&mut reader, interrupted, false);
}

fn run_repl(reader: &mut dyn LineReader, interrupted: Arc<AtomicBool>, trace_exprs: bool) {
    let id_gen = IdGenerator::default();
    let vfs = Vfs::default();
    let mut env = Env::new(id_gen, vfs);

    let mut session = Session {
        interrupted,
        stdout_stderr_mode: StdoutStderrMode::WriteDirectly,
        start_time: Instant::now(),
        trace_exprs,
        pretty_print_json: false,
    };

    let mut is_stopped = false;
    let mut last_src = String::new();
    let path = PathBuf::from("__user.gdn");

    loop {
        println!();

        match read_expr(&mut env, &mut session, reader, is_stopped) {
            Ok((src, items)) => {
                last_src = src;

                let ns = env.get_or_create_namespace(&path);
                let (diagnostics, _) = load_toplevel_items_with_stubs(&items, &mut env, ns);
                for diagnostic in diagnostics {
                    println!("Warning: {}", diagnostic.message.as_string());
                }

                let mut exprs = vec![];
                for item in items {
                    match item {
                        ToplevelItem::Expr(e) => {
                            exprs.push(e.clone());
                        }
                        _ => {}
                    }
                }

                if exprs.is_empty() {
                    continue;
                }

                let stack_frame = env
                    .stack
                    .0
                    .last_mut()
                    .expect("Should always have the toplevel stack frame");

                // Push expressions in reverse order, so the top of
                // exprs_to_eval is the first expression from the
                // user.
                for expr in exprs.iter().rev() {
                    stack_frame
                        .exprs_to_eval
                        .push((ExpressionState::NotEvaluated, expr.0.clone().into()));
                }
            }
            Err(ReadError::NeedsEval(EvalAction::Abort)) => {
                // TODO: doesn't this need to pop the stack to the toplevel?
                // It seems to be working already.
                is_stopped = false;
                continue;
            }
            Err(ReadError::NeedsEval(EvalAction::Resume)) => {
                // Continue to eval_env below.
            }
            Err(ReadError::NeedsEval(EvalAction::Replace(expr))) => {
                let stack_frame = env.stack.0.last_mut().unwrap();

                stack_frame.evalled_values.pop();
                stack_frame
                    .exprs_to_eval
                    .push((ExpressionState::NotEvaluated, expr.into()));

                // TODO: Prevent :replace when we've not just halted.
            }
            Err(ReadError::NeedsEval(EvalAction::Skip)) => {
                let stack_frame = env.stack.0.last_mut().unwrap();

                stack_frame
                    .exprs_to_eval
                    .pop()
                    .expect("Tried to skip an expression, but none in this frame.");
            }
            Err(ReadError::NeedsEval(EvalAction::RunTest(name))) => {
                // Push test then continue to eval_env().
                let test = match env.tests.get(&name) {
                    Some(test) => test.clone(),
                    None => {
                        println!("No such test: {name}");
                        continue;
                    }
                };

                push_test_stackframe(&test, &mut env);
            }
            Err(ReadError::ReadlineError) => {
                break;
            }
        }

        match eval(&mut env, &session) {
            Ok(result) => {
                if let Some(display_str) = result.display_unless_unit(&env) {
                    println!("{display_str}");
                }

                is_stopped = false;
            }
            Err(EvalError::Exception(ExceptionInfo { position, message })) => {
                // TODO: this assumes the bad position occurs in the most recent input,
                // not e.g. in an earlier function definition.
                let _ = last_src; // should use this.
                println!(
                    "{}",
                    format_exception_with_stack(
                        &message,
                        &position,
                        &env.stack.0,
                        &env.vfs,
                        &env.project_root
                    )
                );
                is_stopped = true;
            }
            Err(EvalError::AssertionFailed(position, msg)) => {
                println!(
                    "{}",
                    format_exception_with_stack(
                        &msg,
                        &position,
                        &env.stack.0,
                        &env.vfs,
                        &env.project_root
                    )
                );
                is_stopped = true;
            }
            Err(EvalError::Interrupted) => {
                println!("Interrupted. You can take a look around, or use :resume to continue.");
                is_stopped = true;
            }
            Err(EvalError::ReachedTickLimit(_)) => {
                println!("Reached tick limit.");
                is_stopped = false;
            }
            Err(EvalError::ReachedStackLimit(_)) => {
                println!("Reached recursion limit.");
                is_stopped = false;
            }
            Err(EvalError::ForbiddenInSandbox(pos)) => {
                println!(
                    "{}: This call is forbidden in a sandbox.",
                    pos.as_ide_string(&env.project_root)
                );
                is_stopped = false;
            }
        }
    }
}

fn print_repl_header() {
    println!(
        "{} {}{}",
        "Garden".bold().green(),
        env!("CARGO_PKG_VERSION").bold(),
        ": good programs take time to grow.".bold()
    );
    println!("Type {} if you're new here.", ":help".bold().green());
}

/// Read toplevel items from stdin.
///
/// If the user writes an incomplete item (e.g. the line ends with
/// `{`), then keep reading until we have a full definition or an
/// error.
fn read_multiline_syntax(
    first_line: &str,
    reader: &mut dyn LineReader,
    vfs: &mut Vfs,
    id_gen: &mut IdGenerator,
) -> Result<(String, Vec<ToplevelItem>), ParseError> {
    let mut src = first_line.to_owned();

    loop {
        // Parse interactive input under the same namespace path that
        // definitions are loaded into (see `repl_session`), so that
        // toplevel items defined in the session (e.g. functions) are
        // visible to other items run in that namespace, such as tests
        // invoked with `:test`.
        let path = Rc::new(PathBuf::from("__user.gdn"));
        let vfs_path = vfs.insert(Rc::clone(&path), src.to_owned());

        let (items, errors) = parse_toplevel_items(&vfs_path, &src, id_gen);

        // TODO: return all errors.
        match errors.into_iter().next() {
            None => {
                if items.is_empty() && !src.trim().is_empty() {
                    // If we didn't parse anything, but the text isn't
                    // just whitespace, it's probably a comment that
                    // will become a doc comment.
                    match reader.read_line(&prompt_symbol(false)) {
                        Some(input) => {
                            src.push('\n');
                            src.push_str(&input);
                        }
                        None => return Ok((src, items)),
                    }
                } else {
                    return Ok((src, items));
                }
            }
            Some(e @ ParseError::Incomplete { .. }) => {
                match reader.read_line(&prompt_symbol(false)) {
                    Some(input) => {
                        src.push('\n');
                        src.push_str(&input);
                    }
                    None => return Err(e),
                }
            }
            Some(e @ ParseError::Invalid { .. }) => return Err(e),
        }
    }
}

fn log_src(src: &str) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("log.gdn")?;

    write!(file, "\n{src}")
}
