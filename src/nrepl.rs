//! Minimal nREPL (network REPL) server implementation.
//!
//! Speaks the bencode wire format (via `serde_bencode`) and supports
//! a small subset of the standard nREPL operations: `clone`,
//! `describe`, `eval`, `load-file`, `completions`, `lookup`, `close`
//! and `ls-sessions`.
//!
//! See <https://nrepl.org/nrepl/index.html> for the protocol
//! specification.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use serde_bencode::value::Value;
use tracing::{debug, error, info, warn};

use crate::diagnostics::format_exception_with_stack;
use crate::env::Env;
use crate::eval::{
    eval_toplevel_exprs_then_stop, load_toplevel_items_with_stubs, EvalError, ExceptionInfo,
    Session, StdoutMode,
};
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{IdGenerator, SymbolName};
use crate::parser::vfs::{to_abs_path, to_project_relative, Vfs};
use crate::parser::{parse_toplevel_items, ParseError};
use crate::values::Value_;

/// Refuse to buffer a single message larger than this. Prevents a
/// malformed client from making us allocate without bound.
const MAX_MESSAGE_BYTES: usize = 16 * 1024 * 1024;

fn bstr(s: impl Into<String>) -> Value {
    Value::Bytes(s.into().into_bytes())
}

fn bencode_dict<I, K>(pairs: I) -> Value
where
    I: IntoIterator<Item = (K, Value)>,
    K: Into<Vec<u8>>,
{
    let mut m = HashMap::new();
    for (k, v) in pairs {
        m.insert(k.into(), v);
    }
    Value::Dict(m)
}

/// Get a value from a bencode dict by string key.
fn dict_get<'a>(d: &'a HashMap<Vec<u8>, Value>, key: &str) -> Option<&'a Value> {
    d.get(key.as_bytes())
}

/// Interpret a bencode value as a UTF-8 string slice.
fn as_str(v: &Value) -> Option<&str> {
    match v {
        Value::Bytes(b) => std::str::from_utf8(b).ok(),
        _ => None,
    }
}

/// Read a single bencode value from `reader`. Returns `None` on a
/// clean EOF.
///
/// We feed bytes one at a time into `serde_bencode::from_bytes`. As
/// soon as the buffer parses successfully we have exactly one
/// complete message; any extra bytes the OS has buffered remain in
/// the `BufReader` for the next call.
fn read_message<R: Read>(reader: &mut R) -> io::Result<Option<Value>> {
    let mut buf: Vec<u8> = Vec::new();
    let mut byte = [0u8; 1];

    loop {
        match reader.read(&mut byte) {
            Ok(0) => {
                if buf.is_empty() {
                    return Ok(None);
                }
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "unexpected EOF in the middle of a bencode message",
                ));
            }
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }

        buf.push(byte[0]);

        if let Ok(value) = serde_bencode::from_bytes::<Value>(&buf) {
            return Ok(Some(value));
        }

        if buf.len() > MAX_MESSAGE_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "bencode message exceeded the maximum allowed size",
            ));
        }
    }
}

/// Encode a bencode value and write it to `writer`.
fn write_message<W: Write>(writer: &mut W, value: &Value) -> io::Result<()> {
    let bytes = serde_bencode::to_bytes(value)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("bencode encode: {e}")))?;
    writer.write_all(&bytes)
}

fn next_session_id(counter: &mut u64) -> String {
    *counter += 1;
    format!("garden-{counter}")
}

/// Perform a single `eval` request, returning the response messages
/// to send back to the client.
fn handle_eval(
    code: &str,
    env: &mut Env,
    session: &Session,
    stdout_buf: &Arc<Mutex<String>>,
    base_msg: &HashMap<Vec<u8>, Value>,
) -> Vec<Value> {
    let ns = env.current_namespace();
    let path = (*ns.borrow().abs_path).clone();
    eval_code_in_namespace(code, path, ns, env, session, stdout_buf, base_msg)
}

/// Perform a single `load-file` request, returning the response
/// messages to send back to the client. The file's contents are
/// loaded into a namespace identified by `file_path` (when provided),
/// becoming the session's current namespace afterwards. Subsequent
/// `eval` requests in the session will run in that namespace.
fn handle_load_file(
    code: &str,
    file_path: Option<&str>,
    env: &mut Env,
    session: &Session,
    stdout_buf: &Arc<Mutex<String>>,
    base_msg: &HashMap<Vec<u8>, Value>,
) -> Vec<Value> {
    let path: PathBuf = match file_path {
        Some(p) => to_abs_path(Path::new(p)),
        None => (*env.current_namespace().borrow().abs_path).clone(),
    };
    let ns = env.get_or_create_namespace(&path);
    eval_code_in_namespace(code, path, ns, env, session, stdout_buf, base_msg)
}

/// Parse `code`, load its top-level items into `namespace`, evaluate
/// any expressions, and build the nREPL response messages.
fn eval_code_in_namespace(
    code: &str,
    vfs_path_buf: PathBuf,
    namespace: Rc<RefCell<NamespaceInfo>>,
    env: &mut Env,
    session: &Session,
    stdout_buf: &Arc<Mutex<String>>,
    base_msg: &HashMap<Vec<u8>, Value>,
) -> Vec<Value> {
    let vfs_path = env.vfs.insert(Rc::new(vfs_path_buf), code.to_owned());

    let (items, errors) = parse_toplevel_items(&vfs_path, code, &mut env.id_gen);

    let mut responses = Vec::new();

    if !errors.is_empty() {
        let msg = errors
            .into_iter()
            .map(|e| match e {
                ParseError::Invalid { message, .. } => message.as_string(),
                ParseError::Incomplete { message, .. } => message.as_string(),
            })
            .collect::<Vec<_>>()
            .join("\n");

        let mut err_msg = base_msg.clone();
        err_msg.insert(b"err".to_vec(), bstr(format!("{msg}\n")));
        responses.push(Value::Dict(err_msg));

        let mut done_msg = base_msg.clone();
        done_msg.insert(
            b"status".to_vec(),
            Value::List(vec![bstr("done"), bstr("eval-error")]),
        );
        responses.push(Value::Dict(done_msg));
        return responses;
    }

    let (diagnostics, _) = load_toplevel_items_with_stubs(&items, env, namespace.clone());

    if !diagnostics.is_empty() {
        let warnings = diagnostics
            .iter()
            .map(|d| d.message.as_string())
            .collect::<Vec<_>>()
            .join("\n");

        let mut warn_msg = base_msg.clone();
        warn_msg.insert(b"err".to_vec(), bstr(format!("{warnings}\n")));
        responses.push(Value::Dict(warn_msg));
    }

    let eval_start = Instant::now();
    let eval_result = eval_toplevel_exprs_then_stop(&items, env, session, namespace.clone());
    let eval_msec = eval_start.elapsed().as_millis() as i64;

    let captured = std::mem::take(&mut *stdout_buf.lock().expect("stdout buffer poisoned"));
    if !captured.is_empty() {
        let mut out_msg = base_msg.clone();
        out_msg.insert(b"out".to_vec(), bstr(captured));
        responses.push(Value::Dict(out_msg));
    }

    match eval_result {
        Ok(value) => {
            let value_str = match value {
                Some(v) => v.display(env),
                None => "()".to_owned(),
            };

            let ns_name = namespace
                .borrow()
                .abs_path
                .file_stem()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_else(|| namespace.borrow().abs_path.display().to_string());

            let mut value_msg = base_msg.clone();
            value_msg.insert(b"value".to_vec(), bstr(value_str));
            value_msg.insert(b"ns".to_vec(), bstr(ns_name));
            value_msg.insert(b"eval-msec".to_vec(), Value::Int(eval_msec));
            responses.push(Value::Dict(value_msg));

            let mut done_msg = base_msg.clone();
            done_msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            responses.push(Value::Dict(done_msg));
        }
        Err(e) => {
            let (err_text, status_extra) = format_eval_error(&e, env);

            let mut err_msg = base_msg.clone();
            err_msg.insert(b"err".to_vec(), bstr(format!("{err_text}\n")));
            responses.push(Value::Dict(err_msg));

            let mut done_msg = base_msg.clone();
            let mut status = vec![bstr("done"), bstr("eval-error")];
            if let Some(extra) = status_extra {
                status.push(bstr(extra));
            }
            done_msg.insert(b"ex".to_vec(), bstr(err_text));
            done_msg.insert(b"eval-msec".to_vec(), Value::Int(eval_msec));
            done_msg.insert(b"status".to_vec(), Value::List(status));
            responses.push(Value::Dict(done_msg));
        }
    }

    responses
}

/// Build the `completions` response for a given prefix. Looks at
/// every value in scope in the session's current namespace (which
/// includes the prelude) and returns the ones whose name starts with
/// `prefix`.
fn handle_completions(env: &Env, prefix: &str, base_msg: &HashMap<Vec<u8>, Value>) -> Vec<Value> {
    let ns = env.current_namespace();
    let ns_borrow = ns.borrow();
    let ns_name = ns_borrow
        .abs_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_owned();

    let mut matches: Vec<(String, &'static str)> = ns_borrow
        .values
        .iter()
        .filter(|(name, _)| name.text.starts_with(prefix))
        .map(|(name, value)| {
            let kind = match value.as_ref() {
                Value_::Fun { .. }
                | Value_::Closure(..)
                | Value_::BuiltInFunction(..)
                | Value_::EnumConstructor { .. } => "function",
                Value_::Namespace { .. } => "namespace",
                _ => "var",
            };
            (name.text.clone(), kind)
        })
        .collect();
    matches.sort_by(|a, b| a.0.cmp(&b.0));

    let completions: Vec<Value> = matches
        .into_iter()
        .map(|(name, kind)| {
            bencode_dict(vec![
                (b"candidate".to_vec(), bstr(name)),
                (b"ns".to_vec(), bstr(ns_name.clone())),
                (b"priority".to_vec(), Value::Int(0)),
                (b"type".to_vec(), bstr(kind)),
            ])
        })
        .collect();

    let mut msg = base_msg.clone();
    msg.insert(b"completions".to_vec(), Value::List(completions));
    msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
    vec![Value::Dict(msg)]
}

/// Build the `lookup` response for a given symbol name. Looks up the
/// symbol in the session's current namespace and returns information
/// about it (file, line, doc, arglists, ...).
fn handle_lookup(env: &Env, sym: &str, base_msg: &HashMap<Vec<u8>, Value>) -> Vec<Value> {
    let ns = env.current_namespace();
    let ns_borrow = ns.borrow();
    let ns_name = ns_borrow
        .abs_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_owned();

    let value = ns_borrow.values.get(&SymbolName {
        text: sym.to_owned(),
    });

    let Some(value) = value else {
        let mut msg = base_msg.clone();
        msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
        return vec![Value::Dict(msg)];
    };

    let mut info: HashMap<Vec<u8>, Value> = HashMap::new();
    info.insert(b"name".to_vec(), bstr(sym));
    info.insert(b"ns".to_vec(), bstr(ns_name));

    if let Some(fun_info) = value.fun_info() {
        let params: Vec<String> = fun_info
            .params
            .params
            .iter()
            .map(|p| {
                let mut s = p.symbol.name.text.clone();
                if let Some(hint) = &p.hint {
                    s.push_str(": ");
                    s.push_str(&hint.as_src());
                }
                s
            })
            .collect();
        let arglists_str = format!("({})", params.join(", "));
        info.insert(b"arglists-str".to_vec(), bstr(arglists_str));

        let rel_path = to_project_relative(&fun_info.pos.path, &env.project_root);
        info.insert(b"file".to_vec(), bstr(rel_path.display().to_string()));
        info.insert(
            b"line".to_vec(),
            Value::Int((fun_info.pos.line_number as i64) + 1),
        );
        info.insert(
            b"column".to_vec(),
            Value::Int((fun_info.pos.column as i64) + 1),
        );

        if let Some(doc) = &fun_info.doc_comment {
            info.insert(b"doc".to_vec(), bstr(doc.clone()));
        }
    }

    let mut msg = base_msg.clone();
    msg.insert(b"info".to_vec(), Value::Dict(info));
    msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
    vec![Value::Dict(msg)]
}

fn format_eval_error(e: &EvalError, env: &Env) -> (String, Option<&'static str>) {
    match e {
        EvalError::Exception(ExceptionInfo { position, message }) => (
            format_exception_with_stack(
                message,
                position,
                &env.stack.0,
                &env.vfs,
                &env.project_root,
            ),
            None,
        ),
        EvalError::AssertionFailed(position, message) => (
            format_exception_with_stack(
                message,
                position,
                &env.stack.0,
                &env.vfs,
                &env.project_root,
            ),
            Some("assertion-failed"),
        ),
        EvalError::Interrupted => ("Interrupted.".to_owned(), Some("interrupted")),
        EvalError::ReachedTickLimit(_) => {
            ("Reached the tick limit.".to_owned(), Some("tick-limit"))
        }
        EvalError::ReachedStackLimit(_) => (
            "Reached the recursion limit.".to_owned(),
            Some("stack-limit"),
        ),
        EvalError::ForbiddenInSandbox(_) => (
            "Tried to execute unsafe code in sandboxed mode.".to_owned(),
            Some("sandbox"),
        ),
    }
}

/// State held for one client connection. Each connection may host
/// many logical nREPL sessions.
struct Connection {
    sessions: HashMap<String, Env>,
    next_id: u64,
    interrupted: Arc<AtomicBool>,
}

impl Connection {
    fn new(interrupted: Arc<AtomicBool>) -> Self {
        Self {
            sessions: HashMap::new(),
            next_id: 0,
            interrupted,
        }
    }

    fn new_session(&mut self) -> String {
        let id = next_session_id(&mut self.next_id);
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let env = Env::new(id_gen, vfs);
        self.sessions.insert(id.clone(), env);
        id
    }
}

/// Build the base response dict, copying the `id` and `session`
/// fields from the request when present.
fn base_response(request: &HashMap<Vec<u8>, Value>) -> HashMap<Vec<u8>, Value> {
    let mut base = HashMap::new();
    if let Some(id) = dict_get(request, "id") {
        base.insert(b"id".to_vec(), id.clone());
    }
    if let Some(s) = dict_get(request, "session") {
        base.insert(b"session".to_vec(), s.clone());
    }
    base
}

/// Description of a single nREPL op, used to populate the `ops`
/// field in a verbose `describe` response.
struct OpDescriptor {
    doc: &'static str,
    requires: &'static [(&'static str, &'static str)],
    optional: &'static [(&'static str, &'static str)],
    returns: &'static [(&'static str, &'static str)],
}

const SUPPORTED_OPS: &[(&str, OpDescriptor)] = &[
    (
        "describe",
        OpDescriptor {
            doc: "Produce a machine- and human-readable directory and documentation for the operations supported by an nREPL endpoint.",
            requires: &[],
            optional: &[(
                "verbose?",
                "Include informational detail for each \"op\"eration in the return message.",
            )],
            returns: &[
                ("aux", "Map of auxiliary data."),
                ("ops", "Map of \"op\"erations supported by this nREPL endpoint."),
                (
                    "versions",
                    "Map containing version maps, e.g. major, minor, incremental, and qualifier keys, for values, component names as keys.",
                ),
            ],
        },
    ),
    (
        "clone",
        OpDescriptor {
            doc: "Clones the current session, returning the ID of the newly-created session.",
            requires: &[],
            optional: &[(
                "session",
                "The ID of the session to be cloned; if not provided, a new session with default state will be created, and mapped to the returned id.",
            )],
            returns: &[("new-session", "The ID of the new session.")],
        },
    ),
    (
        "close",
        OpDescriptor {
            doc: "Closes the specified session.",
            requires: &[("session", "The ID of the session to be closed.")],
            optional: &[],
            returns: &[],
        },
    ),
    (
        "ls-sessions",
        OpDescriptor {
            doc: "Lists the IDs of all active sessions.",
            requires: &[],
            optional: &[],
            returns: &[("sessions", "A list of all available session IDs.")],
        },
    ),
    (
        "eval",
        OpDescriptor {
            doc: "Evaluates code.",
            requires: &[
                ("code", "The code to be evaluated."),
                (
                    "session",
                    "The ID of the session in which the code will be evaluated.",
                ),
            ],
            optional: &[],
            returns: &[
                ("ns", "The namespace in which the evaluation occurred."),
                ("value", "The result of evaluating the code, as a string."),
                (
                    "eval-msec",
                    "The number of milliseconds taken to evaluate the code.",
                ),
            ],
        },
    ),
    (
        "load-file",
        OpDescriptor {
            doc: "Loads a body of code, using supplied path and filename info to set source file and line number metadata.",
            requires: &[
                ("file", "Full body of code to be loaded."),
                (
                    "session",
                    "The ID of the session in which the file will be loaded.",
                ),
            ],
            optional: &[
                (
                    "file-path",
                    "Source path (e.g. a/b/c.gdn) of the file being loaded.",
                ),
                ("file-name", "Name of the source file, for example foo.gdn."),
            ],
            returns: &[
                ("ns", "The namespace in which the file was loaded."),
                (
                    "value",
                    "The result of evaluating the file's last expression, as a string.",
                ),
                (
                    "eval-msec",
                    "The number of milliseconds taken to evaluate the file.",
                ),
            ],
        },
    ),
    (
        "completions",
        OpDescriptor {
            doc: "Return a list of symbols matching the specified prefix that are visible in the current namespace.",
            requires: &[
                ("prefix", "The prefix to match against."),
                (
                    "session",
                    "The ID of the session in which completions should be computed.",
                ),
            ],
            optional: &[],
            returns: &[(
                "completions",
                "A list of candidate maps, each with a candidate name, namespace, priority and type.",
            )],
        },
    ),
    (
        "lookup",
        OpDescriptor {
            doc: "Lookup symbol info.",
            requires: &[
                ("sym", "The symbol to look up."),
                (
                    "session",
                    "The ID of the session in which the symbol should be looked up.",
                ),
            ],
            optional: &[],
            returns: &[(
                "info",
                "A map of information about the symbol, including its name, namespace, file, line, column, arglists-str and doc.",
            )],
        },
    ),
];

fn op_descriptor_to_value(d: &OpDescriptor) -> Value {
    let to_dict = |entries: &[(&str, &str)]| -> Value {
        let pairs: Vec<(Vec<u8>, Value)> = entries
            .iter()
            .map(|(k, v)| (k.as_bytes().to_vec(), bstr(*v)))
            .collect();
        bencode_dict(pairs)
    };

    bencode_dict(vec![
        (b"doc".to_vec(), bstr(d.doc)),
        (b"requires".to_vec(), to_dict(d.requires)),
        (b"optional".to_vec(), to_dict(d.optional)),
        (b"returns".to_vec(), to_dict(d.returns)),
    ])
}

fn truthy(v: &Value) -> bool {
    match v {
        Value::Int(n) => *n != 0,
        Value::Bytes(b) => !b.is_empty() && b.as_slice() != b"false" && b.as_slice() != b"nil",
        Value::List(items) => !items.is_empty(),
        Value::Dict(d) => !d.is_empty(),
    }
}

/// Handle a single request message from the client.
fn handle_message(conn: &mut Connection, request: &HashMap<Vec<u8>, Value>) -> Vec<Value> {
    let op = dict_get(request, "op").and_then(as_str).unwrap_or("");
    let id = dict_get(request, "id").and_then(as_str).unwrap_or("");
    let session = dict_get(request, "session").and_then(as_str).unwrap_or("");
    debug!(op, id, session, "nREPL: received request");

    let base = base_response(request);

    match op {
        "describe" => {
            let verbose = dict_get(request, "verbose?").map(truthy).unwrap_or(false);

            let mut ops_dict: HashMap<Vec<u8>, Value> = HashMap::new();
            for (name, descriptor) in SUPPORTED_OPS {
                let value = if verbose {
                    op_descriptor_to_value(descriptor)
                } else {
                    Value::Dict(HashMap::new())
                };
                ops_dict.insert(name.as_bytes().to_vec(), value);
            }

            let garden_major: i64 = env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap_or(0);
            let garden_minor: i64 = env!("CARGO_PKG_VERSION_MINOR").parse().unwrap_or(0);
            let garden_incremental: i64 = env!("CARGO_PKG_VERSION_PATCH").parse().unwrap_or(0);

            let versions = bencode_dict(vec![
                (
                    b"garden".to_vec(),
                    bencode_dict(vec![
                        (b"major".to_vec(), Value::Int(garden_major)),
                        (b"minor".to_vec(), Value::Int(garden_minor)),
                        (b"incremental".to_vec(), Value::Int(garden_incremental)),
                    ]),
                ),
                (
                    b"nrepl".to_vec(),
                    bencode_dict(vec![
                        (b"major".to_vec(), Value::Int(0)),
                        (b"minor".to_vec(), Value::Int(1)),
                        (b"incremental".to_vec(), Value::Int(0)),
                    ]),
                ),
            ]);

            let mut msg = base;
            msg.insert(b"ops".to_vec(), Value::Dict(ops_dict));
            msg.insert(b"versions".to_vec(), versions);
            msg.insert(b"aux".to_vec(), Value::Dict(HashMap::new()));
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            vec![Value::Dict(msg)]
        }
        "clone" => {
            let new_id = conn.new_session();
            let mut msg = base;
            msg.insert(b"new-session".to_vec(), bstr(new_id));
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            vec![Value::Dict(msg)]
        }
        "close" => {
            if let Some(s) = dict_get(request, "session").and_then(as_str) {
                conn.sessions.remove(s);
            }
            let mut msg = base;
            msg.insert(
                b"status".to_vec(),
                Value::List(vec![bstr("done"), bstr("session-closed")]),
            );
            vec![Value::Dict(msg)]
        }
        "ls-sessions" => {
            let sessions: Vec<Value> = conn.sessions.keys().map(|s| bstr(s.clone())).collect();
            let mut msg = base;
            msg.insert(b"sessions".to_vec(), Value::List(sessions));
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            vec![Value::Dict(msg)]
        }
        "eval" => {
            let session_id = dict_get(request, "session")
                .and_then(as_str)
                .map(|s| s.to_owned());

            let code = dict_get(request, "code")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();

            let session_id = match session_id {
                Some(s) if conn.sessions.contains_key(&s) => s,
                _ => {
                    let mut msg = base;
                    msg.insert(
                        b"status".to_vec(),
                        Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
                    );
                    return vec![Value::Dict(msg)];
                }
            };

            let env = conn.sessions.get_mut(&session_id).unwrap();

            let stdout_buf = Arc::new(Mutex::new(String::new()));
            let session = Session {
                interrupted: Arc::clone(&conn.interrupted),
                stdout_mode: StdoutMode::WriteToBuffer(Arc::clone(&stdout_buf)),
                start_time: Instant::now(),
                trace_exprs: false,
                pretty_print_json: false,
            };

            handle_eval(&code, env, &session, &stdout_buf, &base)
        }
        "load-file" => {
            let session_id = dict_get(request, "session")
                .and_then(as_str)
                .map(|s| s.to_owned());

            let file = dict_get(request, "file")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();

            let file_path = dict_get(request, "file-path")
                .and_then(as_str)
                .or_else(|| dict_get(request, "file-name").and_then(as_str))
                .map(|s| s.to_owned());

            let session_id = match session_id {
                Some(s) if conn.sessions.contains_key(&s) => s,
                _ => {
                    let mut msg = base;
                    msg.insert(
                        b"status".to_vec(),
                        Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
                    );
                    return vec![Value::Dict(msg)];
                }
            };

            let env = conn.sessions.get_mut(&session_id).unwrap();

            let stdout_buf = Arc::new(Mutex::new(String::new()));
            let session = Session {
                interrupted: Arc::clone(&conn.interrupted),
                stdout_mode: StdoutMode::WriteToBuffer(Arc::clone(&stdout_buf)),
                start_time: Instant::now(),
                trace_exprs: false,
                pretty_print_json: false,
            };

            handle_load_file(
                &file,
                file_path.as_deref(),
                env,
                &session,
                &stdout_buf,
                &base,
            )
        }
        "completions" => {
            let session_id = dict_get(request, "session")
                .and_then(as_str)
                .map(|s| s.to_owned());

            let prefix = dict_get(request, "prefix")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();

            let session_id = match session_id {
                Some(s) if conn.sessions.contains_key(&s) => s,
                _ => {
                    let mut msg = base;
                    msg.insert(
                        b"status".to_vec(),
                        Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
                    );
                    return vec![Value::Dict(msg)];
                }
            };

            let env = conn.sessions.get(&session_id).unwrap();
            handle_completions(env, &prefix, &base)
        }
        "lookup" => {
            let session_id = dict_get(request, "session")
                .and_then(as_str)
                .map(|s| s.to_owned());

            let sym = dict_get(request, "sym")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();

            let session_id = match session_id {
                Some(s) if conn.sessions.contains_key(&s) => s,
                _ => {
                    let mut msg = base;
                    msg.insert(
                        b"status".to_vec(),
                        Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
                    );
                    return vec![Value::Dict(msg)];
                }
            };

            let env = conn.sessions.get(&session_id).unwrap();
            handle_lookup(env, &sym, &base)
        }
        "" => {
            let mut msg = base;
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("error")]));
            msg.insert(b"err".to_vec(), bstr("Missing 'op' in request.\n"));
            vec![Value::Dict(msg)]
        }
        other => {
            let mut msg = base;
            msg.insert(
                b"status".to_vec(),
                Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-op")]),
            );
            msg.insert(b"err".to_vec(), bstr(format!("Unknown op: {other}\n")));
            vec![Value::Dict(msg)]
        }
    }
}

/// Serve a single TCP connection until the client disconnects.
fn serve_connection(stream: TcpStream, interrupted: Arc<AtomicBool>) {
    let peer = stream
        .peer_addr()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| "<unknown>".to_owned());
    info!("nREPL: client connected from {peer}");

    let mut writer = stream.try_clone().expect("Could not clone TCP stream");
    let mut reader = BufReader::new(stream);

    let mut conn = Connection::new(interrupted);

    loop {
        let request = match read_message(&mut reader) {
            Ok(Some(Value::Dict(d))) => d,
            Ok(Some(_)) => {
                warn!("nREPL: ignoring non-dict request");
                continue;
            }
            Ok(None) => break,
            Err(e) => {
                error!("nREPL: error reading request: {e}");
                break;
            }
        };

        let responses = handle_message(&mut conn, &request);
        for response in responses {
            if let Err(e) = write_message(&mut writer, &response) {
                error!("nREPL: error writing response: {e}");
                return;
            }
        }
        if let Err(e) = writer.flush() {
            error!("nREPL: error flushing writer: {e}");
            return;
        }
    }

    info!("nREPL: client {peer} disconnected");
}

/// Convert a JSON value into a bencode value. Used by the nREPL
/// reftest format, which expresses bencode messages in JSON for
/// readability.
///
/// JSON strings become bencode `Bytes`. Booleans collapse to `0` or
/// `1` (bencode has no native boolean). Floats are truncated to
/// integers since bencode has no float type.
fn json_to_bencode(v: serde_json::Value) -> Value {
    use serde_json::Value as J;
    match v {
        J::Null => Value::Bytes(Vec::new()),
        J::Bool(b) => Value::Int(if b { 1 } else { 0 }),
        J::Number(n) => Value::Int(n.as_i64().unwrap_or(0)),
        J::String(s) => Value::Bytes(s.into_bytes()),
        J::Array(arr) => Value::List(arr.into_iter().map(json_to_bencode).collect()),
        J::Object(obj) => {
            let mut d = HashMap::new();
            for (k, v) in obj {
                d.insert(k.into_bytes(), json_to_bencode(v));
            }
            Value::Dict(d)
        }
    }
}

/// Convert a bencode value into a JSON value for display in a
/// reftest. Byte strings that are valid UTF-8 are rendered as JSON
/// strings; otherwise they are rendered as an array of byte values
/// so the output stays valid JSON.
///
/// Dict keys are sorted so the output is deterministic.
fn bencode_to_json(v: &Value) -> serde_json::Value {
    use serde_json::Value as J;
    match v {
        Value::Int(n) => J::Number((*n).into()),
        Value::Bytes(b) => match std::str::from_utf8(b) {
            Ok(s) => J::String(s.to_owned()),
            Err(_) => J::Array(
                b.iter()
                    .map(|byte| J::Number((*byte as u64).into()))
                    .collect(),
            ),
        },
        Value::List(items) => J::Array(items.iter().map(bencode_to_json).collect()),
        Value::Dict(d) => {
            let mut keys: Vec<&Vec<u8>> = d.keys().collect();
            keys.sort();
            let mut map = serde_json::Map::new();
            for k in keys {
                let key_str = String::from_utf8_lossy(k).into_owned();
                map.insert(key_str, bencode_to_json(&d[k]));
            }
            J::Object(map)
        }
    }
}

/// Replace fields that vary between runs (wall-clock timings, the
/// compiled-in Garden version) with stable placeholder strings so
/// reftest output is reproducible.
fn normalize_for_reftest(value: serde_json::Value) -> serde_json::Value {
    use serde_json::Value as J;
    match value {
        J::Object(map) => {
            let mut new_map = serde_json::Map::new();
            for (k, v) in map {
                // `eval-msec` is wall-clock-dependent: mask only the
                // integer value, not its documentation string in the
                // verbose describe response.
                // `garden` (when an object with version components)
                // changes whenever we bump CARGO_PKG_VERSION.
                let normalized = match (k.as_str(), &v) {
                    ("eval-msec", J::Number(_)) => J::String("<eval-msec>".to_owned()),
                    ("garden", J::Object(_)) => J::String("<garden-version>".to_owned()),
                    _ => normalize_for_reftest(v),
                };
                new_map.insert(k, normalized);
            }
            J::Object(new_map)
        }
        J::Array(arr) => J::Array(arr.into_iter().map(normalize_for_reftest).collect()),
        other => other,
    }
}

/// Run the nREPL reftest driver against `src`.
///
/// `src` is a line-oriented stream where each non-blank,
/// non-comment line is a JSON object representing one nREPL request.
/// Lines starting with `//` are ignored. For each request we print
/// the list of response messages as a JSON array. The request itself
/// is omitted from the output since it already appears in the input
/// file at the corresponding position.
pub(crate) fn reftest_nrepl(src: &str) {
    use serde_json::json;
    use serde_json::Value as J;

    let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

    for line in src.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }

        let request_json: J = match serde_json::from_str(trimmed) {
            Ok(v) => v,
            Err(e) => {
                let err_obj = json!({
                    "error": format!("invalid JSON request: {e}"),
                    "line": trimmed,
                });
                println!("{}", serde_json::to_string_pretty(&err_obj).unwrap());
                continue;
            }
        };

        let request_dict = match json_to_bencode(request_json) {
            Value::Dict(d) => d,
            _ => {
                let err_obj = json!({
                    "error": "request must be a JSON object",
                    "line": trimmed,
                });
                println!("{}", serde_json::to_string_pretty(&err_obj).unwrap());
                continue;
            }
        };

        let responses = handle_message(&mut conn, &request_dict);
        for response in responses {
            let response_json = normalize_for_reftest(bencode_to_json(&response));
            println!("{}", serde_json::to_string_pretty(&response_json).unwrap());
        }
    }
}

/// Run an nREPL server, listening on `host:port`.
pub(crate) fn run_nrepl(host: &str, port: u16, interrupted: Arc<AtomicBool>) {
    let addr = format!("{host}:{port}");
    let listener = match TcpListener::bind(&addr) {
        Ok(l) => l,
        Err(e) => {
            error!("nREPL: could not bind to {addr}: {e}");
            std::process::exit(1);
        }
    };

    let local = listener
        .local_addr()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| addr.clone());
    info!("nREPL server started on {local}");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let interrupted = Arc::clone(&interrupted);
                std::thread::Builder::new()
                    .name("nrepl-client".to_owned())
                    .spawn(move || serve_connection(stream, interrupted))
                    .expect("Could not spawn nREPL client thread");
            }
            Err(e) => {
                error!("nREPL: error accepting connection: {e}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn round_trip_message() {
        let mut d: HashMap<Vec<u8>, Value> = HashMap::new();
        d.insert(b"op".to_vec(), bstr("eval"));
        d.insert(b"code".to_vec(), bstr("1 + 2"));
        let original = Value::Dict(d);

        let bytes = serde_bencode::to_bytes(&original).unwrap();

        let mut cursor = Cursor::new(bytes);
        let decoded = read_message(&mut cursor).unwrap().unwrap();

        let original_bytes = serde_bencode::to_bytes(&original).unwrap();
        let decoded_bytes = serde_bencode::to_bytes(&decoded).unwrap();
        assert_eq!(original_bytes, decoded_bytes);
    }

    #[test]
    fn back_to_back_messages() {
        // Two messages encoded back to back. read_message should
        // consume only the first; the second remains in the reader.
        let m1 = bencode_dict(vec![(b"op".to_vec(), bstr("describe"))]);
        let m2 = bencode_dict(vec![(b"op".to_vec(), bstr("clone"))]);

        let mut bytes = serde_bencode::to_bytes(&m1).unwrap();
        bytes.extend_from_slice(&serde_bencode::to_bytes(&m2).unwrap());

        let mut cursor = Cursor::new(bytes);
        let first = read_message(&mut cursor).unwrap().unwrap();
        let second = read_message(&mut cursor).unwrap().unwrap();

        match first {
            Value::Dict(d) => {
                assert_eq!(dict_get(&d, "op").and_then(as_str), Some("describe"));
            }
            _ => panic!("expected dict"),
        }
        match second {
            Value::Dict(d) => {
                assert_eq!(dict_get(&d, "op").and_then(as_str), Some("clone"));
            }
            _ => panic!("expected dict"),
        }
    }

    #[test]
    fn clean_eof() {
        let bytes: Vec<u8> = vec![];
        let mut cursor = Cursor::new(bytes);
        assert!(read_message(&mut cursor).unwrap().is_none());
    }
}
