//! Minimal nREPL (network REPL) server implementation.
//!
//! Speaks the bencode wire format (via `serde_bencode`) and supports
//! a small subset of the standard nREPL operations: `clone`,
//! `describe`, `eval`, `completions`, `lookup`, `close` and
//! `ls-sessions`.
//!
//! See <https://nrepl.org/nrepl/index.html> for the protocol
//! specification.

// `serde_bencode::value::Value::Dict` is defined as `HashMap<Vec<u8>,
// Value>`, so this module is forced to use `std::collections::HashMap`
// rather than `FxHashMap` to match the third-party API.
#![allow(clippy::disallowed_types)]

use std::collections::HashMap;
use std::io::{self, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
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
use crate::parser::ast::{IdGenerator, SymbolName};
use crate::parser::vfs::{to_project_relative, Vfs};
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
    let vfs_path = env.vfs.insert(Rc::new(path), code.to_owned());

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

    let (diagnostics, _) = load_toplevel_items_with_stubs(&items, env, ns.clone());

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

    let eval_result = eval_toplevel_exprs_then_stop(&items, env, session, ns.clone());

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

            let ns_name = ns
                .borrow()
                .abs_path
                .file_stem()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_else(|| ns.borrow().abs_path.display().to_string());

            let mut value_msg = base_msg.clone();
            value_msg.insert(b"value".to_vec(), bstr(value_str));
            value_msg.insert(b"ns".to_vec(), bstr(ns_name));
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
    fn completions_returns_prelude_matches() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));
        let session_id = conn.new_session();

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("completions")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (b"prefix".to_vec(), bstr("printl")),
        ]);

        let responses = handle_message(&mut conn, &request);
        assert_eq!(responses.len(), 1);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };

        let Some(Value::List(items)) = d.get(b"completions".as_slice()) else {
            panic!("expected completions list");
        };
        assert!(
            items.iter().any(|item| {
                let Value::Dict(c) = item else { return false };
                dict_get(c, "candidate").and_then(as_str) == Some("println")
            }),
            "expected to find println among completions"
        );
    }

    #[test]
    fn completions_follow_current_namespace() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));
        let session_id = conn.new_session();

        // Eval defines a new function in the session's current
        // namespace (__user for a fresh session).
        let eval_request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("eval")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (
                b"code".to_vec(),
                bstr("fun my_unique_helper(): Int { 42 } my_unique_helper()"),
            ),
        ]);
        handle_message(&mut conn, &eval_request);

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("completions")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (b"prefix".to_vec(), bstr("my_unique")),
        ]);
        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        let Some(Value::List(items)) = d.get(b"completions".as_slice()) else {
            panic!("expected completions list");
        };
        let candidate = items
            .iter()
            .find_map(|item| {
                let Value::Dict(c) = item else { return None };
                let name = dict_get(c, "candidate").and_then(as_str)?;
                if name == "my_unique_helper" {
                    Some(c.clone())
                } else {
                    None
                }
            })
            .expect("expected my_unique_helper among completions");

        // The candidate's ns should reflect the session's current
        // namespace, which is __user for a fresh session.
        assert_eq!(dict_get(&candidate, "ns").and_then(as_str), Some("__user"));
    }

    #[test]
    fn lookup_returns_prelude_info() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));
        let session_id = conn.new_session();

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("lookup")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (b"sym".to_vec(), bstr("println")),
        ]);

        let responses = handle_message(&mut conn, &request);
        assert_eq!(responses.len(), 1);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        let Some(Value::Dict(info)) = d.get(b"info".as_slice()) else {
            panic!("expected info dict");
        };
        assert_eq!(dict_get(info, "name").and_then(as_str), Some("println"));
        assert!(dict_get(info, "arglists-str").and_then(as_str).is_some());
    }

    #[test]
    fn lookup_returns_user_function_info() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));
        let session_id = conn.new_session();

        let eval_request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("eval")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (
                b"code".to_vec(),
                bstr("/// Adds one to its argument.\nfun add_one(i: Int): Int { i + 1 }"),
            ),
        ]);
        handle_message(&mut conn, &eval_request);

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("lookup")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (b"sym".to_vec(), bstr("add_one")),
        ]);

        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        let Some(Value::Dict(info)) = d.get(b"info".as_slice()) else {
            panic!("expected info dict");
        };
        assert_eq!(dict_get(info, "name").and_then(as_str), Some("add_one"));
        assert_eq!(dict_get(info, "ns").and_then(as_str), Some("__user"));
        assert_eq!(
            dict_get(info, "arglists-str").and_then(as_str),
            Some("(i: Int)")
        );
        assert_eq!(
            dict_get(info, "doc").and_then(as_str),
            Some("Adds one to its argument.")
        );
        assert!(matches!(info.get(b"line".as_slice()), Some(Value::Int(_))));
    }

    #[test]
    fn lookup_unknown_symbol_omits_info() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));
        let session_id = conn.new_session();

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("lookup")),
            (b"session".to_vec(), bstr(session_id.clone())),
            (b"sym".to_vec(), bstr("definitely_not_defined")),
        ]);

        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        assert!(d.get(b"info".as_slice()).is_none());
        let Some(Value::List(status)) = d.get(b"status".as_slice()) else {
            panic!("expected status list");
        };
        assert!(status.iter().any(|s| as_str(s) == Some("done")));
    }

    #[test]
    fn lookup_unknown_session() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("lookup")),
            (b"session".to_vec(), bstr("does-not-exist")),
            (b"sym".to_vec(), bstr("println")),
        ]);

        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        let Some(Value::List(status)) = d.get(b"status".as_slice()) else {
            panic!("expected status list");
        };
        assert!(status.iter().any(|s| as_str(s) == Some("unknown-session")));
    }

    #[test]
    fn completions_unknown_session() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("completions")),
            (b"session".to_vec(), bstr("does-not-exist")),
            (b"prefix".to_vec(), bstr("p")),
        ]);

        let responses = handle_message(&mut conn, &request);
        assert_eq!(responses.len(), 1);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };
        let Some(Value::List(status)) = d.get(b"status".as_slice()) else {
            panic!("expected status list");
        };
        assert!(status.iter().any(|s| as_str(s) == Some("unknown-session")));
    }

    fn bencode_dict_map(pairs: Vec<(Vec<u8>, Value)>) -> HashMap<Vec<u8>, Value> {
        let mut m = HashMap::new();
        for (k, v) in pairs {
            m.insert(k, v);
        }
        m
    }

    #[test]
    fn describe_returns_ops_versions_and_aux() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

        let request = bencode_dict_map(vec![(b"op".to_vec(), bstr("describe"))]);
        let responses = handle_message(&mut conn, &request);
        assert_eq!(responses.len(), 1);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };

        let Some(Value::Dict(ops)) = d.get(b"ops".as_slice()) else {
            panic!("expected ops dict");
        };
        for op_name in &[
            "describe",
            "clone",
            "close",
            "eval",
            "completions",
            "ls-sessions",
        ] {
            assert!(
                ops.contains_key(op_name.as_bytes()),
                "expected describe to advertise the {op_name} op",
            );
        }

        let Some(Value::Dict(versions)) = d.get(b"versions".as_slice()) else {
            panic!("expected versions dict");
        };
        assert!(versions.contains_key(b"garden".as_slice()));
        assert!(versions.contains_key(b"nrepl".as_slice()));

        assert!(
            d.contains_key(b"aux".as_slice()),
            "describe response should contain an aux field"
        );

        let Some(Value::List(status)) = d.get(b"status".as_slice()) else {
            panic!("expected status list");
        };
        assert!(status.iter().any(|s| as_str(s) == Some("done")));
    }

    #[test]
    fn describe_verbose_populates_op_descriptors() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

        let request = bencode_dict_map(vec![
            (b"op".to_vec(), bstr("describe")),
            (b"verbose?".to_vec(), bstr("true")),
        ]);
        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };

        let Some(Value::Dict(ops)) = d.get(b"ops".as_slice()) else {
            panic!("expected ops dict");
        };

        let Some(Value::Dict(eval_op)) = ops.get(b"eval".as_slice()) else {
            panic!("expected eval op descriptor");
        };
        assert!(
            eval_op.contains_key(b"doc".as_slice()),
            "verbose eval op should have a doc field"
        );
        assert!(eval_op.contains_key(b"requires".as_slice()));
        assert!(eval_op.contains_key(b"optional".as_slice()));
        assert!(eval_op.contains_key(b"returns".as_slice()));
    }

    #[test]
    fn describe_non_verbose_has_empty_op_descriptors() {
        let mut conn = Connection::new(Arc::new(AtomicBool::new(false)));

        let request = bencode_dict_map(vec![(b"op".to_vec(), bstr("describe"))]);
        let responses = handle_message(&mut conn, &request);
        let Value::Dict(d) = &responses[0] else {
            panic!("expected dict");
        };

        let Some(Value::Dict(ops)) = d.get(b"ops".as_slice()) else {
            panic!("expected ops dict");
        };
        let Some(Value::Dict(eval_op)) = ops.get(b"eval".as_slice()) else {
            panic!("expected eval op entry");
        };
        assert!(
            eval_op.is_empty(),
            "non-verbose op entries should be empty maps"
        );
    }

    #[test]
    fn clean_eof() {
        let bytes: Vec<u8> = vec![];
        let mut cursor = Cursor::new(bytes);
        assert!(read_message(&mut cursor).unwrap().is_none());
    }
}
