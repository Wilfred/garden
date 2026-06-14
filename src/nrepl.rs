//! Minimal nREPL (network REPL) server implementation.
//!
//! Speaks the bencode wire format (via `serde_bencode`) and supports
//! a small subset of the standard nREPL operations: `clone`,
//! `describe`, `eval`, `load-file`, `completions`, `lookup`,
//! `interrupt`, `close` and `ls-sessions`.
//!
//! Each session runs on its own worker thread so that `interrupt`
//! requests can be handled while an `eval` is still in progress.
//!
//! See <https://nrepl.org/nrepl/index.html> for the protocol
//! specification.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::sync::{Arc, Mutex, Weak};
use std::time::{Duration, Instant};
use std::{fs, thread};

use serde_bencode::value::Value;
use tracing::{debug, error, info, warn};

use crate::diagnostics::format_exception_with_stack;
use crate::env::Env;
use crate::eval::{
    eval_toplevel_exprs_then_stop, load_toplevel_items_with_stubs, EvalError, ExceptionInfo,
    Session, StdoutStderrMode,
};
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{IdGenerator, SymbolName};
use crate::parser::vfs::{to_abs_path, to_project_relative, Vfs};
use crate::parser::{parse_toplevel_items, ParseError};
use crate::temp_built_in_files::TempBuiltInFiles;
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

fn invalid_data(msg: &'static str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg)
}

/// Wraps a reader, tracking the total number of bytes consumed while
/// parsing a single message so it cannot exceed `MAX_MESSAGE_BYTES`.
struct CountingReader<'a, R: Read> {
    reader: &'a mut R,
    count: usize,
}

impl<R: Read> CountingReader<'_, R> {
    fn check_limit(&self) -> io::Result<()> {
        if self.count > MAX_MESSAGE_BYTES {
            return Err(invalid_data(
                "bencode message exceeded the maximum allowed size",
            ));
        }
        Ok(())
    }

    /// Read a single byte. Retries on `Interrupted`. Returns `None`
    /// on EOF.
    fn read_byte(&mut self) -> io::Result<Option<u8>> {
        let mut byte = [0u8; 1];
        loop {
            match self.reader.read(&mut byte) {
                Ok(0) => return Ok(None),
                Ok(_) => {
                    self.count += 1;
                    self.check_limit()?;
                    return Ok(Some(byte[0]));
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            }
        }
    }

    /// Read a single byte, treating EOF as an error.
    fn read_byte_required(&mut self) -> io::Result<u8> {
        self.read_byte()?.ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected EOF in the middle of a bencode message",
            )
        })
    }

    /// Read exactly `len` bytes. EOF before `len` bytes is an error.
    fn read_exact_counted(&mut self, len: usize) -> io::Result<Vec<u8>> {
        // Account for the declared length before allocating so an
        // over-large string cannot make us allocate without bound.
        self.count = self.count.saturating_add(len);
        self.check_limit()?;

        let mut buf = vec![0u8; len];
        self.reader.read_exact(&mut buf)?;
        Ok(buf)
    }
}

/// Read a single bencode value from `reader`. Returns `None` on a
/// clean EOF.
///
/// Parses the bencode structure directly, reading exactly the bytes
/// of one message. Any extra bytes the OS has buffered remain in the
/// `BufReader` for the next call.
fn read_message<R: Read>(reader: &mut R) -> io::Result<Option<Value>> {
    let mut r = CountingReader { reader, count: 0 };
    match r.read_byte()? {
        None => Ok(None),
        Some(first) => Ok(Some(parse_value(&mut r, first)?)),
    }
}

/// Parse a bencode value whose first byte (`first`) has already been
/// read.
fn parse_value<R: Read>(r: &mut CountingReader<R>, first: u8) -> io::Result<Value> {
    match first {
        b'i' => parse_int(r),
        b'l' => parse_list(r),
        b'd' => parse_dict(r),
        b'0'..=b'9' => parse_bytes(r, first),
        _ => Err(invalid_data("unexpected byte in bencode stream")),
    }
}

/// Parse the remainder of an integer (`i<digits>e`), the leading `i`
/// having been consumed.
fn parse_int<R: Read>(r: &mut CountingReader<R>) -> io::Result<Value> {
    let mut digits = Vec::new();
    loop {
        let b = r.read_byte_required()?;
        if b == b'e' {
            break;
        }
        digits.push(b);
    }
    let n = std::str::from_utf8(&digits)
        .ok()
        .and_then(|s| s.parse::<i64>().ok())
        .ok_or_else(|| invalid_data("invalid bencode integer"))?;
    Ok(Value::Int(n))
}

/// Parse a byte string (`<len>:<bytes>`), the first length digit
/// (`first`) having been consumed.
fn parse_bytes<R: Read>(r: &mut CountingReader<R>, first: u8) -> io::Result<Value> {
    let mut digits = vec![first];
    loop {
        let b = r.read_byte_required()?;
        if b == b':' {
            break;
        }
        digits.push(b);
    }
    let len = std::str::from_utf8(&digits)
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .ok_or_else(|| invalid_data("invalid bencode string length"))?;
    Ok(Value::Bytes(r.read_exact_counted(len)?))
}

/// Parse the remainder of a list (`l<values>e`), the leading `l`
/// having been consumed.
fn parse_list<R: Read>(r: &mut CountingReader<R>) -> io::Result<Value> {
    let mut items = Vec::new();
    loop {
        let b = r.read_byte_required()?;
        if b == b'e' {
            break;
        }
        items.push(parse_value(r, b)?);
    }
    Ok(Value::List(items))
}

/// Parse the remainder of a dict (`d<key><value>...e`), the leading
/// `d` having been consumed. Keys are bencode byte strings.
fn parse_dict<R: Read>(r: &mut CountingReader<R>) -> io::Result<Value> {
    let mut map: HashMap<Vec<u8>, Value> = HashMap::new();
    loop {
        let b = r.read_byte_required()?;
        if b == b'e' {
            break;
        }
        let key = match parse_value(r, b)? {
            Value::Bytes(k) => k,
            _ => return Err(invalid_data("bencode dict key was not a string")),
        };
        let first = r.read_byte_required()?;
        let value = parse_value(r, first)?;
        map.insert(key, value);
    }
    Ok(Value::Dict(map))
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

/// Options for streaming the printed value back to the client.
///
/// Set via the `nrepl.middleware.print/*` keys on an `eval` or
/// `load-file` request, mirroring the nREPL print middleware. See
/// <https://nrepl.org/nrepl/design/middleware.html#nrepl-middleware-print>.
#[derive(Clone, Debug)]
struct PrintOptions {
    /// If true, the printed value is sent in multiple response
    /// messages each containing a `value` chunk of at most
    /// `buffer_size` bytes.
    stream: bool,
    /// Maximum size in bytes of each streamed `value` chunk.
    buffer_size: usize,
    /// Maximum total size in bytes of the printed value. When the
    /// value exceeds the quota, the response is truncated and the
    /// terminal status includes `truncated`.
    quota: Option<usize>,
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self {
            stream: false,
            buffer_size: 1024,
            quota: None,
        }
    }
}

fn parse_print_options(request: &HashMap<Vec<u8>, Value>) -> PrintOptions {
    let stream = dict_get(request, "nrepl.middleware.print/stream?")
        .map(truthy)
        .unwrap_or(false);

    let parse_size = |key: &str| -> Option<usize> {
        match dict_get(request, key) {
            Some(Value::Int(n)) if *n > 0 => Some(*n as usize),
            Some(Value::Bytes(b)) => std::str::from_utf8(b).ok()?.parse().ok(),
            _ => None,
        }
    };

    let buffer_size = parse_size("nrepl.middleware.print/buffer-size").unwrap_or(1024);
    let quota = parse_size("nrepl.middleware.print/quota");

    PrintOptions {
        stream,
        buffer_size,
        quota,
    }
}

/// Split a string into chunks of at most `chunk_size` bytes, never
/// splitting in the middle of a UTF-8 character.
fn chunk_str(s: &str, chunk_size: usize) -> Vec<&str> {
    if s.is_empty() {
        return Vec::new();
    }
    let mut chunks = Vec::new();
    let mut start = 0;
    while start < s.len() {
        let mut end = (start + chunk_size).min(s.len());
        while end > start && !s.is_char_boundary(end) {
            end -= 1;
        }
        if end == start {
            // A single character is larger than chunk_size; emit it
            // anyway so we make progress.
            end = start + 1;
            while end < s.len() && !s.is_char_boundary(end) {
                end += 1;
            }
        }
        chunks.push(&s[start..end]);
        start = end;
    }
    chunks
}

/// Apply the print options to `value_str`, returning the value
/// messages to send and whether the result was truncated by quota.
fn build_value_messages(
    value_str: &str,
    ns_name: &str,
    eval_msec: i64,
    print_opts: &PrintOptions,
    base_msg: &HashMap<Vec<u8>, Value>,
) -> (Vec<Value>, bool) {
    let (display, truncated) = match print_opts.quota {
        Some(q) if value_str.len() > q => {
            let mut end = q;
            while end > 0 && !value_str.is_char_boundary(end) {
                end -= 1;
            }
            (&value_str[..end], true)
        }
        _ => (value_str, false),
    };

    let mut messages = Vec::new();

    if print_opts.stream {
        let chunks = chunk_str(display, print_opts.buffer_size);
        if chunks.is_empty() {
            let mut msg = base_msg.clone();
            msg.insert(b"value".to_vec(), bstr(""));
            msg.insert(b"ns".to_vec(), bstr(ns_name));
            messages.push(Value::Dict(msg));
        } else {
            for chunk in chunks {
                let mut msg = base_msg.clone();
                msg.insert(b"value".to_vec(), bstr(chunk));
                msg.insert(b"ns".to_vec(), bstr(ns_name));
                messages.push(Value::Dict(msg));
            }
        }
    } else {
        let mut msg = base_msg.clone();
        msg.insert(b"value".to_vec(), bstr(display));
        msg.insert(b"ns".to_vec(), bstr(ns_name));
        msg.insert(b"eval-msec".to_vec(), Value::Int(eval_msec));
        messages.push(Value::Dict(msg));
    }

    (messages, truncated)
}

/// Perform a single `eval` request, returning the response messages
/// to send back to the client.
fn handle_eval(
    code: &str,
    env: &mut Env,
    session: &Session,
    stdout_buf: &Arc<Mutex<String>>,
    stderr_buf: &Arc<Mutex<String>>,
    response_tx: &Sender<Value>,
    base_msg: &HashMap<Vec<u8>, Value>,
    print_opts: &PrintOptions,
) -> Vec<Value> {
    let ns = env.current_namespace();
    let path = (*ns.borrow().abs_path).clone();
    eval_code_in_namespace(
        code,
        path,
        ns,
        env,
        session,
        stdout_buf,
        stderr_buf,
        response_tx,
        base_msg,
        print_opts,
    )
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
    stderr_buf: &Arc<Mutex<String>>,
    response_tx: &Sender<Value>,
    base_msg: &HashMap<Vec<u8>, Value>,
    print_opts: &PrintOptions,
) -> Vec<Value> {
    let path: PathBuf = match file_path {
        Some(p) => to_abs_path(Path::new(p)),
        None => (*env.current_namespace().borrow().abs_path).clone(),
    };
    let ns = env.get_or_create_namespace(&path);
    eval_code_in_namespace(
        code,
        path,
        ns,
        env,
        session,
        stdout_buf,
        stderr_buf,
        response_tx,
        base_msg,
        print_opts,
    )
}

/// How often the output flusher drains the session's stdout and
/// stderr buffers while an eval is running.
const OUTPUT_FLUSH_INTERVAL: Duration = Duration::from_millis(100);

/// Drain `buf` and, if it held anything, send it to the client as a
/// single message keyed by `key` (`out` for stdout, `err` for
/// stderr).
fn flush_output_buffer(
    buf: &Arc<Mutex<String>>,
    key: &[u8],
    response_tx: &Sender<Value>,
    base_msg: &HashMap<Vec<u8>, Value>,
) {
    let captured = std::mem::take(&mut *buf.lock().expect("output buffer poisoned"));
    if !captured.is_empty() {
        let mut msg = base_msg.clone();
        msg.insert(key.to_vec(), bstr(captured));
        let _ = response_tx.send(Value::Dict(msg));
    }
}

/// Spawn a thread that periodically drains the session's stdout and
/// stderr buffers and streams them to the client while an eval runs.
///
/// The thread exits promptly once `stop_rx`'s sender is dropped; the
/// caller performs the final drain after joining.
fn spawn_output_flusher(
    stdout_buf: Arc<Mutex<String>>,
    stderr_buf: Arc<Mutex<String>>,
    response_tx: Sender<Value>,
    base_msg: HashMap<Vec<u8>, Value>,
    stop_rx: Receiver<()>,
) -> thread::JoinHandle<()> {
    thread::Builder::new()
        .name("nrepl-output-flusher".to_owned())
        .spawn(move || loop {
            match stop_rx.recv_timeout(OUTPUT_FLUSH_INTERVAL) {
                Err(RecvTimeoutError::Timeout) => {
                    flush_output_buffer(&stdout_buf, b"out", &response_tx, &base_msg);
                    flush_output_buffer(&stderr_buf, b"err", &response_tx, &base_msg);
                }
                // Stop requested (sender dropped); the caller does the
                // final drain.
                Ok(()) | Err(RecvTimeoutError::Disconnected) => return,
            }
        })
        .expect("Could not spawn nREPL output flusher thread")
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
    stderr_buf: &Arc<Mutex<String>>,
    response_tx: &Sender<Value>,
    base_msg: &HashMap<Vec<u8>, Value>,
    print_opts: &PrintOptions,
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

    let (diagnostics, _) = load_toplevel_items_with_stubs(&items, env, Rc::clone(&namespace));

    if !diagnostics.is_empty() {
        let warnings = diagnostics
            .iter()
            .map(|d| d.message.as_string())
            .collect::<Vec<_>>()
            .join("\n");

        let mut warn_msg = base_msg.clone();
        warn_msg.insert(b"err".to_vec(), bstr(format!("{warnings}\n")));
        let _ = response_tx.send(Value::Dict(warn_msg));
    }

    // Stream stdout/stderr to the client while the eval runs rather
    // than buffering everything until it finishes. This keeps output
    // flowing for a long-running eval and bounds the buffers for an
    // eval that prints in an infinite loop.
    let (flush_stop_tx, flush_stop_rx) = mpsc::channel::<()>();
    let flusher = spawn_output_flusher(
        Arc::clone(stdout_buf),
        Arc::clone(stderr_buf),
        response_tx.clone(),
        base_msg.clone(),
        flush_stop_rx,
    );

    let eval_start = Instant::now();
    let eval_result = eval_toplevel_exprs_then_stop(&items, env, session, Rc::clone(&namespace));
    let eval_msec = eval_start.elapsed().as_millis() as i64;

    // Stop the flusher and drain whatever printed since its last pass.
    drop(flush_stop_tx);
    let _ = flusher.join();
    flush_output_buffer(stdout_buf, b"out", response_tx, base_msg);
    flush_output_buffer(stderr_buf, b"err", response_tx, base_msg);

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

            let (value_msgs, truncated) =
                build_value_messages(&value_str, &ns_name, eval_msec, print_opts, base_msg);
            responses.extend(value_msgs);

            let mut done_msg = base_msg.clone();
            let mut status = vec![bstr("done")];
            if truncated {
                status.push(bstr("truncated"));
            }
            if print_opts.stream {
                done_msg.insert(b"eval-msec".to_vec(), Value::Int(eval_msec));
            }
            done_msg.insert(b"status".to_vec(), Value::List(status));
            responses.push(Value::Dict(done_msg));
        }
        Err(e) => {
            let is_interrupt = matches!(e, EvalError::Interrupted);
            let (err_text, status_extra) = format_eval_error(&e, env);

            let mut err_msg = base_msg.clone();
            err_msg.insert(b"err".to_vec(), bstr(format!("{err_text}\n")));
            responses.push(Value::Dict(err_msg));

            let mut done_msg = base_msg.clone();
            let mut status = vec![bstr("done")];
            if is_interrupt {
                status.push(bstr("interrupted"));
            } else {
                status.push(bstr("eval-error"));
                if let Some(extra) = status_extra {
                    status.push(bstr(extra));
                }
                done_msg.insert(b"ex".to_vec(), bstr(err_text));
            }
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
fn handle_lookup(
    env: &Env,
    sym: &str,
    base_msg: &HashMap<Vec<u8>, Value>,
    temp_built_in_files: Option<&TempBuiltInFiles>,
) -> Vec<Value> {
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

        let file_path = temp_built_in_files
            .and_then(|b| b.resolve(&fun_info.pos.path))
            .unwrap_or_else(|| to_project_relative(&fun_info.pos.path, &env.project_root));
        info.insert(b"file".to_vec(), bstr(file_path.display().to_string()));
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
        EvalError::Interrupted => ("Interrupted.".to_owned(), None),
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

/// A request handed off to a session worker thread. The worker
/// owns the session's `Env`, so any op that needs to read or mutate
/// it (eval, load-file, completions, lookup) is dispatched here.
enum SessionRequest {
    Eval {
        code: String,
        base_msg: HashMap<Vec<u8>, Value>,
        print_opts: PrintOptions,
    },
    LoadFile {
        code: String,
        file_path: Option<String>,
        base_msg: HashMap<Vec<u8>, Value>,
        print_opts: PrintOptions,
    },
    Completions {
        prefix: String,
        base_msg: HashMap<Vec<u8>, Value>,
    },
    Lookup {
        sym: String,
        base_msg: HashMap<Vec<u8>, Value>,
    },
}

/// State for one logical nREPL session.
struct SessionState {
    request_tx: Sender<SessionRequest>,
    interrupted: Arc<AtomicBool>,
}

/// State held for one client connection. Each connection may host
/// many logical nREPL sessions, each running on its own worker
/// thread.
struct Connection {
    sessions: HashMap<String, SessionState>,
    next_id: u64,
    response_tx: Sender<Value>,
    /// Shared list of session interrupt flags, used by the SIGINT
    /// watchdog to broadcast a global interrupt to every active
    /// session.
    interrupt_flags: Arc<Mutex<Vec<Weak<AtomicBool>>>>,
    /// On-disk copies of the built-in Garden files, shared across all
    /// sessions of this connection so `lookup` responses can point at
    /// real files for built-in symbols.
    temp_built_in_files: Arc<Option<TempBuiltInFiles>>,
}

impl Connection {
    fn new(response_tx: Sender<Value>) -> Self {
        Self::with_temp_built_in_files(response_tx, Arc::new(None))
    }

    fn with_temp_built_in_files(
        response_tx: Sender<Value>,
        temp_built_in_files: Arc<Option<TempBuiltInFiles>>,
    ) -> Self {
        Self {
            sessions: HashMap::new(),
            next_id: 0,
            response_tx,
            interrupt_flags: Arc::new(Mutex::new(Vec::new())),
            temp_built_in_files,
        }
    }

    fn new_session(&mut self) -> String {
        let id = next_session_id(&mut self.next_id);
        let (request_tx, request_rx) = mpsc::channel();
        let interrupted = Arc::new(AtomicBool::new(false));

        self.interrupt_flags
            .lock()
            .unwrap()
            .push(Arc::downgrade(&interrupted));

        let response_tx = self.response_tx.clone();
        let worker_interrupted = Arc::clone(&interrupted);
        let temp_built_in_files = Arc::clone(&self.temp_built_in_files);
        let thread_name = format!("nrepl-session-{id}");

        thread::Builder::new()
            .name(thread_name)
            .spawn(move || {
                session_worker(
                    request_rx,
                    response_tx,
                    worker_interrupted,
                    temp_built_in_files,
                )
            })
            .expect("Could not spawn nREPL session worker");

        self.sessions.insert(
            id.clone(),
            SessionState {
                request_tx,
                interrupted,
            },
        );
        id
    }

    fn close_session(&mut self, id: &str) {
        // Wake any in-progress eval so the worker shuts down
        // promptly once we drop the request channel.
        if let Some(s) = self.sessions.get(id) {
            s.interrupted.store(true, Ordering::SeqCst);
        }
        self.sessions.remove(id);
    }

    fn send(&self, msg: Value) {
        let _ = self.response_tx.send(msg);
    }
}

/// Dispatch a session-bound request to the worker thread for
/// `session_id`. Sends an `unknown-session` response on `base` if the
/// session does not exist or its worker has exited.
fn dispatch_to_session(
    conn: &Connection,
    session_id: Option<&str>,
    base: HashMap<Vec<u8>, Value>,
    build_request: impl FnOnce(HashMap<Vec<u8>, Value>) -> SessionRequest,
) {
    let session = session_id.and_then(|s| conn.sessions.get(s));
    let Some(session) = session else {
        let mut msg = base;
        msg.insert(
            b"status".to_vec(),
            Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
        );
        conn.send(Value::Dict(msg));
        return;
    };

    let req = build_request(base.clone());
    if session.request_tx.send(req).is_err() {
        let mut msg = base;
        msg.insert(
            b"status".to_vec(),
            Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
        );
        conn.send(Value::Dict(msg));
    }
}

/// Poll the global SIGINT flag and propagate it to every active
/// session in this connection. The connection's Ctrl-C handler in
/// `main.rs` sets the flag; we clear it via `swap` so each press of
/// Ctrl-C triggers exactly one round of interrupts.
fn sigint_watchdog(
    global_interrupted: Arc<AtomicBool>,
    interrupt_flags: Weak<Mutex<Vec<Weak<AtomicBool>>>>,
) {
    loop {
        thread::sleep(Duration::from_millis(100));
        let Some(flags) = interrupt_flags.upgrade() else {
            return;
        };
        if global_interrupted.swap(false, Ordering::SeqCst) {
            let mut guard = flags.lock().unwrap();
            guard.retain(|w| match w.upgrade() {
                Some(arc) => {
                    arc.store(true, Ordering::SeqCst);
                    true
                }
                None => false,
            });
        }
    }
}

/// Worker thread that owns the `Env` for a session and handles each
/// session-bound op sequentially.
fn session_worker(
    request_rx: Receiver<SessionRequest>,
    response_tx: Sender<Value>,
    interrupted: Arc<AtomicBool>,
    temp_built_in_files: Arc<Option<TempBuiltInFiles>>,
) {
    let id_gen = IdGenerator::default();
    let vfs = Vfs::default();
    let mut env = Env::new(id_gen, vfs);

    while let Ok(req) = request_rx.recv() {
        // Clear any stray interrupt set while the session was idle.
        interrupted.store(false, Ordering::SeqCst);

        let stdout_buf = Arc::new(Mutex::new(String::new()));
        let stderr_buf = Arc::new(Mutex::new(String::new()));
        let session = Session {
            interrupted: Arc::clone(&interrupted),
            stdout_stderr_mode: StdoutStderrMode::WriteToNReplBuffers {
                stdout_buf: Arc::clone(&stdout_buf),
                stderr_buf: Arc::clone(&stderr_buf),
            },
            start_time: Instant::now(),
            trace_exprs: false,
            pretty_print_json: false,
        };

        let responses = match req {
            SessionRequest::Eval {
                code,
                base_msg,
                print_opts,
            } => handle_eval(
                &code,
                &mut env,
                &session,
                &stdout_buf,
                &stderr_buf,
                &response_tx,
                &base_msg,
                &print_opts,
            ),
            SessionRequest::LoadFile {
                code,
                file_path,
                base_msg,
                print_opts,
            } => handle_load_file(
                &code,
                file_path.as_deref(),
                &mut env,
                &session,
                &stdout_buf,
                &stderr_buf,
                &response_tx,
                &base_msg,
                &print_opts,
            ),
            SessionRequest::Completions { prefix, base_msg } => {
                handle_completions(&env, &prefix, &base_msg)
            }
            SessionRequest::Lookup { sym, base_msg } => {
                handle_lookup(&env, &sym, &base_msg, temp_built_in_files.as_ref().as_ref())
            }
        };
        for r in responses {
            if response_tx.send(r).is_err() {
                return;
            }
        }
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
        "interrupt",
        OpDescriptor {
            doc: "Attempts to interrupt any currently-running eval on the given session.",
            requires: &[(
                "session",
                "The ID of the session whose eval should be interrupted.",
            )],
            optional: &[],
            returns: &[],
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

/// Handle a single request message from the client. Responses are
/// queued on the connection's response channel; for `eval`, responses
/// arrive later from the session worker.
fn handle_message(conn: &mut Connection, request: &HashMap<Vec<u8>, Value>) {
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
            conn.send(Value::Dict(msg));
        }
        "clone" => {
            let new_id = conn.new_session();
            let mut msg = base;
            msg.insert(b"new-session".to_vec(), bstr(new_id));
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            conn.send(Value::Dict(msg));
        }
        "close" => {
            if let Some(s) = dict_get(request, "session").and_then(as_str) {
                conn.close_session(s);
            }
            let mut msg = base;
            msg.insert(
                b"status".to_vec(),
                Value::List(vec![bstr("done"), bstr("session-closed")]),
            );
            conn.send(Value::Dict(msg));
        }
        "ls-sessions" => {
            let sessions: Vec<Value> = conn.sessions.keys().map(|s| bstr(s.clone())).collect();
            let mut msg = base;
            msg.insert(b"sessions".to_vec(), Value::List(sessions));
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
            conn.send(Value::Dict(msg));
        }
        "eval" => {
            let session_id = dict_get(request, "session").and_then(as_str);
            let code = dict_get(request, "code")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();
            let print_opts = parse_print_options(request);
            dispatch_to_session(conn, session_id, base, |base_msg| SessionRequest::Eval {
                code,
                base_msg,
                print_opts,
            });
        }
        "interrupt" => {
            let session_id = dict_get(request, "session").and_then(as_str);
            match session_id.and_then(|s| conn.sessions.get(s)) {
                Some(s) => {
                    s.interrupted.store(true, Ordering::SeqCst);
                    let mut msg = base;
                    msg.insert(b"status".to_vec(), Value::List(vec![bstr("done")]));
                    conn.send(Value::Dict(msg));
                }
                None => {
                    let mut msg = base;
                    msg.insert(
                        b"status".to_vec(),
                        Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-session")]),
                    );
                    conn.send(Value::Dict(msg));
                }
            }
        }
        "load-file" => {
            let session_id = dict_get(request, "session").and_then(as_str);
            let code = dict_get(request, "file")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();
            let file_path = dict_get(request, "file-path")
                .and_then(as_str)
                .or_else(|| dict_get(request, "file-name").and_then(as_str))
                .map(|s| s.to_owned());
            let print_opts = parse_print_options(request);
            dispatch_to_session(conn, session_id, base, |base_msg| {
                SessionRequest::LoadFile {
                    code,
                    file_path,
                    base_msg,
                    print_opts,
                }
            });
        }
        "completions" => {
            let session_id = dict_get(request, "session").and_then(as_str);
            let prefix = dict_get(request, "prefix")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();
            dispatch_to_session(conn, session_id, base, |base_msg| {
                SessionRequest::Completions { prefix, base_msg }
            });
        }
        "lookup" => {
            let session_id = dict_get(request, "session").and_then(as_str);
            let sym = dict_get(request, "sym")
                .and_then(as_str)
                .unwrap_or("")
                .to_owned();
            dispatch_to_session(conn, session_id, base, |base_msg| SessionRequest::Lookup {
                sym,
                base_msg,
            });
        }
        "" => {
            let mut msg = base;
            msg.insert(b"status".to_vec(), Value::List(vec![bstr("error")]));
            msg.insert(b"err".to_vec(), bstr("Missing 'op' in request.\n"));
            conn.send(Value::Dict(msg));
        }
        other => {
            let mut msg = base;
            msg.insert(
                b"status".to_vec(),
                Value::List(vec![bstr("done"), bstr("error"), bstr("unknown-op")]),
            );
            msg.insert(b"err".to_vec(), bstr(format!("Unknown op: {other}\n")));
            conn.send(Value::Dict(msg));
        }
    }
}

/// Writer thread: pulls responses off the channel and writes them to
/// the socket. Exits when all senders are dropped.
fn writer_thread(mut writer: TcpStream, rx: Receiver<Value>) {
    while let Ok(value) = rx.recv() {
        if let Err(e) = write_message(&mut writer, &value) {
            error!("nREPL: error writing response: {e}");
            return;
        }
        if let Err(e) = writer.flush() {
            error!("nREPL: error flushing writer: {e}");
            return;
        }
    }
}

/// Serve a single TCP connection until the client disconnects.
fn serve_connection(
    stream: TcpStream,
    interrupted: Arc<AtomicBool>,
    temp_built_in_files: Arc<Option<TempBuiltInFiles>>,
) {
    let peer = stream
        .peer_addr()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| "<unknown>".to_owned());
    info!("nREPL: client connected from {peer}");

    let writer_stream = stream.try_clone().expect("Could not clone TCP stream");
    let mut reader = BufReader::new(stream);

    let (response_tx, response_rx) = mpsc::channel();

    let writer_handle = thread::Builder::new()
        .name("nrepl-writer".to_owned())
        .spawn(move || writer_thread(writer_stream, response_rx))
        .expect("Could not spawn nREPL writer thread");

    let mut conn = Connection::with_temp_built_in_files(response_tx, temp_built_in_files);

    let watchdog_flags = Arc::downgrade(&conn.interrupt_flags);
    thread::Builder::new()
        .name("nrepl-sigint-watchdog".to_owned())
        .spawn(move || sigint_watchdog(interrupted, watchdog_flags))
        .expect("Could not spawn nREPL SIGINT watchdog");

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

        handle_message(&mut conn, &request);
    }

    // Dropping `conn` drops every session's request channel, which
    // makes each worker exit once its current eval finishes. That in
    // turn drops their `response_tx` clones, and once the last sender
    // is gone the writer thread exits.
    for s in conn.sessions.values() {
        s.interrupted.store(true, Ordering::SeqCst);
    }
    drop(conn);
    let _ = writer_handle.join();

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
    use serde_json::{json, Value as J};

    let (tx, rx) = mpsc::channel();
    let mut conn = Connection::new(tx);

    let mut request_count = 0;
    for line in src.lines() {
        let trimmed = line.trim();

        // Test directive: `// sleep <ms>` pauses the reftest harness
        // for the given number of milliseconds. Useful when a request
        // needs to land while a previous async op is still in flight,
        // e.g. firing `interrupt` against a running `eval`.
        if let Some(rest) = trimmed.strip_prefix("// sleep ") {
            if let Ok(ms) = rest.trim().parse::<u64>() {
                std::thread::sleep(Duration::from_millis(ms));
            }
            continue;
        }

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

        handle_message(&mut conn, &request_dict);
        request_count += 1;
    }

    // Each request produces exactly one terminal response — a
    // message carrying a `status` field. Drain the channel until
    // we've seen one per request so the fixture captures
    // asynchronous worker responses too.
    let mut responses = Vec::new();
    let mut status_seen = 0;
    while status_seen < request_count {
        let Ok(response) = rx.recv_timeout(Duration::from_secs(5)) else {
            break;
        };
        let has_status = matches!(
            &response,
            Value::Dict(d) if d.contains_key(b"status".as_slice())
        );
        responses.push(response);
        if has_status {
            status_seen += 1;
        }
    }

    // Stable-sort by the request `id` field, so responses produced
    // by different requests never interleave in the output even if
    // the worker thread and the main thread race on the wire. Within
    // a single id, the worker's send order is preserved.
    responses.sort_by_key(|r| match r {
        Value::Dict(d) => dict_get(d, "id")
            .and_then(as_str)
            .map(str::to_owned)
            .unwrap_or_default(),
        _ => String::new(),
    });

    for response in responses {
        let response_json = normalize_for_reftest(bencode_to_json(&response));
        println!("{}", serde_json::to_string_pretty(&response_json).unwrap());
    }
}

/// Removes a `.nrepl-port` file when dropped.
///
/// Following the nREPL convention, the server writes its bound port
/// to `.nrepl-port` in the current working directory so that editor
/// tooling (CIDER, Calva, etc.) can auto-discover the server. The
/// file must be removed when the server exits.
struct PortFile {
    path: PathBuf,
}

impl PortFile {
    fn write(port: u16) -> io::Result<Self> {
        let path = PathBuf::from(".nrepl-port");
        fs::write(&path, port.to_string())?;
        Ok(Self { path })
    }
}

impl Drop for PortFile {
    fn drop(&mut self) {
        if let Err(e) = fs::remove_file(&self.path) {
            if e.kind() != io::ErrorKind::NotFound {
                warn!("nREPL: could not remove {}: {e}", self.path.display());
            }
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

    let local_addr = listener.local_addr().ok();
    let bound_port = local_addr.map(|a| a.port()).unwrap_or(port);
    let local = local_addr
        .map(|a| a.to_string())
        .unwrap_or_else(|| addr.clone());
    info!("nREPL server started on {local}");

    let _port_file = match PortFile::write(bound_port) {
        Ok(pf) => Some(pf),
        Err(e) => {
            warn!("nREPL: could not write .nrepl-port: {e}");
            None
        }
    };

    let temp_built_in_files = Arc::new(match TempBuiltInFiles::new("garden-nrepl") {
        Ok(b) => Some(b),
        Err(e) => {
            warn!("nREPL: could not write built-in files to a temporary directory: {e}");
            None
        }
    });

    // Use a non-blocking listener so the accept loop can periodically
    // check the interrupted flag and shut down on Ctrl-C.
    if let Err(e) = listener.set_nonblocking(true) {
        error!("nREPL: could not set listener to non-blocking: {e}");
        std::process::exit(1);
    }

    loop {
        if interrupted.load(Ordering::SeqCst) {
            info!("nREPL: shutting down");
            return;
        }

        match listener.accept() {
            Ok((stream, _)) => {
                let interrupted = Arc::clone(&interrupted);
                let temp_built_in_files = Arc::clone(&temp_built_in_files);
                std::thread::Builder::new()
                    .name("nrepl-client".to_owned())
                    .spawn(move || serve_connection(stream, interrupted, temp_built_in_files))
                    .expect("Could not spawn nREPL client thread");
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                std::thread::sleep(Duration::from_millis(100));
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

    /// Read all messages with a matching `id` field from the response
    /// channel, until a `status` containing `done` is seen.
    fn collect_until_done(rx: &Receiver<Value>, id: &str) -> Vec<HashMap<Vec<u8>, Value>> {
        let mut out = Vec::new();
        loop {
            let v = rx
                .recv_timeout(Duration::from_secs(5))
                .expect("timeout waiting for response");
            let Value::Dict(d) = v else {
                continue;
            };
            let msg_id = dict_get(&d, "id").and_then(as_str).unwrap_or("");
            if msg_id != id {
                continue;
            }
            let is_done = match dict_get(&d, "status") {
                Some(Value::List(items)) => items
                    .iter()
                    .any(|i| as_str(i).map(|s| s == "done").unwrap_or(false)),
                _ => false,
            };
            out.push(d);
            if is_done {
                return out;
            }
        }
    }

    #[test]
    fn interrupt_aborts_eval() {
        // Spin up a Connection without a real TCP socket and verify
        // that an interrupt op set on the session causes a long
        // running eval to terminate with `interrupted` status.
        let (tx, rx) = mpsc::channel();
        let mut conn = Connection::new(tx);

        let clone_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("clone")),
            (b"id".to_vec(), bstr("c1")),
        ]);
        handle_message(&mut conn, &clone_req);
        let clone_resp = collect_until_done(&rx, "c1");
        let new_session = clone_resp
            .iter()
            .find_map(|d| dict_get(d, "new-session").and_then(as_str))
            .expect("clone produced a new-session id")
            .to_owned();

        // Start an infinite loop, then interrupt it.
        let eval_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("eval")),
            (b"id".to_vec(), bstr("e1")),
            (b"session".to_vec(), bstr(new_session.clone())),
            (b"code".to_vec(), bstr("while True { 1 }")),
        ]);
        handle_message(&mut conn, &eval_req);

        // Give the worker a moment to enter the loop.
        std::thread::sleep(Duration::from_millis(500));

        let int_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("interrupt")),
            (b"id".to_vec(), bstr("i1")),
            (b"session".to_vec(), bstr(new_session.clone())),
        ]);
        handle_message(&mut conn, &int_req);

        let eval_resp = collect_until_done(&rx, "e1");
        let done = eval_resp.last().expect("eval produced a done message");
        let status = match dict_get(done, "status") {
            Some(Value::List(items)) => items
                .iter()
                .filter_map(|i| as_str(i).map(|s| s.to_owned()))
                .collect::<Vec<_>>(),
            _ => panic!("expected status list"),
        };
        assert!(
            status.iter().any(|s| s == "interrupted"),
            "expected interrupted in status, got {status:?}"
        );
        assert!(
            !status.iter().any(|s| s == "eval-error"),
            "interrupt should not be reported as eval-error: {status:?}"
        );
    }

    #[test]
    fn streams_output_during_eval() {
        // An eval that prints in an infinite loop should stream `out`
        // messages while it runs, rather than buffering everything
        // until completion.
        let (tx, rx) = mpsc::channel();
        let mut conn = Connection::new(tx);

        let clone_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("clone")),
            (b"id".to_vec(), bstr("c1")),
        ]);
        handle_message(&mut conn, &clone_req);
        let clone_resp = collect_until_done(&rx, "c1");
        let new_session = clone_resp
            .iter()
            .find_map(|d| dict_get(d, "new-session").and_then(as_str))
            .expect("clone produced a new-session id")
            .to_owned();

        let eval_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("eval")),
            (b"id".to_vec(), bstr("e1")),
            (b"session".to_vec(), bstr(new_session.clone())),
            (b"code".to_vec(), bstr("while True { println(\"tick\") }")),
        ]);
        handle_message(&mut conn, &eval_req);

        // We should receive at least one `out` message before the
        // eval produces its terminal `done`, proving the output is
        // streamed rather than buffered until the end.
        loop {
            let v = rx
                .recv_timeout(Duration::from_secs(5))
                .expect("timeout waiting for streamed output");
            let Value::Dict(d) = v else {
                continue;
            };
            assert!(
                dict_get(&d, "status").is_none(),
                "eval finished before any output was streamed"
            );
            if let Some(out) = dict_get(&d, "out").and_then(as_str) {
                assert!(out.contains("tick"));
                break;
            }
        }

        // Stop the loop so the worker can shut down.
        let int_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("interrupt")),
            (b"id".to_vec(), bstr("i1")),
            (b"session".to_vec(), bstr(new_session)),
        ]);
        handle_message(&mut conn, &int_req);
        let _ = collect_until_done(&rx, "e1");
    }

    #[test]
    fn sigint_aborts_eval() {
        // Verify that setting the global SIGINT flag — the same
        // signal Ctrl-C sets in main — propagates via the watchdog
        // and interrupts a running eval.
        let (tx, rx) = mpsc::channel();
        let mut conn = Connection::new(tx);

        let global = Arc::new(AtomicBool::new(false));
        let watchdog_flags = Arc::downgrade(&conn.interrupt_flags);
        let global_for_watchdog = Arc::clone(&global);
        thread::spawn(move || sigint_watchdog(global_for_watchdog, watchdog_flags));

        let clone_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("clone")),
            (b"id".to_vec(), bstr("c1")),
        ]);
        handle_message(&mut conn, &clone_req);
        let clone_resp = collect_until_done(&rx, "c1");
        let new_session = clone_resp
            .iter()
            .find_map(|d| dict_get(d, "new-session").and_then(as_str))
            .expect("clone produced a new-session id")
            .to_owned();

        let eval_req: HashMap<Vec<u8>, Value> = HashMap::from([
            (b"op".to_vec(), bstr("eval")),
            (b"id".to_vec(), bstr("e1")),
            (b"session".to_vec(), bstr(new_session)),
            (b"code".to_vec(), bstr("while True { 1 }")),
        ]);
        handle_message(&mut conn, &eval_req);

        // Let the worker enter the loop, then fire SIGINT.
        std::thread::sleep(Duration::from_millis(500));
        global.store(true, Ordering::SeqCst);

        let eval_resp = collect_until_done(&rx, "e1");
        let done = eval_resp.last().expect("eval produced a done message");
        let status = match dict_get(done, "status") {
            Some(Value::List(items)) => items
                .iter()
                .filter_map(|i| as_str(i).map(|s| s.to_owned()))
                .collect::<Vec<_>>(),
            _ => panic!("expected status list"),
        };
        assert!(
            status.iter().any(|s| s == "interrupted"),
            "expected interrupted in status, got {status:?}"
        );
    }
}
