//! LSP (Language Server Protocol) support for Garden.

use serde::Deserialize;
use std::io::{self, BufRead, Read, Write};
use std::path::PathBuf;

use crate::checks::check_toplevel_items_in_env;
use crate::checks::type_checker::check_types;
use crate::diagnostics::{Diagnostic, Severity};
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::parser::ast::{AstId, Expression_, IdGenerator};
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::parser::{parse_toplevel_items, ParseError};
use crate::pos_to_id::{find_expr_of_id, find_item_at};
use crate::types::TypeDef;

#[derive(Debug, Deserialize)]
struct Message {
    #[allow(dead_code)]
    jsonrpc: String,
    #[serde(default)]
    id: Option<serde_json::Value>,
    method: Option<String>,
    #[allow(dead_code)]
    #[serde(default)]
    params: Option<serde_json::Value>,
}

/// Read a single LSP message from stdin.
fn read_message() -> io::Result<Option<serde_json::Value>> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut content_length = None;

    // Read headers
    loop {
        let mut header = String::new();
        let bytes_read = stdin.read_line(&mut header)?;

        if bytes_read == 0 {
            // EOF
            return Ok(None);
        }

        let header = header.trim();

        if header.is_empty() {
            // Empty line marks end of headers
            break;
        }

        if let Some(length_str) = header.strip_prefix("Content-Length: ") {
            content_length = Some(length_str.parse::<usize>().map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Invalid Content-Length: {e}"),
                )
            })?);
        }
    }

    let content_length = content_length.ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidData, "Missing Content-Length header")
    })?;

    // Read the JSON content
    let mut buffer = vec![0u8; content_length];
    stdin.read_exact(&mut buffer)?;

    let message: serde_json::Value = serde_json::from_slice(&buffer)?;
    Ok(Some(message))
}

/// Write an LSP message to stdout.
fn write_message(message: &serde_json::Value) -> io::Result<()> {
    let content = serde_json::to_string(message)?;
    let response = format!("Content-Length: {}\r\n\r\n{}", content.len(), content);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    stdout.write_all(response.as_bytes())?;
    stdout.flush()?;

    Ok(())
}

/// Convert a file path to an LSP URI.
fn path_to_uri(path: &PathBuf) -> String {
    format!("file://{}", path.display())
}

/// Get diagnostics for a file.
fn get_diagnostics(src: &str, path: &PathBuf) -> Vec<serde_json::Value> {
    let mut diagnostics = vec![];

    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    // Collect parse errors
    for e in errors.into_iter() {
        let (position, message) = match e {
            ParseError::Invalid {
                position, message, ..
            } => (position, message.as_string()),
            ParseError::Incomplete {
                position, message, ..
            } => (position, message.as_string()),
        };

        diagnostics.push(serde_json::json!({
            "range": {
                "start": {
                    "line": position.line_number,
                    "character": position.column
                },
                "end": {
                    "line": position.end_line_number,
                    "character": position.end_column
                }
            },
            "severity": 1, // Error
            "message": message
        }));
    }

    // If no parse errors, check for semantic errors
    if diagnostics.is_empty() {
        let mut env = Env::new(id_gen, vfs);
        let ns = env.get_or_create_namespace(path);
        let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, ns.clone());
        raw_diagnostics.extend(check_toplevel_items_in_env(&vfs_path, &items, &env, ns));

        for Diagnostic {
            message,
            position,
            severity,
            ..
        } in raw_diagnostics
        {
            let lsp_severity = match severity {
                Severity::Error => 1,
                Severity::Warning => 2,
            };

            diagnostics.push(serde_json::json!({
                "range": {
                    "start": {
                        "line": position.line_number,
                        "character": position.column
                    },
                    "end": {
                        "line": position.end_line_number,
                        "character": position.end_column
                    }
                },
                "severity": lsp_severity,
                "message": message.as_string()
            }));
        }
    }

    diagnostics
}

/// Get completion items at the given offset.
fn get_completions(src: &str, path: &PathBuf, offset: usize) -> Vec<serde_json::Value> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let ids_at_pos = find_item_at(&items, offset, offset);
    let summary = check_types(&vfs_path, &items, &env, ns);

    let mut completion_items = vec![];

    for id in ids_at_pos.iter().rev() {
        let AstId::Expr(expr_id) = id else {
            continue;
        };
        let Some(expr) = find_expr_of_id(&items, *expr_id) else {
            break;
        };
        match &expr.expr_ {
            Expression_::DotAccess(recv, meth_sym) => {
                let recv_id = recv.id;
                let recv_ty = &summary.id_to_ty[&recv_id];

                let prefix = if meth_sym.name.is_placeholder() {
                    ""
                } else {
                    &meth_sym.name.text
                };

                completion_items.extend(get_method_completions(&env, recv_ty, prefix));
            }
            _ => {}
        }
    }

    completion_items
}

/// Get method and field completions for a given type.
fn get_method_completions(env: &Env, recv_ty: &Type, prefix: &str) -> Vec<serde_json::Value> {
    let Some(type_name) = recv_ty.type_name() else {
        return vec![];
    };

    let empty_hashmap = rustc_hash::FxHashMap::default();
    let methods = env
        .types
        .get(&type_name)
        .map(|tdm| &tdm.methods)
        .unwrap_or(&empty_hashmap);

    let mut items = vec![];

    // Add methods
    for (method_name, meth_info) in methods.iter() {
        if !method_name.text.starts_with(prefix) {
            continue;
        }

        let Some(fun_info) = meth_info.fun_info() else {
            continue;
        };

        let params = &fun_info
            .params
            .params
            .iter()
            .map(|param| match &param.hint {
                Some(hint) => hint.as_src(),
                None => "_".to_owned(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_hint = match &fun_info.return_hint {
            Some(hint) => format!(": {}", hint.as_src()),
            None => "".to_owned(),
        };

        let (label, insert_text) = if params.is_empty() {
            // Complete parentheses too if there are zero parameters.
            let name = format!("{}()", method_name.text);
            (name.clone(), name)
        } else {
            (
                method_name.text.clone(),
                format!("{}($0)", method_name.text),
            )
        };

        let detail = format!("({params}){return_hint}");

        items.push(serde_json::json!({
            "label": label,
            "kind": 2, // Method
            "detail": detail,
            "insertText": insert_text,
            "insertTextFormat": 2, // Snippet
        }));
    }

    // Add struct fields
    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&type_name) {
        for field in &struct_info.fields {
            if !field.sym.name.text.starts_with(prefix) {
                continue;
            }

            items.push(serde_json::json!({
                "label": field.sym.name.text,
                "kind": 5, // Field
                "detail": format!(": {}", field.hint.as_src()),
            }));
        }
    }

    items
}

/// Get the definition position for a symbol at the given offset.
fn get_definition(src: &str, path: &PathBuf, offset: usize) -> Option<Position> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let summary = check_types(&vfs_path, &items, &env, ns);

    let mut ids_at_query_pos = vec![];
    if offset > 0 {
        ids_at_query_pos.extend(find_item_at(&items, offset - 1, offset - 1));
    }
    ids_at_query_pos.extend(find_item_at(&items, offset, offset));

    for id in ids_at_query_pos.iter().rev() {
        if let Some(pos) = summary.id_to_def_pos.get(&id.id()) {
            return Some(pos.clone());
        }
    }

    None
}

/// Handle an initialize request.
fn handle_initialize(id: serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": {
            "capabilities": {
                "definitionProvider": true,
                "completionProvider": {
                    "triggerCharacters": ["."]
                },
                "textDocumentSync": {
                    "openClose": true,
                    "change": 1 // Full document sync
                }
            },
            "serverInfo": {
                "name": "garden-lsp",
                "version": "0.1.0"
            }
        }
    })
}

/// Handle a shutdown request.
fn handle_shutdown(id: serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": null
    })
}

/// Send diagnostics for a file.
fn send_diagnostics(uri: &str, diagnostics: Vec<serde_json::Value>) -> io::Result<()> {
    let notification = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": {
            "uri": uri,
            "diagnostics": diagnostics
        }
    });

    write_message(&notification)
}

/// Handle textDocument/didOpen notification.
fn handle_did_open(params: &serde_json::Value) -> io::Result<()> {
    let uri = match params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|u| u.as_str())
    {
        Some(uri) => uri,
        None => return Ok(()),
    };

    let text = match params
        .get("textDocument")
        .and_then(|doc| doc.get("text"))
        .and_then(|t| t.as_str())
    {
        Some(text) => text,
        None => return Ok(()),
    };

    // Convert URI to path
    let path = match uri.strip_prefix("file://") {
        Some(p) => PathBuf::from(p),
        None => return Ok(()),
    };

    // Get diagnostics and send them
    let diagnostics = get_diagnostics(text, &path);
    send_diagnostics(uri, diagnostics)
}

/// Handle textDocument/didChange notification.
fn handle_did_change(params: &serde_json::Value) -> io::Result<()> {
    let uri = match params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|u| u.as_str())
    {
        Some(uri) => uri,
        None => return Ok(()),
    };

    // Get the new text from contentChanges (full document sync)
    let text = match params
        .get("contentChanges")
        .and_then(|changes| changes.as_array())
        .and_then(|arr| arr.first())
        .and_then(|change| change.get("text"))
        .and_then(|t| t.as_str())
    {
        Some(text) => text,
        None => return Ok(()),
    };

    // Convert URI to path
    let path = match uri.strip_prefix("file://") {
        Some(p) => PathBuf::from(p),
        None => return Ok(()),
    };

    // Get diagnostics and send them
    let diagnostics = get_diagnostics(text, &path);
    send_diagnostics(uri, diagnostics)
}

/// Handle a textDocument/completion request.
fn handle_completion(id: serde_json::Value, params: &serde_json::Value) -> serde_json::Value {
    // Extract the text document URI and position from params
    let uri = match params.get("textDocument").and_then(|doc| doc.get("uri")) {
        Some(serde_json::Value::String(uri)) => uri,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    let position = match params.get("position") {
        Some(pos) => pos,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    let line = match position.get("line").and_then(|l| l.as_u64()) {
        Some(l) => l as usize,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    let character = match position.get("character").and_then(|c| c.as_u64()) {
        Some(c) => c as usize,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    // Convert file:// URI to path
    let path = match uri.strip_prefix("file://") {
        Some(p) => PathBuf::from(p),
        None => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    // Read the file
    let src = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": []
            });
        }
    };

    // Convert line/character to offset
    let offset = line_char_to_offset(&src, line, character);

    // Get completions
    let items = get_completions(&src, &path, offset);

    serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": items
    })
}

/// Handle a textDocument/definition request.
fn handle_definition(id: serde_json::Value, params: &serde_json::Value) -> serde_json::Value {
    // Extract the text document URI and position from params
    let uri = match params.get("textDocument").and_then(|doc| doc.get("uri")) {
        Some(serde_json::Value::String(uri)) => uri,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    let position = match params.get("position") {
        Some(pos) => pos,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    let line = match position.get("line").and_then(|l| l.as_u64()) {
        Some(l) => l as usize,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    let character = match position.get("character").and_then(|c| c.as_u64()) {
        Some(c) => c as usize,
        _ => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    // Convert file:// URI to path
    let path = match uri.strip_prefix("file://") {
        Some(p) => PathBuf::from(p),
        None => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    // Read the file
    let src = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    // Convert line/character to offset
    let offset = line_char_to_offset(&src, line, character);

    // Get the definition position
    let def_pos = match get_definition(&src, &path, offset) {
        Some(pos) => pos,
        None => {
            return serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            });
        }
    };

    // Convert to LSP Location format
    serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": {
            "uri": path_to_uri(&def_pos.path),
            "range": {
                "start": {
                    "line": def_pos.line_number,
                    "character": def_pos.column
                },
                "end": {
                    "line": def_pos.end_line_number,
                    "character": def_pos.end_column
                }
            }
        }
    })
}

/// Convert line and character position to byte offset in the source.
fn line_char_to_offset(src: &str, line: usize, character: usize) -> usize {
    let mut current_line = 0;
    let mut offset = 0;

    for (idx, ch) in src.char_indices() {
        if current_line == line {
            if character == 0 {
                return idx;
            }
            // Count characters on this line
            let line_start = idx;
            let mut char_count = 0;
            for (char_idx, ch) in src[line_start..].char_indices() {
                if ch == '\n' {
                    break;
                }
                if char_count == character {
                    return line_start + char_idx;
                }
                char_count += 1;
            }
            return idx;
        }

        if ch == '\n' {
            current_line += 1;
        }
        offset = idx;
    }

    offset
}

/// Run the LSP server.
pub(crate) fn run_lsp() {
    let mut should_exit = false;

    loop {
        let message = match read_message() {
            Ok(Some(msg)) => msg,
            Ok(None) => {
                // EOF reached
                break;
            }
            Err(e) => {
                eprintln!("Error reading message: {e}");
                continue;
            }
        };

        // Parse the message
        let parsed: Message = match serde_json::from_value(message.clone()) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("Error parsing message: {e}");
                continue;
            }
        };

        match parsed.method.as_deref() {
            Some("initialize") => {
                if let Some(id) = parsed.id {
                    let response = handle_initialize(id);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("initialized") => {
                // This is a notification, no response needed
            }
            Some("textDocument/completion") => {
                if let Some(id) = parsed.id {
                    let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                    let response = handle_completion(id, params);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/definition") => {
                if let Some(id) = parsed.id {
                    let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                    let response = handle_definition(id, params);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/didOpen") => {
                let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                if let Err(e) = handle_did_open(params) {
                    eprintln!("Error handling didOpen: {e}");
                }
            }
            Some("textDocument/didChange") => {
                let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                if let Err(e) = handle_did_change(params) {
                    eprintln!("Error handling didChange: {e}");
                }
            }
            Some("shutdown") => {
                if let Some(id) = parsed.id {
                    let response = handle_shutdown(id);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
                should_exit = true;
            }
            Some("exit") => {
                // Exit notification
                break;
            }
            _ => {
                // Ignore other methods
            }
        }

        if should_exit {
            break;
        }
    }
}
