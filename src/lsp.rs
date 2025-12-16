//! LSP (Language Server Protocol) support for Garden.

use serde::Deserialize;
use std::io::{self, BufRead, Read, Write};
use std::path::PathBuf;

use crate::checks::type_checker::check_types;
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::parser::ast::IdGenerator;
use crate::parser::parse_toplevel_items;
use crate::parser::position::Position;
use crate::parser::vfs::Vfs;
use crate::pos_to_id::find_item_at;

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
                "definitionProvider": true
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
            Some("textDocument/definition") => {
                if let Some(id) = parsed.id {
                    let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                    let response = handle_definition(id, params);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
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
