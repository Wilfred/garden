//! LSP (Language Server Protocol) support for Garden.

use lsp_types::request::{Completion, GotoDefinition, Initialize, Request, Shutdown};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, Diagnostic,
    DiagnosticSeverity, GotoDefinitionParams, InitializeParams, InitializeResult, InsertTextFormat,
    Location, Position, PublishDiagnosticsParams, Range, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::io::{self, BufRead, Read, Write};
use std::path::PathBuf;
use url::Url;

/// Storage for open document contents, keyed by file path.
type DocumentStore = FxHashMap<PathBuf, String>;

use crate::checks::check_toplevel_items_in_env;
use crate::checks::type_checker::check_types;
use crate::diagnostics::{Diagnostic as GardenDiagnostic, Severity};
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::garden_type::Type;
use crate::parser::ast::{AstId, Expression_, IdGenerator};
use crate::parser::position::Position as GardenPosition;
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

/// JSON-RPC 2.0 response structure.
#[derive(Debug, Serialize)]
struct JsonRpcResponse<T> {
    jsonrpc: &'static str,
    id: serde_json::Value,
    result: T,
}

impl<T: Serialize> JsonRpcResponse<T> {
    fn new(id: serde_json::Value, result: T) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result,
        }
    }
}

/// JSON-RPC 2.0 notification structure.
#[derive(Debug, Serialize)]
struct JsonRpcNotification<T> {
    jsonrpc: &'static str,
    method: &'static str,
    params: T,
}

impl<T: Serialize> JsonRpcNotification<T> {
    fn new(method: &'static str, params: T) -> Self {
        Self {
            jsonrpc: "2.0",
            method,
            params,
        }
    }
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
fn write_message<T: Serialize>(message: &T) -> io::Result<()> {
    let content = serde_json::to_string(message)?;
    let response = format!("Content-Length: {}\r\n\r\n{}", content.len(), content);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    stdout.write_all(response.as_bytes())?;
    stdout.flush()?;

    Ok(())
}

/// Convert a file path to an LSP URI.
fn path_to_uri(path: &PathBuf) -> Uri {
    let url = Url::from_file_path(path).unwrap_or_else(|_| {
        // Fallback to file:// scheme with display
        Url::parse(&format!("file://{}", path.display())).unwrap()
    });
    url.as_str().parse().unwrap()
}

/// Convert a Garden position to an LSP range.
fn garden_pos_to_lsp_range(pos: &GardenPosition) -> Range {
    Range {
        start: Position {
            line: pos.line_number as u32,
            character: pos.column as u32,
        },
        end: Position {
            line: pos.end_line_number as u32,
            character: pos.end_column as u32,
        },
    }
}

/// Get diagnostics for a file.
fn get_diagnostics(src: &str, path: &PathBuf) -> Vec<Diagnostic> {
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

        diagnostics.push(Diagnostic {
            range: garden_pos_to_lsp_range(&position),
            severity: Some(DiagnosticSeverity::ERROR),
            message,
            ..Default::default()
        });
    }

    // If no parse errors, check for semantic errors
    if diagnostics.is_empty() {
        let mut env = Env::new(id_gen, vfs);
        let ns = env.get_or_create_namespace(path);
        let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, ns.clone());
        raw_diagnostics.extend(check_toplevel_items_in_env(&vfs_path, &items, &env, ns));

        for GardenDiagnostic {
            message,
            position,
            severity,
            ..
        } in raw_diagnostics
        {
            let lsp_severity = match severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            };

            diagnostics.push(Diagnostic {
                range: garden_pos_to_lsp_range(&position),
                severity: Some(lsp_severity),
                message: message.as_string(),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Get completion items at the given offset.
fn get_completions(src: &str, path: &PathBuf, offset: usize) -> Vec<CompletionItem> {
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
fn get_method_completions(env: &Env, recv_ty: &Type, prefix: &str) -> Vec<CompletionItem> {
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

        items.push(CompletionItem {
            label,
            kind: Some(CompletionItemKind::METHOD),
            detail: Some(detail),
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Add struct fields
    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&type_name) {
        for field in &struct_info.fields {
            if !field.sym.name.text.starts_with(prefix) {
                continue;
            }

            items.push(CompletionItem {
                label: field.sym.name.text.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!(": {}", field.hint.as_src())),
                ..Default::default()
            });
        }
    }

    items
}

/// Get the definition position for a symbol at the given offset.
fn get_definition(src: &str, path: &PathBuf, offset: usize) -> Option<GardenPosition> {
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
fn handle_initialize(
    id: serde_json::Value,
    _params: InitializeParams,
) -> JsonRpcResponse<InitializeResult> {
    let result = InitializeResult {
        capabilities: ServerCapabilities {
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".to_owned()]),
                ..Default::default()
            }),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            ..Default::default()
        },
        server_info: Some(ServerInfo {
            name: "garden-lsp".to_owned(),
            version: Some("0.1.0".to_owned()),
        }),
    };

    JsonRpcResponse::new(id, result)
}

/// Handle a shutdown request.
fn handle_shutdown(id: serde_json::Value) -> JsonRpcResponse<()> {
    JsonRpcResponse::new(id, ())
}

/// Send diagnostics for a file.
fn send_diagnostics(uri: Uri, diagnostics: Vec<Diagnostic>) -> io::Result<()> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };

    let notification = JsonRpcNotification::new("textDocument/publishDiagnostics", params);

    write_message(&notification)
}

/// Handle textDocument/didOpen notification.
fn handle_did_open(params: &serde_json::Value, documents: &mut DocumentStore) -> io::Result<()> {
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
    let url = match Url::parse(uri) {
        Ok(u) => u,
        Err(_) => return Ok(()),
    };

    let path = match url.to_file_path() {
        Ok(p) => p,
        Err(_) => return Ok(()),
    };

    // Store the document content
    documents.insert(path.clone(), text.to_owned());

    // Get diagnostics and send them
    let diagnostics = get_diagnostics(text, &path);
    send_diagnostics(url.as_str().parse().unwrap(), diagnostics)
}

/// Handle textDocument/didChange notification.
fn handle_did_change(params: &serde_json::Value, documents: &mut DocumentStore) -> io::Result<()> {
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
    let url = match Url::parse(uri) {
        Ok(u) => u,
        Err(_) => return Ok(()),
    };

    let path = match url.to_file_path() {
        Ok(p) => p,
        Err(_) => return Ok(()),
    };

    // Update the stored document content
    documents.insert(path.clone(), text.to_owned());

    // Get diagnostics and send them
    let diagnostics = get_diagnostics(text, &path);
    send_diagnostics(url.as_str().parse().unwrap(), diagnostics)
}

/// Handle a textDocument/completion request.
fn handle_completion(
    id: serde_json::Value,
    params: CompletionParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Vec<CompletionItem>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    // Convert file:// URI to path
    let url = match Url::parse(uri.as_str()) {
        Ok(u) => u,
        Err(_) => return JsonRpcResponse::new(id, vec![]),
    };

    let path = match url.to_file_path() {
        Ok(p) => p,
        Err(_) => return JsonRpcResponse::new(id, vec![]),
    };

    // Get the document content from our store, falling back to disk
    let src = match documents.get(&path) {
        Some(content) => content.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => return JsonRpcResponse::new(id, vec![]),
        },
    };

    // Convert line/character to offset
    let offset = line_char_to_offset(&src, position.line as usize, position.character as usize);

    // Get completions
    let items = get_completions(&src, &path, offset);

    JsonRpcResponse::new(id, items)
}

/// Handle a textDocument/definition request.
fn handle_definition(
    id: serde_json::Value,
    params: GotoDefinitionParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Option<Location>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Convert file:// URI to path
    let url = match Url::parse(uri.as_str()) {
        Ok(u) => u,
        Err(_) => return JsonRpcResponse::new(id, None),
    };

    let path = match url.to_file_path() {
        Ok(p) => p,
        Err(_) => return JsonRpcResponse::new(id, None),
    };

    // Get the document content from our store, falling back to disk
    let src = match documents.get(&path) {
        Some(content) => content.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => return JsonRpcResponse::new(id, None),
        },
    };

    // Convert line/character to offset
    let offset = line_char_to_offset(&src, position.line as usize, position.character as usize);

    // Get the definition position
    let def_pos = match get_definition(&src, &path, offset) {
        Some(pos) => pos,
        None => return JsonRpcResponse::new(id, None),
    };

    // Convert to LSP Location format
    let location = Location {
        uri: path_to_uri(&def_pos.path),
        range: garden_pos_to_lsp_range(&def_pos),
    };

    JsonRpcResponse::new(id, Some(location))
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
            for (char_count, (char_idx, ch)) in src[line_start..].char_indices().enumerate() {
                if ch == '\n' {
                    break;
                }
                if char_count == character {
                    return line_start + char_idx;
                }
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

/// Process an LSP message and return the response, if any.
///
/// Returns a tuple of (response_to_write, notification_to_write).
fn process_message(
    message: serde_json::Value,
    documents: &mut DocumentStore,
) -> (Option<serde_json::Value>, Option<serde_json::Value>) {
    let parsed: Message = match serde_json::from_value(message.clone()) {
        Ok(m) => m,
        Err(_) => return (None, None),
    };

    match parsed.method.as_deref() {
        Some(method) if method == Initialize::METHOD => {
            if let Some(id) = parsed.id {
                let params: InitializeParams = match message
                    .get("params")
                    .and_then(|p| serde_json::from_value(p.clone()).ok())
                {
                    Some(p) => p,
                    None => return (None, None),
                };
                let response = handle_initialize(id, params);
                return (Some(serde_json::to_value(response).unwrap()), None);
            }
        }
        Some("initialized") => {
            // This is a notification, no response needed
        }
        Some(method) if method == Completion::METHOD => {
            if let Some(id) = parsed.id {
                let params: CompletionParams = match message
                    .get("params")
                    .and_then(|p| serde_json::from_value(p.clone()).ok())
                {
                    Some(p) => p,
                    None => return (None, None),
                };
                let response = handle_completion(id, params, documents);
                return (Some(serde_json::to_value(response).unwrap()), None);
            }
        }
        Some(method) if method == GotoDefinition::METHOD => {
            if let Some(id) = parsed.id {
                let params: GotoDefinitionParams = match message
                    .get("params")
                    .and_then(|p| serde_json::from_value(p.clone()).ok())
                {
                    Some(p) => p,
                    None => return (None, None),
                };
                let response = handle_definition(id, params, documents);
                return (Some(serde_json::to_value(response).unwrap()), None);
            }
        }
        Some("textDocument/didOpen") => {
            let params = message.get("params").unwrap_or(&serde_json::Value::Null);
            if let Some((uri, diagnostics)) = handle_did_open_for_test(params, documents) {
                let notification = JsonRpcNotification::new(
                    "textDocument/publishDiagnostics",
                    PublishDiagnosticsParams {
                        uri,
                        diagnostics,
                        version: None,
                    },
                );
                return (None, Some(serde_json::to_value(notification).unwrap()));
            }
        }
        Some("textDocument/didChange") => {
            let params = message.get("params").unwrap_or(&serde_json::Value::Null);
            if let Some((uri, diagnostics)) = handle_did_change_for_test(params, documents) {
                let notification = JsonRpcNotification::new(
                    "textDocument/publishDiagnostics",
                    PublishDiagnosticsParams {
                        uri,
                        diagnostics,
                        version: None,
                    },
                );
                return (None, Some(serde_json::to_value(notification).unwrap()));
            }
        }
        Some(method) if method == Shutdown::METHOD => {
            if let Some(id) = parsed.id {
                let response = handle_shutdown(id);
                return (Some(serde_json::to_value(response).unwrap()), None);
            }
        }
        _ => {}
    }

    (None, None)
}

/// Handle textDocument/didOpen for testing (returns diagnostics instead of writing).
fn handle_did_open_for_test(
    params: &serde_json::Value,
    documents: &mut DocumentStore,
) -> Option<(Uri, Vec<Diagnostic>)> {
    let uri = params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|u| u.as_str())?;

    let text = params
        .get("textDocument")
        .and_then(|doc| doc.get("text"))
        .and_then(|t| t.as_str())?;

    let url = Url::parse(uri).ok()?;
    let path = url.to_file_path().ok()?;

    documents.insert(path.clone(), text.to_owned());

    let diagnostics = get_diagnostics(text, &path);
    Some((url.as_str().parse().unwrap(), diagnostics))
}

/// Handle textDocument/didChange for testing (returns diagnostics instead of writing).
fn handle_did_change_for_test(
    params: &serde_json::Value,
    documents: &mut DocumentStore,
) -> Option<(Uri, Vec<Diagnostic>)> {
    let uri = params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|u| u.as_str())?;

    let text = params
        .get("contentChanges")
        .and_then(|changes| changes.as_array())
        .and_then(|arr| arr.first())
        .and_then(|change| change.get("text"))
        .and_then(|t| t.as_str())?;

    let url = Url::parse(uri).ok()?;
    let path = url.to_file_path().ok()?;

    documents.insert(path.clone(), text.to_owned());

    let diagnostics = get_diagnostics(text, &path);
    Some((url.as_str().parse().unwrap(), diagnostics))
}

/// Run LSP test by processing JSONL input.
///
/// Each non-comment, non-empty line is treated as a JSON-RPC message.
/// Lines starting with `//` are ignored.
pub(crate) fn run_lsp_test(src: &str) {
    let mut documents: DocumentStore = FxHashMap::default();

    let json_lines = src
        .lines()
        .filter(|line| !line.starts_with("//") && !line.is_empty());

    for line in json_lines {
        let message: serde_json::Value = match serde_json::from_str(line) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("Error parsing JSON: {e}");
                continue;
            }
        };

        let (response, notification) = process_message(message, &mut documents);

        // Print notification first (e.g., diagnostics from didOpen)
        if let Some(notif) = notification {
            println!("{}", serde_json::to_string(&notif).unwrap());
        }

        // Print response
        if let Some(resp) = response {
            println!("{}", serde_json::to_string(&resp).unwrap());
        }
    }
}

/// Run the LSP server.
pub(crate) fn run_lsp() {
    let mut should_exit = false;
    let mut documents: DocumentStore = FxHashMap::default();

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
            Some(method) if method == Initialize::METHOD => {
                if let Some(id) = parsed.id {
                    let params: InitializeParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing initialize params");
                            continue;
                        }
                    };
                    let response = handle_initialize(id, params);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("initialized") => {
                // This is a notification, no response needed
            }
            Some(method) if method == Completion::METHOD => {
                if let Some(id) = parsed.id {
                    let params: CompletionParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing completion params");
                            continue;
                        }
                    };
                    let response = handle_completion(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some(method) if method == GotoDefinition::METHOD => {
                if let Some(id) = parsed.id {
                    let params: GotoDefinitionParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing definition params");
                            continue;
                        }
                    };
                    let response = handle_definition(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/didOpen") => {
                let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                if let Err(e) = handle_did_open(params, &mut documents) {
                    eprintln!("Error handling didOpen: {e}");
                }
            }
            Some("textDocument/didChange") => {
                let params = message.get("params").unwrap_or(&serde_json::Value::Null);
                if let Err(e) = handle_did_change(params, &mut documents) {
                    eprintln!("Error handling didChange: {e}");
                }
            }
            Some(method) if method == Shutdown::METHOD => {
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
