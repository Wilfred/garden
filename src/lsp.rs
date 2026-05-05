//! LSP (Language Server Protocol) support for Garden.

use gen_lsp_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionParams, CodeActionProvider,
    CodeActionResponse, CompletionItem, CompletionOptions, CompletionParams, Contents,
    DefinitionParams, DefinitionProvider, Diagnostic, DiagnosticSeverity, DocumentFormattingParams,
    DocumentFormattingProvider, DocumentHighlight, DocumentHighlightParams,
    DocumentHighlightProvider, Hover, HoverParams, HoverProvider, InitializeParams,
    InitializeResult, Location, MarkupContent, MarkupKind, Position, PublishDiagnosticsParams,
    Range, RenameParams, RenameProvider, ServerCapabilities, ServerInfo, TextDocumentSync,
    TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::io::{self, BufRead, Read, Write};
use std::path::{Path, PathBuf};
use url::Url;

/// Storage for open document contents, keyed by file path.
type DocumentStore = FxHashMap<PathBuf, String>;

use crate::checks::check_toplevel_items_in_env;
use crate::checks::type_checker::check_types;
use crate::completions;
use crate::diagnostics::{Autofix, Diagnostic as GardenDiagnostic, Severity};
use crate::env::Env;
use crate::eval::load_toplevel_items;
use crate::extract_function::extract_function;
use crate::highlight::highlight_occurrences;
use crate::parser::ast::IdGenerator;
use crate::parser::position::Position as GardenPosition;
use crate::parser::vfs::Vfs;
use crate::parser::{parse_toplevel_items, ParseError};
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
    Url::from_file_path(path).unwrap_or_else(|_| {
        // Fallback to file:// scheme with display
        Url::parse(&format!("file://{}", path.display())).unwrap()
    })
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
            severity: Some(DiagnosticSeverity::Error),
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
                Severity::Error => DiagnosticSeverity::Error,
                Severity::Warning => DiagnosticSeverity::Warning,
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

/// Get autofixes for a file.
fn get_fixes(src: &str, path: &PathBuf) -> Vec<Autofix> {
    let mut fixes = vec![];

    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, ns.clone());
    raw_diagnostics.extend(check_toplevel_items_in_env(&vfs_path, &items, &env, ns));

    for diagnostic in raw_diagnostics {
        fixes.extend(diagnostic.fixes);
    }

    fixes
}

/// Get completion items at the given offset.
fn get_completions(src: &str, path: &Path, offset: usize) -> Vec<CompletionItem> {
    completions::complete(src, path, offset)
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

/// Get hover information for a symbol at the given offset.
fn get_hover(src: &str, path: &PathBuf, offset: usize) -> Option<Hover> {
    let mut id_gen = IdGenerator::default();
    let (vfs, vfs_path) = Vfs::singleton(path.to_owned(), src.to_owned());

    let (items, _errors) = parse_toplevel_items(&vfs_path, src, &mut id_gen);

    let mut env = Env::new(id_gen, vfs);
    let ns = env.get_or_create_namespace(path);
    load_toplevel_items(&items, &mut env, ns.clone());

    let summary = check_types(&vfs_path, &items, &env, ns);

    let hovered_ids = find_item_at(&items, offset, offset);

    let mut ty_str: Option<String> = None;
    for id in hovered_ids.iter().rev() {
        if let Some(ty) = summary.id_to_ty.get(&id.id()) {
            if !ty.is_error() {
                ty_str = Some(ty.to_string());
            }
            break;
        }
    }

    let mut doc_comment: Option<String> = None;
    for id in hovered_ids.iter().rev() {
        if let Some(doc) = summary.id_to_doc_comment.get(&id.id()) {
            doc_comment = Some(doc.clone());
            break;
        }
    }

    if ty_str.is_none() && doc_comment.is_none() {
        return None;
    }

    let mut value = String::new();
    if let Some(ty) = ty_str {
        value.push_str("```garden\n");
        value.push_str(&ty);
        value.push_str("\n```");
    }
    if let Some(doc) = doc_comment {
        if !value.is_empty() {
            value.push_str("\n\n");
        }
        value.push_str(&doc);
    }

    Some(Hover {
        contents: Contents::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    })
}

/// Handle an initialize request.
fn handle_initialize(
    id: serde_json::Value,
    _params: InitializeParams,
) -> JsonRpcResponse<InitializeResult> {
    let result = InitializeResult {
        capabilities: ServerCapabilities {
            definition_provider: Some(DefinitionProvider::Bool(true)),
            hover_provider: Some(HoverProvider::Bool(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".to_owned(), "::".to_owned()]),
                ..Default::default()
            }),
            text_document_sync: Some(TextDocumentSync::Kind(TextDocumentSyncKind::Full)),
            document_formatting_provider: Some(DocumentFormattingProvider::Bool(true)),
            document_highlight_provider: Some(DocumentHighlightProvider::Bool(true)),
            code_action_provider: Some(CodeActionProvider::CodeActionOptions(CodeActionOptions {
                code_action_kinds: Some(vec![
                    CodeActionKind::QuickFix,
                    CodeActionKind::RefactorExtract,
                ]),
                ..Default::default()
            })),
            rename_provider: Some(RenameProvider::Bool(true)),
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
    send_diagnostics(url, diagnostics)
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
    send_diagnostics(url, diagnostics)
}

/// Handle a textDocument/completion request.
fn handle_completion(
    id: serde_json::Value,
    params: CompletionParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Vec<CompletionItem>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Convert file:// URI to path
    let path = match uri.to_file_path() {
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
    params: DefinitionParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Option<Location>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Convert file:// URI to path
    let path = match uri.to_file_path() {
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

/// Handle a textDocument/hover request.
fn handle_hover(
    id: serde_json::Value,
    params: HoverParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let path = match uri.to_file_path() {
        Ok(p) => p,
        Err(_) => return JsonRpcResponse::new(id, None),
    };

    let src = match documents.get(&path) {
        Some(content) => content.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => return JsonRpcResponse::new(id, None),
        },
    };

    let offset = line_char_to_offset(&src, position.line as usize, position.character as usize);

    let hover = get_hover(&src, &path, offset);

    JsonRpcResponse::new(id, hover)
}

/// Handle a textDocument/documentHighlight request.
fn handle_document_highlight(
    id: serde_json::Value,
    params: DocumentHighlightParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Vec<DocumentHighlight>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let path = match uri.to_file_path() {
        Ok(p) => p,
        Err(_) => return JsonRpcResponse::new(id, vec![]),
    };

    let src = match documents.get(&path) {
        Some(content) => content.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => return JsonRpcResponse::new(id, vec![]),
        },
    };

    let offset = line_char_to_offset(&src, position.line as usize, position.character as usize);

    let highlights = highlight_occurrences(&src, &path, offset)
        .into_iter()
        .map(|pos| DocumentHighlight {
            range: garden_pos_to_lsp_range(&pos),
            kind: None,
        })
        .collect();

    JsonRpcResponse::new(id, highlights)
}

/// Handle a textDocument/formatting request.
fn handle_formatting(
    id: serde_json::Value,
    params: DocumentFormattingParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Vec<TextEdit>> {
    let uri = &params.text_document.uri;

    // Convert file:// URI to path
    let path = match uri.to_file_path() {
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

    // Format the document
    let formatted = crate::format::format(&src, &path);

    // Create a TextEdit that replaces the entire document
    let edit = TextEdit {
        range: whole_document_range(&src),
        new_text: formatted,
    };

    JsonRpcResponse::new(id, vec![edit])
}

/// Handle a textDocument/codeAction request.
fn handle_code_action(
    id: serde_json::Value,
    params: CodeActionParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Vec<CodeActionResponse>> {
    let uri = &params.text_document.uri;

    // Convert file:// URI to path
    let path = match uri.to_file_path() {
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

    let fixes = get_fixes(&src, &path);

    // Convert fixes to code actions
    let mut actions: Vec<CodeActionResponse> = vec![];
    for fix in fixes {
        let range = garden_pos_to_lsp_range(&fix.position);

        // Only include fixes that overlap with the requested range
        if !ranges_overlap(&range, &params.range) {
            continue;
        }

        let text_edit = TextEdit {
            range,
            new_text: fix.new_text,
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert(uri.clone(), vec![text_edit]);

        let workspace_edit = WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        };

        let action = CodeAction {
            title: fix.description,
            kind: Some(CodeActionKind::QuickFix),
            edit: Some(workspace_edit),
            ..Default::default()
        };

        actions.push(CodeActionResponse::CodeAction(action));
    }

    if let Some(action) = build_extract_function_action(&src, &path, uri, &params.range) {
        actions.push(CodeActionResponse::CodeAction(action));
    }

    JsonRpcResponse::new(id, actions)
}

/// Handle a textDocument/rename request.
fn handle_rename(
    id: serde_json::Value,
    params: RenameParams,
    documents: &DocumentStore,
) -> JsonRpcResponse<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let new_name = params.new_name;

    let path = match uri.to_file_path() {
        Ok(p) => p,
        Err(_) => return JsonRpcResponse::new(id, None),
    };

    let src = match documents.get(&path) {
        Some(content) => content.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => return JsonRpcResponse::new(id, None),
        },
    };

    let offset = line_char_to_offset(&src, position.line as usize, position.character as usize);

    let positions = match crate::rename::rename_positions(&src, &path, offset) {
        Ok(positions) => positions,
        Err(_) => return JsonRpcResponse::new(id, None),
    };

    let edits: Vec<TextEdit> = positions
        .into_iter()
        .map(|pos| TextEdit {
            range: garden_pos_to_lsp_range(&pos),
            new_text: new_name.clone(),
        })
        .collect();

    if edits.is_empty() {
        return JsonRpcResponse::new(id, None);
    }

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), edits);

    let workspace_edit = WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    };

    JsonRpcResponse::new(id, Some(workspace_edit))
}

/// Build an "Extract function" code action for the selected range, if the
/// selection covers a Garden expression.
fn build_extract_function_action(
    src: &str,
    path: &Path,
    uri: &Uri,
    range: &Range,
) -> Option<CodeAction> {
    let start_offset = line_char_to_offset(
        src,
        range.start.line as usize,
        range.start.character as usize,
    );
    let end_offset =
        line_char_to_offset(src, range.end.line as usize, range.end.character as usize);

    // Only offer extract function for non-empty selections.
    if start_offset >= end_offset {
        return None;
    }

    let new_src = extract_function(src, path, start_offset, end_offset, "extracted").ok()?;

    let full_range = whole_document_range(src);
    let text_edit = TextEdit {
        range: full_range,
        new_text: new_src,
    };

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), vec![text_edit]);

    let workspace_edit = WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    };

    Some(CodeAction {
        title: "Extract function".to_owned(),
        kind: Some(CodeActionKind::RefactorExtract),
        edit: Some(workspace_edit),
        ..Default::default()
    })
}

/// A range that covers the entire source document.
fn whole_document_range(src: &str) -> Range {
    let line_count = src.lines().count();
    let last_line_length = src.lines().last().map_or(0, |l| l.len());

    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: line_count.saturating_sub(1) as u32,
            character: last_line_length as u32,
        },
    }
}

/// Check if two ranges overlap.
fn ranges_overlap(a: &Range, b: &Range) -> bool {
    // a starts after b ends
    if a.start.line > b.end.line
        || (a.start.line == b.end.line && a.start.character > b.end.character)
    {
        return false;
    }
    // a ends before b starts
    if a.end.line < b.start.line
        || (a.end.line == b.start.line && a.end.character < b.start.character)
    {
        return false;
    }
    true
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
                if char_count == character {
                    return line_start + char_idx;
                }
                if ch == '\n' {
                    break;
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
            Some("initialize") => {
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
            Some("textDocument/completion") => {
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
            Some("textDocument/definition") => {
                if let Some(id) = parsed.id {
                    let params: DefinitionParams = match message
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
            Some("textDocument/hover") => {
                if let Some(id) = parsed.id {
                    let params: HoverParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing hover params");
                            continue;
                        }
                    };
                    let response = handle_hover(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/documentHighlight") => {
                if let Some(id) = parsed.id {
                    let params: DocumentHighlightParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing documentHighlight params");
                            continue;
                        }
                    };
                    let response = handle_document_highlight(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/formatting") => {
                if let Some(id) = parsed.id {
                    let params: DocumentFormattingParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing formatting params");
                            continue;
                        }
                    };
                    let response = handle_formatting(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/codeAction") => {
                if let Some(id) = parsed.id {
                    let params: CodeActionParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing code action params");
                            continue;
                        }
                    };
                    let response = handle_code_action(id, params, &documents);
                    if let Err(e) = write_message(&response) {
                        eprintln!("Error writing response: {e}");
                    }
                }
            }
            Some("textDocument/rename") => {
                if let Some(id) = parsed.id {
                    let params: RenameParams = match message
                        .get("params")
                        .and_then(|p| serde_json::from_value(p.clone()).ok())
                    {
                        Some(p) => p,
                        None => {
                            eprintln!("Error parsing rename params");
                            continue;
                        }
                    };
                    let response = handle_rename(id, params, &documents);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_char_to_offset_at_newline() {
        let src = "fun foo(p: Path) {\n  p.\n}";
        // Line 1, character 4 is the newline after "p."
        // This is the case that was broken before the fix
        assert_eq!(line_char_to_offset(src, 1, 4), 23);
    }

    #[test]
    fn test_line_char_to_offset() {
        let src = "fun foo(p: Path) {\n  p.\n}";
        // Line 1, character 2 is 'p'
        assert_eq!(line_char_to_offset(src, 1, 2), 21);
        // Line 1, character 3 is '.'
        assert_eq!(line_char_to_offset(src, 1, 3), 22);
    }
}
