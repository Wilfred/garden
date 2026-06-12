//! LSP (Language Server Protocol) support for Garden.

use gen_lsp_types::json_rpc::Error as JsonRpcError;
use gen_lsp_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionParams, CodeActionProvider,
    CodeActionResponse, CompletionItem, CompletionOptions, CompletionParams, Contents,
    DefinitionParams, DefinitionProvider, Diagnostic, DiagnosticSeverity, DocumentFormattingParams,
    DocumentFormattingProvider, DocumentHighlight, DocumentHighlightParams,
    DocumentHighlightProvider, ErrorCodes, Hover, HoverParams, HoverProvider, InitializeParams,
    InitializeResult, Location, MarkupContent, MarkupKind, Position, PublishDiagnosticsParams,
    Range, RenameParams, RenameProvider, ServerCapabilities, ServerInfo, TextDocumentSync,
    TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::io::{self, BufRead, Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use tracing::error;
use url::Url;

/// Storage for open document contents, keyed by file path.
type DocumentStore = FxHashMap<PathBuf, String>;

use crate::checks::check_toplevel_items_in_env;
use crate::checks::type_checker::check_types;
use crate::completions;
use crate::destructure::destructure;
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
use crate::temp_built_in_files::TempBuiltInFiles;
use crate::wrap_in_dbg::wrap_in_dbg;

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

/// JSON-RPC 2.0 error response structure.
#[derive(Debug, Serialize)]
struct JsonRpcErrorResponse {
    jsonrpc: &'static str,
    id: serde_json::Value,
    error: JsonRpcError,
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

        // Header field names are case-insensitive per the LSP spec, and
        // the amount of whitespace after the colon is not fixed.
        if let Some((name, value)) = header.split_once(':') {
            if name.trim().eq_ignore_ascii_case("Content-Length") {
                content_length = Some(value.trim().parse::<usize>().map_err(|e| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Invalid Content-Length: {e}"),
                    )
                })?);
            }
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
        let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, Rc::clone(&ns));
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
    let (mut raw_diagnostics, _) = load_toplevel_items(&items, &mut env, Rc::clone(&ns));
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
    load_toplevel_items(&items, &mut env, Rc::clone(&ns));

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
    load_toplevel_items(&items, &mut env, Rc::clone(&ns));

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
                    CodeActionKind::RefactorRewrite,
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

/// Build a publishDiagnostics notification for a file.
fn diagnostics_notification(uri: Uri, diagnostics: Vec<Diagnostic>) -> Option<serde_json::Value> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };

    let notification = JsonRpcNotification::new("textDocument/publishDiagnostics", params);

    match serde_json::to_value(&notification) {
        Ok(v) => Some(v),
        Err(e) => {
            error!("Error serializing diagnostics: {e}");
            None
        }
    }
}

/// Handle textDocument/didOpen notification, returning the
/// publishDiagnostics notification to send.
fn handle_did_open(
    params: &serde_json::Value,
    documents: &mut DocumentStore,
) -> Option<serde_json::Value> {
    let text_document = params.get("textDocument")?;
    let uri = text_document.get("uri")?.as_str()?;
    let text = text_document.get("text")?.as_str()?;

    // Convert URI to path
    let url = Url::parse(uri).ok()?;
    let path = url.to_file_path().ok()?;

    // Store the document content
    documents.insert(path.clone(), text.to_owned());

    let diagnostics = get_diagnostics(text, &path);
    diagnostics_notification(url, diagnostics)
}

/// Handle textDocument/didChange notification, returning the
/// publishDiagnostics notification to send.
fn handle_did_change(
    params: &serde_json::Value,
    documents: &mut DocumentStore,
) -> Option<serde_json::Value> {
    let text_document = params.get("textDocument")?;
    let uri = text_document.get("uri")?.as_str()?;

    // Get the new text from contentChanges (full document sync)
    let text = params
        .get("contentChanges")?
        .as_array()?
        .first()?
        .get("text")?
        .as_str()?;

    // Convert URI to path
    let url = Url::parse(uri).ok()?;
    let path = url.to_file_path().ok()?;

    // Update the stored document content
    documents.insert(path.clone(), text.to_owned());

    let diagnostics = get_diagnostics(text, &path);
    diagnostics_notification(url, diagnostics)
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
    temp_built_in_files: Option<&TempBuiltInFiles>,
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

    // Definitions in built-in files have relative paths like
    // `__prelude.gdn`. Redirect those to the temporary on-disk copy so
    // the editor can open them.
    let def_path = temp_built_in_files
        .and_then(|b| b.resolve(&def_pos.path))
        .unwrap_or_else(|| (*def_pos.path).clone());

    // Convert to LSP Location format
    let location = Location {
        uri: path_to_uri(&def_path),
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

    if let Some(action) = build_destructure_action(&src, &path, uri, &params.range) {
        actions.push(CodeActionResponse::CodeAction(action));
    }

    if let Some(action) = build_wrap_in_dbg_action(&src, &path, uri, &params.range) {
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

/// Build a "Destructure enum" code action for the selected range, if the
/// selection covers an expression with an enum type.
fn build_destructure_action(
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

    let new_src = destructure(src, path, start_offset, end_offset).ok()?;

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
        title: "Destructure enum".to_owned(),
        kind: Some(CodeActionKind::RefactorRewrite),
        edit: Some(workspace_edit),
        ..Default::default()
    })
}

/// Build a "Wrap in dbg()" code action for the selected range, if the
/// selection covers a Garden expression.
fn build_wrap_in_dbg_action(
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

    let new_src = wrap_in_dbg(src, path, start_offset, end_offset).ok()?;

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
        title: "Wrap in dbg()".to_owned(),
        kind: Some(CodeActionKind::RefactorRewrite),
        edit: Some(workspace_edit),
        ..Default::default()
    })
}

/// A range that covers the entire source document.
fn whole_document_range(src: &str) -> Range {
    let (end_line, end_character) = if src.is_empty() {
        (0, 0)
    } else if src.ends_with('\n') {
        (src.lines().count(), 0)
    } else {
        let line_count = src.lines().count();
        let last_line_length = src.lines().last().map_or(0, |l| l.len());
        (line_count.saturating_sub(1), last_line_length)
    };

    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: end_line as u32,
            character: end_character as u32,
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
///
/// A character position past the end of a line is clamped to the end
/// of that line, and a line past the end of the source is clamped to
/// the end of the source.
fn line_char_to_offset(src: &str, line: usize, character: usize) -> usize {
    // Find the byte offset of the start of the requested line.
    let mut line_start = 0;
    for _ in 0..line {
        match src[line_start..].find('\n') {
            Some(i) => line_start += i + 1,
            None => return src.len(),
        }
    }

    // Walk forward `character` characters, stopping at the end of the
    // line or the end of the source.
    let mut offset = line_start;
    for (char_count, (char_idx, ch)) in src[line_start..].char_indices().enumerate() {
        if char_count == character || ch == '\n' {
            return line_start + char_idx;
        }
        offset = line_start + char_idx + ch.len_utf8();
    }

    offset
}

/// Serialize a response and append it to the outgoing messages.
fn push_response<T: Serialize>(
    outgoing: &mut Vec<serde_json::Value>,
    response: JsonRpcResponse<T>,
) {
    match serde_json::to_value(&response) {
        Ok(v) => outgoing.push(v),
        Err(e) => error!("Error serializing response: {e}"),
    }
}

/// Serialize an error response and append it to the outgoing
/// messages.
///
/// Every request needs a response, even a malformed or unsupported
/// one: a client that receives nothing will wait indefinitely.
fn push_error(
    outgoing: &mut Vec<serde_json::Value>,
    id: serde_json::Value,
    code: ErrorCodes,
    message: String,
) {
    let response = JsonRpcErrorResponse {
        jsonrpc: "2.0",
        id,
        error: JsonRpcError {
            code,
            message,
            data: None,
        },
    };
    match serde_json::to_value(&response) {
        Ok(v) => outgoing.push(v),
        Err(e) => error!("Error serializing error response: {e}"),
    }
}

/// Parse the params of a request and call `handler`, appending the
/// response to the outgoing messages. If the params don't parse,
/// respond with an invalid params error.
fn push_request_response<P: serde::de::DeserializeOwned, T: Serialize>(
    outgoing: &mut Vec<serde_json::Value>,
    message: &serde_json::Value,
    id: serde_json::Value,
    method: &str,
    handler: impl FnOnce(serde_json::Value, P) -> JsonRpcResponse<T>,
) {
    match message
        .get("params")
        .and_then(|p| serde_json::from_value(p.clone()).ok())
    {
        Some(params) => push_response(outgoing, handler(id, params)),
        None => {
            error!("Error parsing {method} params");
            push_error(
                outgoing,
                id,
                ErrorCodes::InvalidParams,
                format!("Could not parse parameters for {method}."),
            );
        }
    }
}

/// What the server should do after handling a message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    /// Continue processing messages.
    Continue,
    /// A shutdown request was received.
    Shutdown,
    /// An exit notification was received; stop the loop.
    Exit,
}

/// Handle a single message from the client. Returns the messages to
/// send back, and the action the server should take next.
fn handle_message(
    message: &serde_json::Value,
    documents: &mut DocumentStore,
    temp_built_in_files: Option<&TempBuiltInFiles>,
) -> (Vec<serde_json::Value>, Action) {
    let mut outgoing: Vec<serde_json::Value> = vec![];
    let mut action = Action::Continue;

    // Parse the message
    let parsed: Message = match serde_json::from_value(message.clone()) {
        Ok(m) => m,
        Err(e) => {
            error!("Error parsing message: {e}");
            // If the malformed message was a request, the client is
            // waiting on a response.
            if let Some(id) = message.get("id") {
                push_error(
                    &mut outgoing,
                    id.clone(),
                    ErrorCodes::InvalidRequest,
                    format!("Could not parse message: {e}"),
                );
            }
            return (outgoing, action);
        }
    };

    match parsed.method.as_deref() {
        Some("initialize") => {
            if let Some(id) = parsed.id {
                push_request_response(&mut outgoing, message, id, "initialize", handle_initialize);
            }
        }
        Some("initialized") => {
            // This is a notification, no response needed
        }
        Some("textDocument/completion") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/completion",
                    |id, params| handle_completion(id, params, documents),
                );
            }
        }
        Some("textDocument/definition") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/definition",
                    |id, params| handle_definition(id, params, documents, temp_built_in_files),
                );
            }
        }
        Some("textDocument/hover") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/hover",
                    |id, params| handle_hover(id, params, documents),
                );
            }
        }
        Some("textDocument/documentHighlight") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/documentHighlight",
                    |id, params| handle_document_highlight(id, params, documents),
                );
            }
        }
        Some("textDocument/formatting") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/formatting",
                    |id, params| handle_formatting(id, params, documents),
                );
            }
        }
        Some("textDocument/codeAction") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/codeAction",
                    |id, params| handle_code_action(id, params, documents),
                );
            }
        }
        Some("textDocument/rename") => {
            if let Some(id) = parsed.id {
                push_request_response(
                    &mut outgoing,
                    message,
                    id,
                    "textDocument/rename",
                    |id, params| handle_rename(id, params, documents),
                );
            }
        }
        Some("textDocument/didOpen") => {
            let params = message.get("params").unwrap_or(&serde_json::Value::Null);
            outgoing.extend(handle_did_open(params, documents));
        }
        Some("textDocument/didChange") => {
            let params = message.get("params").unwrap_or(&serde_json::Value::Null);
            outgoing.extend(handle_did_change(params, documents));
        }
        Some("shutdown") => {
            if let Some(id) = parsed.id {
                push_response(&mut outgoing, handle_shutdown(id));
            }
            action = Action::Shutdown;
        }
        Some("exit") => {
            // Exit notification
            action = Action::Exit;
        }
        Some(method) => {
            // Unknown notifications can be safely ignored, but
            // requests can't: the client is waiting on a response.
            if let Some(id) = parsed.id {
                push_error(
                    &mut outgoing,
                    id,
                    ErrorCodes::MethodNotFound,
                    format!("Unsupported method {method}."),
                );
            }
        }
        None => {
            // A message without a method is a response to a
            // server-initiated request. We never send those, so
            // there's nothing to do.
        }
    }

    (outgoing, action)
}

/// Run the LSP server.
pub(crate) fn run_lsp() {
    let mut documents: DocumentStore = FxHashMap::default();

    let temp_built_in_files = match TempBuiltInFiles::new("garden-lsp") {
        Ok(b) => Some(b),
        Err(e) => {
            error!("Could not write built-in files to a temporary directory: {e}");
            None
        }
    };

    let mut shutdown_received = false;

    loop {
        let message = match read_message() {
            Ok(Some(msg)) => msg,
            Ok(None) => {
                // EOF reached
                break;
            }
            Err(e) => {
                error!("Error reading message: {e}");
                continue;
            }
        };

        let (outgoing, action) =
            handle_message(&message, &mut documents, temp_built_in_files.as_ref());

        for msg in &outgoing {
            if let Err(e) = write_message(msg) {
                error!("Error writing message: {e}");
            }
        }

        match action {
            Action::Continue => {}
            Action::Shutdown => shutdown_received = true,
            Action::Exit => {
                // Per the LSP spec, the server exits with code 0 if an
                // `exit` notification follows a `shutdown` request, and
                // with code 1 otherwise.
                std::process::exit(if shutdown_received { 0 } else { 1 });
            }
        }
    }
}

/// Replay LSP messages from a .jsonl file against the LSP message
/// handler and print the messages the server sends back.
///
/// Each non-blank, non-comment line is a JSON object representing a
/// single LSP message from the client, without the `Content-Length`
/// framing used on stdin. Lines starting `//` are ignored.
///
/// Responses and server notifications (e.g. publishDiagnostics) are
/// printed as pretty JSON in the order they would be sent.
pub(crate) fn reftest_lsp(src: &str) {
    let mut documents: DocumentStore = FxHashMap::default();

    for line in src.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }

        let message: serde_json::Value = match serde_json::from_str(trimmed) {
            Ok(v) => v,
            Err(e) => {
                let err_obj = serde_json::json!({
                    "error": format!("invalid JSON message: {e}"),
                    "line": trimmed,
                });
                println!("{}", serde_json::to_string_pretty(&err_obj).unwrap());
                continue;
            }
        };

        // Built-in files are not written to disk in reftests, so
        // definition positions in built-ins keep their relative paths.
        let (outgoing, _action) = handle_message(&message, &mut documents, None);
        for msg in outgoing {
            println!("{}", serde_json::to_string_pretty(&msg).unwrap());
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

    #[test]
    fn test_line_char_to_offset_end_of_file() {
        // A position at the end of the last line, when the file has
        // no trailing newline, is the end of the source.
        assert_eq!(line_char_to_offset("abc", 0, 3), 3);
        let src = "fun foo(p: Path) {\n  p.\n}";
        assert_eq!(line_char_to_offset(src, 2, 1), 25);
    }

    #[test]
    fn test_line_char_to_offset_after_trailing_newline() {
        // The empty line after a trailing newline starts at the end
        // of the source.
        assert_eq!(line_char_to_offset("abc\n", 1, 0), 4);
    }

    #[test]
    fn test_line_char_to_offset_clamps_to_line_end() {
        // A character position past the end of a line is clamped to
        // the end of that line, not the start.
        assert_eq!(line_char_to_offset("ab\ncd\n", 0, 10), 2);
    }

    #[test]
    fn test_line_char_to_offset_clamps_to_source_end() {
        // A line past the end of the source is clamped to the end of
        // the source.
        assert_eq!(line_char_to_offset("ab\ncd", 7, 0), 5);
    }

    #[test]
    fn test_whole_document_range_trailing_newline() {
        // A file ending in `\n` must extend the range past the last content
        // line, otherwise replacing it leaves the original `\n` in the
        // document and the replacement (which also ends in `\n`) introduces
        // a spurious blank line.
        let range = whole_document_range("abc\ndef\n");
        assert_eq!(range.end.line, 2);
        assert_eq!(range.end.character, 0);
    }

    #[test]
    fn test_whole_document_range_no_trailing_newline() {
        let range = whole_document_range("abc\ndef");
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 3);
    }

    #[test]
    fn test_whole_document_range_empty() {
        let range = whole_document_range("");
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 0);
    }
}
