//! LSP (Language Server Protocol) support for Garden.

use serde::Deserialize;
use std::io::{self, BufRead, Read, Write};

#[derive(Debug, Deserialize)]
struct Message {
    #[allow(dead_code)]
    jsonrpc: String,
    #[serde(default)]
    id: Option<serde_json::Value>,
    method: Option<String>,
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

/// Handle an initialize request.
fn handle_initialize(id: serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": {
            "capabilities": {
                // Minimal capabilities - we support nothing
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
