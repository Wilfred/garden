use std::io::IsTerminal as _;

use crate::diagnostics::with_syntax_highlighting;

#[derive(Debug, Clone)]
pub(crate) enum MessagePart {
    Text(String),
    Code(String),
    /// A clickable hyperlink. In terminals that support OSC 8, this
    /// renders as a clickable link.
    Link { url: String, text: String },
}

/// A macro that wraps Text(format!("foo")) to make it easier to write
/// error messages.
#[macro_export]
macro_rules! msgtext {
    ($($arg:tt)*) => {
        $crate::parser::diagnostics::MessagePart::Text(format!($($arg)*))
    };
}

/// A macro that wraps Code(format!("foo")) to make it easier to write
/// error messages.
#[macro_export]
macro_rules! msgcode {
    ($($arg:tt)*) => {
        $crate::parser::diagnostics::MessagePart::Code(format!($($arg)*))
    };
}

/// A macro that creates a clickable hyperlink in error messages.
/// Usage: msglink!("https://example.com", "click here")
#[macro_export]
macro_rules! msglink {
    ($url:expr, $text:expr) => {
        $crate::parser::diagnostics::MessagePart::Link {
            url: $url.to_owned(),
            text: $text.to_owned(),
        }
    };
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorMessage(pub(crate) Vec<MessagePart>);

impl ErrorMessage {
    pub(crate) fn as_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&format!("`{c}`")),
                MessagePart::Link { url, text } => {
                    // In plain text, show "text (url)"
                    s.push_str(&format!("{text} ({url})"));
                }
            }
        }

        s
    }

    pub(crate) fn as_styled_string(&self) -> String {
        let use_osc8 = std::io::stdout().is_terminal();

        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&with_syntax_highlighting(c, true)),
                MessagePart::Link { url, text } => {
                    if use_osc8 {
                        // OSC 8 hyperlink: \x1b]8;;URL\x1b\\TEXT\x1b]8;;\x1b\\
                        s.push_str(&format!("\x1b]8;;{url}\x1b\\{text}\x1b]8;;\x1b\\"));
                    } else {
                        s.push_str(&format!("{text} ({url})"));
                    }
                }
            }
        }

        s
    }
}
