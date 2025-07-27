use crate::diagnostics::with_syntax_highlighting;

#[derive(Debug, Clone)]
pub(crate) enum MessagePart {
    Text(String),
    Code(String),
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

#[derive(Debug, Clone)]
pub(crate) struct ErrorMessage(pub(crate) Vec<MessagePart>);

impl ErrorMessage {
    pub(crate) fn as_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&format!("`{c}`")),
            }
        }

        s
    }

    pub(crate) fn as_styled_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&with_syntax_highlighting(c, true)),
            }
        }

        s
    }
}
