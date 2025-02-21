use owo_colors::OwoColorize as _;

#[derive(Debug, Clone)]
pub enum MessagePart {
    Text(String),
    Code(String),
}

#[derive(Debug, Clone)]
pub struct ErrorMessage(pub Vec<MessagePart>);

impl ErrorMessage {
    pub fn as_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&format!("`{}`", c)),
            }
        }

        s
    }

    pub fn as_styled_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            match message_part {
                MessagePart::Text(t) => s.push_str(t),
                MessagePart::Code(c) => s.push_str(&c.bold().to_string()),
            }
        }

        s
    }
}
