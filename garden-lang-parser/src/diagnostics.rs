#[derive(Debug, Clone)]
pub struct ErrorMessage(pub Vec<String>);

impl ErrorMessage {
    pub fn as_string(&self) -> String {
        let mut s = String::new();
        for message_part in &self.0 {
            s.push_str(message_part);
        }

        s
    }
}
