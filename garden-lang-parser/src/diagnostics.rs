use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct ErrorMessage(pub String);
