use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct ErrorMessage(pub String);
