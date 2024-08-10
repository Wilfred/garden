use std::path::PathBuf;

use garden_lang_parser::{ast::SyntaxIdGenerator, parse_toplevel_items};
use wasm_bindgen::prelude::*;

// https://rustwasm.github.io/docs/book/game-of-life/hello-world.html#build-the-project

#[wasm_bindgen]
pub fn check_parse(src: &str) -> Option<String> {
    let mut id_gen = SyntaxIdGenerator::default();
    let (_, errors) = parse_toplevel_items(&PathBuf::new(), src, &mut id_gen);
    // TODO: report all errors.
    match errors.first() {
        Some(e) => match e {
            garden_lang_parser::ParseError::Invalid { message, .. } => {
                Some(format!("Invalid: {}", message.0))
            }
            garden_lang_parser::ParseError::Incomplete { message, .. } => {
                Some(format!("Incomplete: {}", message.0))
            }
        },
        None => None,
    }
}
