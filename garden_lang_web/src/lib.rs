use std::path::PathBuf;

use garden_lang_parser::parse_toplevel_items;
use wasm_bindgen::prelude::*;

// https://rustwasm.github.io/docs/book/game-of-life/hello-world.html#build-the-project

#[wasm_bindgen]
pub fn check_parse(src: &str) -> bool {
    parse_toplevel_items(&PathBuf::new(), src).is_ok()
}
