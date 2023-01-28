mod eval;
mod parse;
mod prompt;

use crate::{
    eval::{evaluate_stmt, Env, Value},
    parse::{lex, parse_toplevel},
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;
use rustyline::Editor;

fn main() {
    println!(
        "{} {}{}",
        "Welcome to the".bold(),
        "garden".bold().green(),
        "!".bold()
    );

    let mut env = Env::default();
    env.set_with_file_scope("x", Value::Boolean(true));

    let mut rl: Editor<()> = Editor::new().unwrap();
    loop {
        println!();

        match rl.readline(&prompt_symbol(0)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());

                let input = input.trim().to_string();

                if let Some(input) = input.strip_prefix(":parse ") {
                    let tokens = match lex(input) {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            println!("{}: {}", "Error".bright_red(), e);
                            continue;
                        }
                    };
                    let mut token_ptr = &tokens[..];
                    println!("{:?}", parse_toplevel(&mut token_ptr));
                    continue;
                }

                let tokens = match lex(&input) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        println!("{}: {}", "Error".bright_red(), e);
                        continue;
                    }
                };
                let mut token_ptr = &tokens[..];

                match parse_toplevel(&mut token_ptr) {
                    Ok(stmt) => match evaluate_stmt(&stmt, &mut env) {
                        Ok(result) => {
                            println!("{}", result)
                        }
                        Err(e) => {
                            println!("{}: {}", "Error".bright_red(), e);
                        }
                    },
                    Err(e) => {
                        println!("Parsing failed: {}", e);
                    }
                }
            }
            Err(_) => break,
        }
    }
}
