mod eval;
mod parse;
mod prompt;

use std::collections::HashMap;

use crate::{
    eval::{evaluate_stmt, Value},
    parse::{lex, parse_toplevel},
    prompt::prompt_symbol,
};
use owo_colors::OwoColorize;

fn main() {
    println!(
        "{} {}{}",
        "Welcome to the".bold(),
        "garden".bold().green(),
        "!".bold()
    );

    let mut env: HashMap<String, Value> = HashMap::new();
    env.insert("x".into(), Value::Boolean(true));

    loop {
        println!();
        prompt_symbol(0);

        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                input = input.trim().to_string();

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
            Err(error) => println!("error: {error}"),
        }
    }
}
