mod eval;
mod parse;
mod prompt;

use crate::{
    eval::{eval_iter, Env},
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
    println!("Type {} if you're new here.", ":help".bold().green(),);

    let mut env = Env::default();

    let mut rl: Editor<()> = Editor::new().unwrap();
    // TODO: put this in the home directory rather than the current directory.
    let _ = rl.load_history(".history");

    loop {
        println!();

        match rl.readline(&prompt_symbol(0)) {
            Ok(input) => {
                rl.add_history_entry(input.as_str());

                let input = input.trim().to_string();

                if input == ":globals" {
                    for (var_name, value) in &env.file_scope {
                        println!("{}\t{}", var_name.bright_green(), value);
                    }

                    continue;
                }

                if input == ":locals" {
                    if let Some((_, fun_scope)) = env.fun_scopes.last() {
                        for (var_name, value) in fun_scope {
                            println!("{}\t{}", var_name.bright_green(), value);
                        }
                    }

                    continue;
                }

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
                if input.trim() == ":help" {
                    println!(
                        "Available commands are {}, {}, {} and {}.",
                        ":parse".green(),
                        ":locals".green(),
                        ":globals".green(),
                        ":help".green(),
                    );
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
                    Ok(stmts) => match eval_iter(&stmts, &mut env) {
                        Ok(result) => match result {
                            eval::Value::Void => {}
                            v => {
                                println!("{}", v)
                            }
                        },
                        Err(e) => {
                            println!("{}: {}", "Error".bright_red(), e);
                            print_stack(&env);
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

    let _ = rl.save_history(".history");
}

fn print_stack(env: &Env) {
    for (description, _) in env.fun_scopes.iter().rev() {
        println!("In {}", description);
    }
}
