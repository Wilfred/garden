use std::io::Write;

fn lex(s: &str) -> Vec<&str> {
    s.split(' ').collect()
}

fn main() {
    println!("Welcome to the garden!");

    loop {
        print!("> ");
        let mut input = String::new();
        std::io::stdout().flush().unwrap();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                dbg!(lex(&input));
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
