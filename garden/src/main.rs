fn lex(s: &str) -> Vec<&str> {
    s.split(' ').collect()
}

fn main() {
    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                dbg!(lex(&input));
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
