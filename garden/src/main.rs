fn main() {
    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(n) => {
                println!("{n} bytes read");
                println!("{input}");
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
