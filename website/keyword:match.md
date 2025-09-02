# `match` keyword

`match` evaluates code depending on a value of an enum.

```title:"Example 1"
fun say_hello(name: Option<String>) {
  match name {
    Some(n) => println("Hello " ^ n)
    None => println("Hello there!")
  }
}

say_hello(None) // Hello there!
say_hello(Some("Wilfred")) // Hello Wilfred
```

If you want to ignore some cases, use `_`.

```title:"Example 2"
enum Flavor {
  Chocolate,
  Strawberry,
  Vanilla,
}

fun describe_flavor(f: Flavor) {
  match f {
    Chocolate => println("Tasty!")
    _ => {
      println("Some other flavor.")
      println("Could be strawberry or vanilla.")
    }
  }
}
```

`match` also supports destructuring of tuples.

```title:"Example 3"
fun describe_email(address: String) {
  match address.split_once("@") {
    Some((_, domain)) => {
      println("Looks like an email at domain " ^ domain)
    }
    None => {
      println("Doesn't look like an email address.")
    }
  }
}
```
