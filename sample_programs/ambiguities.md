## return

Garden now requires return to always have a value to avoid ambiguity
with expressions immediately after.

```
return 1

return 
1
```

## struct literals

There is still an ambiguity with struct literals and blocks, however.

```
// struct literal
b {}

// block
{}

// ambiguous
let a = b
{}
```

Proposal: require curly paren immediately after struct name, following
Go, e.g.

```
Foo{}

Foo{
  name: "wilfred",
}
```

Rust solves this by defining an expression pattern which is
'everything except struct literals':
https://rust-lang.github.io/rfcs/0092-struct-grammar.html

## tuples

There is also an ambiguity with tuples/parentheses and function calls.

```
foo()

(1, 2)


let x = foo
(1, 2)
```

Proposal: consider EOL, EOF or } to be an expression terminator.
Or require types to begin with a capital letter?
