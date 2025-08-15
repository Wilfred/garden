# `if` keyword

`if` evaluates a conditional.

```title:example
let b = True

if b {
  print("b was true!")
} else {
  print("b was false!")
}
```

`if` is an expression in Garden, so it can be used like a ternary.

```title:example
let food = "chocolate"

let description = if food == "chocolate" {
  "tasty"
} else {
  "hmm"
}
```

`else` is optional.

```title:example
let b = True

if b {
  print("b was true!")
}
```

