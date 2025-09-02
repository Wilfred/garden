# `if` keyword

`if` evaluates a conditional.

```title:"Example 1"
let b = True

if b {
  print("b was true!")
} else {
  print("b was false!")
}
```

`if` is an expression in Garden, so it can be used like a ternary.

```title:"Example 2"
let food = "chocolate"

let description = if food == "chocolate" {
  "tasty"
} else {
  "hmm"
}
```

`else` is optional.

```title:"Example 3"
let b = True

if b {
  print("b was true!")
}
```

