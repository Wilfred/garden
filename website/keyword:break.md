# `break` and `continue` keywords

`break` stops evaluation of a loop.

```
let flavors = ["Chocolate", "Strawberry", "Mint", "Vanilla"]

for flavor in flavors {
  if flavor == "Mint" {
    break
  }
  println(flavor ^ " ice cream is yummy.")
}

// Chocolate ice cream is yummy.
// Strawberry ice cream is yummy.
```


`continue` stops evaluation of the current loop iteration, and
immediately starts the next iteration.

```
let flavors = ["Chocolate", "Strawberry", "Mint", "Vanilla"]

for flavor in flavors {
  if flavor == "Mint" {
    continue // Yuck.
  }
  println(flavor ^ " ice cream is yummy.")
}

// Chocolate ice cream is yummy.
// Strawberry ice cream is yummy.
// Vanilla ice cream is yummy.
```
