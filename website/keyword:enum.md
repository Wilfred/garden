# `enum` keyword

`enum` defines a type where values are one of a possible set of
variants.

```title:example
enum Flavor {
  Chocolate,
  Strawberry,
  Custom(String),
}

let f = Custom("Fudge")
```

