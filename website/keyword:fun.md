# `fun` keyword

`fun` defines named functions and anonymous functions.

```title:"Named Function"
fun add_one(x: Int): Int {
  x + 1
}

test two_add_one {
  assert(add_one(2) == 3)
}
```

Type annotations are optional. A function without type annotations is
assumed to take any type as an argument, and may return any type.

```garden title:"No Types Declared" nocheck
fun add_one(x) {
  x + 1
}
```

You can also use `fun` to declare anonymous functions, sometimes
called lambdas.

```title:"Anonymous Function"
let f = fun(x: Int): Int { 
  x + 1 
}
assert(f(2) == 3)
```
