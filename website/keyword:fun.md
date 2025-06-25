# `fun` keyword

`fun` defines named functions and anonymous functions.

```
fun add_one(x: Int): Int {
  x + 1
}

test two_add_one {
  assert(add_one(2) == 3)
}
```

```
let f = fun(x: Int): Int { x + 1 }
assert(f(2) == 3)
```
