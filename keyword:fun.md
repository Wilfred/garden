# `fun` keyword

`fun` defines functions and methods.

```
fun add_one(x: Int): Int {
  x + 1
}

test two_add_one {
  assert(add_one(2) == 3)
}
```

```
fun (this: Int) increment(): Int {
  this + 1
}

test two_increment {
  assert(2.increment() == 3)
}
```
