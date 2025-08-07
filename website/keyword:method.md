# `method` keyword

`method` defines methods on types.

```title:example
method increment(this: Int): Int {
  this + 1
}

test two_increment {
  assert(2.increment() == 3)
}
```
