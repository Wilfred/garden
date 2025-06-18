# `assert` keyword

You can use `assert` to perform a runtime check that a value is
`True`.

```
assert(1 < 2)
```

If the assertion fails, Garden will raise an error.

`assert` is particularly useful when defining a `test`.

```
fun add_one(i: Int): Int {
  i + 1
}

test add_one_to_five {
  assert(add_one(5) == 6)
}
```
