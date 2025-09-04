# `return` keyword

`return` stops the current function or method and returns the value to
the caller.

```title:Example
fun subtract_one(i: Int): Int {
  if i == 0 {
    return 0
  }
  
  i - 1
}
```
