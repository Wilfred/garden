# `&&` Operator

`&&` performs logical AND of two `Bool` values. See also `||`.

```
True && True   //-> True
True && False  //-> False
False && True  //-> False
False && False //-> False
```

`&&` uses short-circuiting: if the left-hand side is `False`, the
right-hand side is not evaluated.

```
// Does not print "hello"
False && fun() {
  println("hello")
  False
}()
```

This means the left-hand side can guard the right-hand side.

```
let l: List<Int> = []

// Safe: `l.first().or_throw()` is only evaluated when `l` is nonempty.
if (l.len() > 0) && (l.first().or_throw() == 1) {
  println("first item is 1")
}
```
