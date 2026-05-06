# `&&` Operator

`&&` performs logical AND of two `Bool` values. See also `||`.

```
True && True   //-> True
True && False  //-> False
False && True  //-> False
False && False //-> False
```

Note that `&&` does not currently use short-circuiting, so both sides are
evaluated.

```
// Prints "hello"
False && fun() { 
  println("hello")
  False
}()
```
