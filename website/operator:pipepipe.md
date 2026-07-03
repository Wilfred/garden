# `||` Operator

`||` performs logical OR of two `Bool` values.  See also `&&`.

```
True || True   //-> True
True || False  //-> True
False || True  //-> True
False || False //-> False
```

`||` uses short-circuiting: if the left-hand side is `True`, the
right-hand side is not evaluated.

```
// Does not print "hello"
True || fun() {
  println("hello")
  False
}()
```
