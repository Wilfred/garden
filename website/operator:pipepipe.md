# `||` Operator

`||` performs logical OR of two booleans.

```
True || True   //-> True
True || False  //-> True
False || True  //-> True
False || False //-> False
```

Note that `||` does not currently use short-circuiting, so both sides are
evaluated.

```
// Prints "hello"
False || fun() { 
  println("hello")
  False
}()
```
