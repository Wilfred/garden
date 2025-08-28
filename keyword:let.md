# `let` keyword

`let` introduces a new variable.

```title:example
let x = 0
// Variables can be reassigned.
x = 1
```

You can add type annotations to `let`.

```title:example
let x: List<Int> = []
```

If you have a tuple, you can use `let` to destructure the tuple
elements.

```title:example
let position = (1, 2)
let (x, y) = position

dbg(x) // 1
dbg(y) // 2
```

You can also use `let` to shadow variables, where you introduce a new
variable that hides the old value.

```title:example
let i = 0

if True {
  let i = 1
  dbg(i) // 1
  Unit
}

dbg(i) // 0
```
