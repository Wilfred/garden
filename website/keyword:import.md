# `import` keyword

You can use `import` to load definitions from another file.

```
import "./foo.gdn" as f

fun bar() {
  f::defined_in_foo()
}
```

You can also load all the definitions into the current file.

```
import "./foo.gdn"

fun bar() {
  defined_in_foo()
}
```

