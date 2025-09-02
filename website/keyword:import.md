# `import` keyword

You can use `import` to load definitions from another file.

```title:"File: foo.gdn"
public fun defined_in_foo() {}
```

```title:"Example 1"
import "./foo.gdn" as f

fun bar() {
  f::defined_in_foo()
}
```

You can also load all the definitions into the current file.

```title:"Example 2"
import "./foo.gdn"

fun bar() {
  defined_in_foo()
}
```
