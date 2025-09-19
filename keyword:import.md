# `import` keyword

You can use `import` to load definitions from another file.

```title:"File: greetings.gdn"
public fun say_hello() {
  println("Hello!")
}
```

```title:"Example 1"
import "./greetings.gdn" as g

g::say_hello()
```

You can also load all the definitions into the current file.

```title:"Example 2"
import "./greetings.gdn"

say_hello()
```
