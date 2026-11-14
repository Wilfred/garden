# `import` keyword

You can use `import` to load definitions from another file.

```title:"File: greetings.gdn"
public fun say_hello() {
  println("Hello!")
}
```

```title:"Example"
import "./greetings.gdn" as g

g::say_hello()
```
