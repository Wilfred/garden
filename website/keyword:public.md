# `public` keyword

You can use `public` to declare the visibility of your functions and
methods.

If a function or method does not have `public`, it can only be used in
the current file where it is defined.

```title:example
fun visible_in_the_current_file() {}
```

If you use `public`, you are declaring a function that can be used
anywhere.

```title:example
public fun visible_everywhere() {}
```

```title:example
import "./other_file.gdn" as other_file

other_file::visible_everywhere()
```

The keywords `shared`, `internal` and `external` are currently
reserved for additional visibility levels.
