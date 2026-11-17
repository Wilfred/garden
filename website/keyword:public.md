# `public` keyword

You can use `public` to declare the visibility of your functions and
methods.

If a function or method does not have `public`, it can only be used in
the current file where it is defined.

```title:"Example 1"
fun visible_in_the_current_file() {}
```

If you use `public`, you are declaring a function that can be used
anywhere.

```title:"File: other_file.gdn"
public fun visible_everywhere() {}
```

```title:"Example 2"
import "./other_file.gdn" as other_file

other_file::visible_everywhere()
```

Enum variants can also be marked `public`, so other files can use
them.

```title:"File: other_file.gdn"
public enum Direction {
  public Up,
  Down,
}
```

```title:"Example 3"
import "./other_file.gdn" as other_file

other_file::Up
```

The keyword `shared` is currently reserved for an additional
visibility level.
