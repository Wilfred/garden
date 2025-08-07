# `external` and `internal` keywords

You can use `external` and `internal` to declare the visibility of
your functions and methods.

If you don't declare visibility, Garden will assume that your function
or method is only used in the current file.

```title:example
fun visible_in_the_current_file() {}
```

If you use `external`, you are declaring a function that can be used
anywhere.

```title:example
external fun visible_everywhere() {}
```

`internal` is reserved for declaring a function that can be used
from any file in the current library, but not outside the library.
