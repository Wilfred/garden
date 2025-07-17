# `struct` keyword

`struct` defines a type made up of fields. Structs are immutable.

```
struct User {
  email: String,
  admin: Bool,
}

let alice = User{ 
  email: "alice@example.com",
  admin: True,
}
```

To avoid ambiguity, struct literals cannot have a space between the
type name and curly brace. `User{` cannot be written `User {`.
