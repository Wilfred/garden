# Markdown Check

A code block with a type error.

```
let x: Int = "hello"
```

A second block that is fine.

```garden
1 + 2
```

// args: check
// expected exit status: 1
// expected stdout:
// Error: Expected `Int`, but got `String`.
// --| src/test_files/check/markdown_type_error.md:6:14
//  5| ```
//  6| let x: Int = "hello"
//  7| ```          ^^^^^^^
