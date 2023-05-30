# 0.4 (unreleased)

Syntax: `if` is now an expression, so the following is legal:

```
let x = if (y) { 1; } else { 2; };
```

Syntax: Added support for escaping in string literals, so `"\n"`,
`"\\"` and `"\""` now work correctly.

Syntax: Added list literals, e.g. `[1, 2 + 3]`.

Standard library: Added the functions `list_append`,
`string_substring` and `string_length`.

Garden executable: Added the `run` command, so the following now
works:

```
$ garden run sample_programs/hello_world.gdn
```

# 0.3 (released 17 May 2023)

**Goal: Basic Emacs integration**

Syntax: Added `//` comment syntax. Added `return`. Allowed `if`
without `else`, as well as `else if (...) { ... }`.

Interactive interpreter: added `:abort`, `:doc`, `fstmts`, `:replace`,
`:resume`, `:skip`, `:trace` and `:quit` commands. `:help` can now
show information on what commands do.

# 0.2 (released 14 March 2023)

**Goal: Run FizzBuzz**

Syntax: Added `if`, `while`, assignment, `!=`, `-`, `*`, `/`, `<`,
`>`, `||` and `&&`.

Standard library: Added `int_to_string`.

Interactive interpreter: added `:stack` and `:source`
commands. Commands can now be run when errors occur.

# 0.1 (released 11 February 2023)

**Goal: Run Hello World**

A basic toplevel that supports hello world.
