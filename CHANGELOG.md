# 0.3 (unreleased)

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
