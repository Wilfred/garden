# 0.10 (unreleased)
**Goal: Offer actions when runtime errors occur.**

# 0.9 (unreleased)
**Goal: User-defined enum types.**

### Commands

Added `:forget`, `:types` and `:uptime`.

### Stdlib

`Void` has been renamed to `Unit` and moved to the prelude.

The function `lines()` has been replaced with `String::lines`.

# 0.8 (released 30th October 2023)
**Goal: Red and yellow squiggles.**

### Stdlib

Added functions: `list_directory` and `should_equal`.

Added methods: `String::join`, `String::contains`, `String::lines`, `List::filter`,
and `List::is_empty()`.

### Editor

Emacs sessions now handle large outputs without crashing.

# 0.7 (released 21st October 2023)

**Goal: Support interactive unit tests.**

### Syntax

A local variable or parameter named `_` is now always ignored. The
following is now legal:

```
fun f(_, _) {}
fun g() {
    let _ = 1;
    let _ = 2;
}
```

### Stdlib

Implemented `List::append`, `List::get`, `List::len`,
`String::concat`, `String::len` and `String::substring` methods,
removing the corresponding `list_append`, `list_get`, `list_len`,
`string_concat`, `string_length` and `string_substring functions.

Added `error()` and `string_repr()` functions. Removed `int_to_string()`
in favour of `string_repr()`.

### Commands

Added `:forget_local` and `:test`.

### Evaluation

Fixed a crash on calling method calls with the wrong arity.

### Parsing

Repeating a parameter is now a parse error.

# 0.6 (released 16th September 2023)

**Goal: Support method calls.**

### Syntax

Functions and methods can now specify the return type and parameter
types. That type will be enforced at runtime.

Added `<=` and `>=` comparison operators.

Added parsing and evaluation support for methods.

Symbols may now include uppercase ASCII letters, so `Foo` is a legal
function name.

### Commands

Added `:type` to show the type of an expression.

Added `:methods` to show all the defined methods available.

### Stdlib

Added `println` and changed `print` to not include a trailing newline.

# 0.5 (released 27th August 2023)

**Goal: Define a real program that solves a problem for me.**

### Stdlib

Added `dbg`, `list_get`, `list_length`, `path_exists`, `string_concat`
and `working_directory`.

### Commands

Renamed `:globals` to `:funs` to reflect its current purpose.

### Syntax

Fixed parsing of repeated calls, e.g. `f()()` was previously not
permitted.

Added closures, e.g. `fun(x, y) { x + y; }`.

If the first line of a file starts with `#`, it will be ignored,
enabling use of `garden` in a shebang.

### Interpreter

`main` functions are now always called with one argument, the CLI
arguments that are passed.

# 0.4 (released 9 June 2023)

**Goal: Build up and evaluate blocks of code**

Interactive sessions: Added the `:search` command for finding
definitions with a given name.

Syntax: `if` is now an expression, so the following is legal:

```
let x = if (y) { 1; } else { 2; };
```

Syntax: Added support for escaping in string literals, so `"\n"`,
`"\\"` and `"\""` now work correctly.

Syntax: Added list literals, e.g. `[1, 2 + 3]`.

Syntax: Added negative int literals, e.g. `-123`.

Syntax: `==` and `!=` now work for all types.

Syntax: Added block expressions, e.g. `{ foo(); bar(); }` evaluates
`foo()` and returns the value of `bar()`.

Syntax: `if` and `while` now have block scope, so local variables
introduced there are only available until the end of the block.

Standard library: Added the functions `list_append`,
`string_substring` and `string_length`.

Garden executable: Added the `run` command, so the following now
works:

```
$ garden run sample_programs/hello_world.gdn
```

Commands: `:abort` exits out of any blocks as well as functions.

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
