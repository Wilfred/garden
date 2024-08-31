# 0.17 (unreleased)
**Goal: WASM and website.**
**Goal: Offer actions when runtime errors occur.**
**Goal: Speculative sandboxed execution of tests and completions.**

## Language

Variable shadowing is now supported, so the following is legal:

```
let x = 1;
let x = x + 1; // 2
```

## Runtime

Fixed an interpreter crash when evaluating tests.

## Stdlib

Added `List::for_each`.

## Checks

Fixed several type checking bugs.

# 0.16 (released 11th August 2024)
**Goal: Evaluate functions up to the cursor.**

## Tooling

Added an `eval_up_to_id` JSON command that allows functions or blocks
to be evaluated up to a specific expression. For functions, the
previous argument values used are passed in.

The command `:methods` now takes an additional argument, to only
show items matching that substring, e.g. `:methods Str`.

Fixed several crashes, particularly on incomplete programs.

# 0.15 (released 15th July 2024)
**Goal: Basic IDE features.**

## Tooling

Added a command `garden show-type foo.gdn 123` that shows the type of
the expression in file `foo.gdn` at offset 123.

Added a command `garden definition-position foo.gdn 123` that shows
the definition position of the symbol in file `foo.gdn` at offset 123.

Added a command `garden definition-position foo.gdn 123` that shows
the definition position of the symbol in file `foo.gdn` at offset 123.

Added a command `garden complete foo.gdn 123` that shows all the
methods available for the type of the expression in file `foo.gdn` at
offset 123.

Configured Emacs mode to show hover information, provide go-to-def and
code completion.

## Syntax

There is now a syntax for function types. For example, `Fun<(Int,
Int), Int>` is the type of the function `fun(x, y) { x + y; }`.

## Stdlib

`dbg` now returns its argument, so it can be added to subexpressions.

Added `String::split`, `List::map` and `List::last`.

Renamed `String::concat` to `String::append`.

## Checks

The type checker now solves generics on arbitrary user-defined types.

The type checker also handles many more expressions correctly.

## Build

Garden now requires Rust 1.78 to build.

# 0.14 (released 16th May 2024)

Fixing bugs with the GitHub actions for uploading releases to crates.io.

# 0.13 (released 16th May 2024)
**Goal: Implement generics.**
**Goal: Implement type checker.**

## Syntax

Added `break;` inside loops. `continue` is now a reserved word.

`let x = y;` now evaluates to `Unit` rather than `y`.

## Checks

Type hints for built-in types are now checked for the correct arity
(e.g. `List` or `String<Foo>`).

Added a basic type checker.

## Stdlib

Added methods `write_file()`, `String::trim_left()`,
`String::trim_right()`, `String::starts_with()`,
`String::ends_with()`, `String::strip_prefix()`,
`String::strip_suffix()`, `String::split_once()` and
`Option::or_exception()`.

Added function `todo()`.

`list_directory` now returns a `Result`.

Fixed a crash on `shell()` with a nonexistent command.

## Commands

`:doc` now prints the position of functions.

`:funs` now only shows callable values in the file scope.

## Test Runner

If tests fail, the test runner now has an exit code of 1.

## Build

Garden now requires Rust 1.73 to build.

# 0.12 (released 3 April 2024)
**Goal: Add structs.**

## Emacs

`C-c C-z` now toggles between session and source code buffers.

## Stdlib

Added the `read_file` function.

`shell` now returns a `Result`, where `Err` represents a non-zero exit
code.

Added `Result::or_error()`.

Added a `NoValue` type.

## Syntax

Added structs.

Match cases now support an optional trailing comma.

```
match (x) {
  None => 0,
  Some(i) => { i + 1; }
}
```

`return;` is now syntax sugar for `return Unit;`.

## Checks

Added a type hint arity checker.

## Website

Added a placeholder site with a parser sandbox.

# 0.11 (released 30th December 2023)

### Syntax

Added pattern matching.

```
match (x) {
  Some(i) => { i + 1; }
  _ => 0
}
```

### Stdlib

Added the `Result` type.

Added the `List::first` method.

### Commands

`:source` now shows the source code of a single definition, rather
than the session history.

### Warnings

Fixed an issue where use of function parameters would generate a
warning about unbound symbols.

# 0.10 (released 30th November 2023)

**Goal: Warn on loading functions with free variables.**

### Warnings

A warning is generated when loading a function with non-existent type
names.

A warning is generated when loading a function with unbound symbols.

### Interactive Sessions

Loading a function will now display any warnings generated.

### Tests

Fixed a crash on evaluating an empty test.

### Emacs

Added `garden-switch-to-session` (bound to `C-c C-z`) to switch to the
`*garden*` buffer from a `garden-mode` buffer.

# 0.9 (released 13th November 2023)
**Goal: User-defined enum types.**

### Commands

Added `:forget`, `:types` and `:uptime`. `:doc` now supports types as
well as functions.

### Stdlib

`Void` has been renamed to `Unit` and moved to the prelude.

The function `lines()` has been replaced with `String::lines`.

### Booleans

`Bool` is now an enum defined in the prelude. `true` and `false` are
no longer reserved words, and boolean values are now `True` and `False`.

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
