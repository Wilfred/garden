# 0.23 (unreleased)
**Goal: LSP.**
**Goal: Offer actions when runtime errors occur.**

## Tooling

Garden now has an LSP server providing go-to-definition, code
completion, quickfixes and basic formatting. You can start it with
`garden lsp`.

## Syntax

Documentation is now written with triple slashes, e.g. `///`, to
distinguish from normal comments.

## Standard Library

Added `read_line()` for reading a single line of text from stdin.

Added `Dict::remove()`.

## Commands

`:file` is now `:namespace`.

## CLI

Added a `format` subcommand that fixes indentation in Garden files.

Added a `run-code-blocks` subcommand that evaluates code snippets in
.md files and in doc comments, and checks the output against `//->`
comments (if present).

Added `--fix` to `garden check`.

`sandboxed-test` now accepts a `--override-path` argument.

# 0.22 (released 15 December 2025)
**Goal: Playground.**

## Syntax

Added a Dict type, a key-value store with string keys.

```
let prices = Dict["apple" => 2, "banana" => 1]
```

Visibility is now set with `public` instead of `external`.

```
// New
public fun foo() {}

// Old
external fun foo() {}
```

Integer literals now support underscores to make them easier to
read. `10000` can now be written `10_000`.

## Runtime

Fixed a crash when evaluating empty blocks.

Fixed a crash on `foo % 0`. `%` is now Euclidean modulo with
documented behaviour on negative values.

## Standard Library

`__garden.gdn` is now `__reflect.gdn`.

Added `min()` and `max()`.

Added `Dict::get()`, `Dict::set()` and `Dict::items()`.

Added `__random::int()`.

Added `__fs::create_dir()`, `__fs::remove_dir()`,
`__fs::copy_file()`, and `__fs::remove_file()`.

Added `__time::unixtime()`.

Added `__reflect::operators()` and `__reflect::built_in_files()`.

## Build

Garden now requires Rust 1.85+ to build.

## Check

`garden check` now warns on unused type variables.

## IDE Operations

Destructure, extract variable, extract function and rename now return
a non-zero exit code if they weren't able to perform the requested
operation.

# 0.21 (released 23 August 2025)
**Goal: Blog.**
**Goal: Comprehensive docs on website.**

## Syntax

Methods are now defined with the `method` keyword rather than the
`fun` keyword.

```
// New
method my_append(this: String, s: String) {
  this ^ s
}

// Old
fun (this: String) my_append(s: String) {
  this ^ s
}
```

Expressions may now contains multiple operators, e.g. `2 * 3 + 4` is
now legal syntax. Note that Garden all operators have the same
precedence and are left-associative, so `1 + 2 * 3` is the same as
`(1 + 2) * 3` and produces 9.

Fixed an inconsistency with the parser, where function calls were not
allowed after keywords. The following is now legal syntax:

```
if True { println } else { println }("hello world")
```

`public` and `shared` are now keywords.

## Standard Library

Added `List::index_of()`, `String::as_int()`, `String::trim()`,
`get_env()`, `garden::source_for_fun`, `garden::source_for_method()`,
`garden::doc_comment()`, `garden::doc_comment_for_method()`, and
`garden::methods_for_type()`.

Fixed an issue with `__fs::list_directory()` returning an invalid
list.

`Path::parent()` now return an `Option<Path>` on `/` or unqualified
file names `foo.txt`.

`"".split(anything)` now returns `[]` rather than `[""]`.

Renamed `Option::or_exception()` to `Option::or_throw()`. Renamed
`Result::or_error()` to `Result::or_throw()`.

`error()` is now `throw()`.

## Checks

`check` now stops if syntax errors occur, as type errors are rarely
actionable or accurate when syntax errors are present.

## Test Runner

`garden test` now supports a `-n` argument that lets you specify a
test name substring to use.

# 0.20 (released 1st June 2025)
**Goal: Namespaces.**

## Diagnostics

Error and warning printing now uses a new formatter, showing context
and syntax highlighting the source code.

Diagnostics can now show multiple relevant positions.

## Checks

`import "x.gdn" as y` now warns if `y` is unused.

## Standard Library

`write_file()` is now `__fs::write_file()`, `list_directory()` is now
`__fs::list_directory()`, `working_directory()` is now
`__fs::working_directory()`, and `set_working_directory()` is now
`__fs::set_working_directory()`.

`keywords()` is now `__garden::keywords()`, `type_source()` is now
`__garden::source_for_type()`, `lex()` is now `__garden::lex()`,
`check_snippet()` is now `__garden::check_snippet()`,
`type_doc_comment()` is now `__garden::doc_comment_for_type()`,
`built_in_types` is now `__garden::prelude_types`.

The `__fs` and `__garden` definitions are no longer in the prelude,
and must be explicitly imported. For example:

```
import "__fs.gdn" as fs

dbg(fs::list_directory())
```

`Path::file_name()` now handles paths with trailing slashes correctly.

Added `Path::set_extension()`.

Added `__garden::namespace_functions`.

## Syntax

`as` is now a keyword.

## Commands

Added a `:globals` command to view all values in the current
namespace.

Added a `:file` command to view which file namespace the current stack
frame is in.

Added a `:forget_calls` command to discard saved values from previous
function and method calls.

## CLI

`garden test` can now take multiple file arguments.

# 0.19 (released 7th April 2025)
**Goal: Website has snippets that are checked and linkified.**

## Running Code

Garden no longer executes a function called `main`. Instead, any
toplevel blocks in the current file are evaluated in order.

## Syntax

Added `^` to concatenate strings.

Added `**` to raise a number to a power.

`assert` is now syntax rather than a built-in function. The symbol
`assert` is now a keyword.

Destructuring is now supported in loop headers and `match` cases, so the following is
now legal:

```
for (x, y) in [(1, 2), (3, 4)] {
  dbg(x * y)
}

match Some((1, 2)) {
  Some((x, y)) => x + y,
  None => 0,
}
```

Functions and types visible in other files now use the `external`
keyword, rather than `export`. `internal` is now a keyword too.

## Standard Library

Removed `String::append`.

Added `String::replace`, `List::enumerate`, `lex`, `check_snippet`,
`shell_arguments`, `keywords`, `type_doc_comment`, `type_source`,
`built_in_types` and `set_working_directory`.

`List::get` now returns an option rather than erroring on
out-of-bounds access.

`write_file` now takes a `Path` instead of a string.

`Path::parent` now handles `/` correctly.

## Checks

Fixed several type checking bugs on local variables and on type
parameters in methods.

The unused function check now reports functions that are only used in
tests.

## Tooling

Renamed `eval_up_to_id` JSON command to `eval_up_to`.

The `check --json` CLI command now returns line and columns in its
output, instead of offsets.

## Build

Garden now requires Rust 1.79 to build.

# 0.18 (released 31st December 2024)
**Goal: Speculative sandboxed execution of tests and completions.**

## Standard Library

Added a `Path` type, with methods `Path::exists`, `Path::extension`,
`Path::file_name`, `Path::join`, `Path::parent` and `Path::read`.

Added `List::slice`, `Option::is_some()`, `Option::is_none()` and
`Option::or_value()`.

`list_directory` and `working_directory` now return absolute `Path`
values. `list_directory` now takes a `Path` argument.

Removed `path_exists` (use `Path::exists` instead) and `read_file`
(use `Path::read` instead).

`String` methods are now substantially faster, 10x in common cases.

Added `source_directory()`, `assert()`, `range()` and removed
`should_equal()`.

Removed `List::for_each` (use `for x in y` loops instead).

## Syntax

Type parameters now occur after function or method names, so Garden
programs are more easily searched with `grep` or `rg`.

```
// Old
fun<T> foo(items: List<T>) {}

// New
fun foo<T>(items: List<T>) {}
```

Tests now require a name, so `test {}` is no longer legal. Use `test
some_name {}` instead.

Added `%`, `+=` and `-=` operators.

## Checks

Fixed several type checking bugs.

## Tooling

Added a `destructure` and an `extract-function` CLI commands.

# 0.17 (released 15th September 2024)
**Goal: WASM and website.**
**Goal: Less compulsory punctuation following eval-up-to-cursor testing.**

## Language

Expressions no longer use trailing semicolons, so `let x = 1;` is now
`let x = 1`.

Added tuples, e.g. `(1, "Foo")`.

Added `for x in some_list` loops, and added `continue` to both `while`
and `for` loops.

Variable shadowing is now supported, so the following is legal:

```
let x = 1;
let x = x + 1; // 2
```

Struct literals now require the opening brace to be touching the type
name, so `Person { name: "wilfred" }` is now written
`Person{ name: "wilfred" }`.

`while` loops, `match` expressions and `if` expressions no longer
require parentheses, so `while x {}`, `match x {}` and `if True {}`
are now valid.

## Runtime

Fixed an interpreter crash when evaluating tests.

## Stdlib

Added `List::for_each`, `List::is_non_empty`, `String::chars` and
`Bool::not`.

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

Added `break;` inside loops. `continue` is now a keyword.

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
no longer keywords, and boolean values are now `True` and `False`.

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
