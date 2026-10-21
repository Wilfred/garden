# Scheme interpreter implementation notes

The Scheme interpreter in `scheme.gdn` supports integers, booleans,
strings, symbols and proper lists. It implements quoting, `if`, `begin`,
top-level `define`, lexical closures, named recursion and a small set of
numeric, comparison, list, predicate and output primitives.

The REPL reads and evaluates one expression per line. It deliberately does
not implement floating-point numbers, escaped Scheme strings, dotted pairs,
variadic lambdas, mutation, macros, tail-call optimisation or a garbage
collector separate from Garden's runtime.

## Language issues

Garden enum variants take at most one payload. Representing a procedure
therefore requires a tuple payload such as:

```garden
LambdaValue((List<String>, List<Expression>, Environment, Option<String>))
```

Patterns can destructure enum and tuple payloads, but they cannot match a
literal inside a payload. For example, `Some(")")` and
`BooleanValue(False)` are rejected. The payload must first be bound and then
compared in an `if` expression.

There is no catch-all binding pattern. A bare lower-case name in a match arm
is interpreted as a type or variant rather than as a binding. Code that
needs the complete unmatched value must bind the scrutinee before the match
and use `_` for the catch-all arm.

An infinite `while True` expression has type `Unit`, even when every way out
of its body returns from the containing function. At the same time, the
static analysis reports code following that loop as unreachable. This makes
the flow analysis contradictory: removing the reported unreachable value
can cause a function return-type error.

Garden string literals do not support `\r`. The invalid-escape diagnostic
rendered the offending sequence as `\\`, rather than `\r`, while developing
the tokenizer.

Garden provides `+=` and `-=`, but not `*=`. Multiplication assignment must
be written as `value = value * factor`.

## Diagnostics and checks

Writing an enum variant with multiple payloads produced several follow-on
parser errors after the first useful error. Literal enum payload patterns
similarly produced a cascade of errors about symbols, closing parentheses,
match arrows, variants and braces. Both cases would be clearer with a single
diagnostic at the unsupported construct.

The checker otherwise caught useful implementation errors, including
non-exhaustive matches, duplicate match arms, unbound names, return-type
mismatches and unused pattern bindings. `garden check` passes cleanly for the
finished interpreter.

## Documentation issues

The `enum` manual page demonstrates a variant with one payload, but does not
state that this is the maximum or show the tuple workaround for variants
which carry several values.

The `match` manual page documents enum and tuple destructuring and `_`, but
does not state that literal payload patterns and catch-all binding patterns
are unsupported.

The manual does not document the supported string escape sequences. In
particular, it gives no indication that `\r` is unavailable.

## Tooling issues

Running `cargo fmt --check` with stable Rust prints a warning because the
repository's `.rustfmt.toml` sets the nightly-only `imports_granularity`
option. Formatting still succeeds.

The full `cargo test reftest` run had two failures unrelated to the
interpreter:

- The nREPL interrupt response sequence differed from its fixture around the
  interrupted evaluation and interrupt acknowledgement.
- The invalid shell-command runtime reftest received
  `Permission denied (os error 13)` in the sandbox where its fixture expects
  `No such file or directory (os error 2)`.

The interpreter itself passes `garden check`, `garden format --check`, its
four embedded tests and manual REPL exercises covering lexical capture,
recursion and recoverable errors. The Rust workspace passes Clippy across all
targets and features.
