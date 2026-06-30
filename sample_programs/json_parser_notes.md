# Writing a JSON parser in Garden: issues encountered

These are the language, standard-library, tooling and documentation
issues I ran into while writing `sample_programs/json.gdn`, a basic
recursive-descent JSON parser. Each entry has a minimal reproduction,
what I expected, what actually happened, and (where I could find it) the
root cause and a suggested fix.

Everything was reproduced with `garden` built from this checkout
(`cargo build`, rustc 1.95.0).

## Summary

| # | Area | Issue | Severity |
|---|------|-------|----------|
| 1 | Runtime | `==` on `Float` is always `False` (e.g. `1.0 == 1.0`) | High |
| 2 | Lexer | A string literal whose contents end in `\` is mis-tokenized as unclosed | High |
| 3 | Parser | All binary operators share one precedence level (`2 + 3 * 4 == 20`) | High |
| 4 | Language | Floats cannot be ordered (`<`, `<=`, `>`, `>=` throw on `Float`) | Medium |
| 5 | Stdlib | No way to parse a `String` into a `Float` | Medium |
| 6 | Stdlib | No codepoint↔character conversion, and only 4 string escapes exist | Medium |
| 7 | Parser | `match` rejects bare tuple patterns, but `let`/`for` accept them | Medium |
| 8 | Tooling | The dead-code check ignores uses inside `test` blocks | Low |
| 9 | Diagnostics | The "use a float operator" hint appears where no float operator exists | Low |
| 10 | Stdlib | No `is_ok` / `is_err` on `Result` | Low |
| 11 | Parser | A binary operator cannot start a continuation line | Low |
| 12 | Diagnostics | The `**` operator prints as `^` in messages | Low |
| 13 | Docs | The required Rust toolchain (1.95.0) is not documented | Low |

The parser works around every issue above, so `garden check`, `garden
test` and `garden run` all succeed on `json.gdn`. The workarounds are
called out in comments in the source.

---

## 1. `==` on `Float` is always `False` (High)

Equality on floating-point values never holds for two separately
constructed values, which silently breaks equality for every list,
tuple, dict, struct or enum that contains a float.

```
$ garden run -c 'dbg(1.0 == 1.0)'   #=> False
$ garden run -c 'dbg(0.0 == 0.0)'   #=> False
$ garden run -c 'let x = 1.0
dbg(x == x)'                        #=> True
```

`x == x` is `True` but `1.0 == 1.0` is `False`. The most confusing
symptom is that two values print identically yet compare unequal:

```
JsonArray([JsonNumber(1.0)]) == JsonArray([JsonNumber(1.0)])   #=> False
# but both sides print as `JsonArray([JsonNumber(1.0)])`
```

**Root cause.** `impl PartialEq for Value_` in `src/values.rs` has no
arm for `Float`, so any two non-identical floats fall through to the
final `_ => false`. (`x == x` works only because `Value` is
`Rc<Value_>`, and `Rc`'s `PartialEq` short-circuits on pointer
identity.) `impl Eq for Value_ {}` is also asserted, even though
`1.0 != 1.0` violates reflexivity.

**Suggested fix.** Add an arm to `impl PartialEq for Value_`. Comparing
with `OrderedFloat` (already a dependency, used in `ast.rs`) keeps the
`Eq` impl sound for `NaN`:

```rust
(Value_::Float(f1), Value_::Float(f2)) => OrderedFloat(*f1) == OrderedFloat(*f2),
```

**Impact on the parser.** The tests in `json.gdn` compare parser output
by serializing back to a `String` rather than comparing `Json` values
with `==`, specifically to avoid this bug.

---

## 2. A string literal ending in a backslash is mis-tokenized (High)

A string literal whose *contents* end in a backslash is reported as an
unclosed string.

```
$ garden run -c 'println("\\")'      # one backslash
Error: Parse error: Unclosed string literal.

$ garden run -c 'println("\\\\")'    # two backslashes
Error: Parse error: Unclosed string literal.

$ garden run -c 'println("\\x")'     # backslash, then x — fine
\x
```

**Root cause.** The string regex in `src/parser/lex.rs`:

```rust
Regex::new(r#"^"(\\"|[^"])*("|\z)"#)
```

The `\\"` alternative matches "backslash then quote" anywhere. In
`"\\"` the two inner backslashes are an escaped backslash, but the
regex pairs the *second* backslash with the closing quote and treats it
as an escaped quote, so the string "runs on" past its real end.

**Suggested fix.** Treat a backslash as escaping whatever follows, and
exclude backslash from the "ordinary character" class:

```rust
Regex::new(r#"^"(\\.|[^"\\])*("|\z)"#)
```

**Impact on the parser.** I cannot write the literal `"\\"`, which a
JSON parser needs in order to handle the `\\` escape and to emit a
backslash. The workaround (`backslash()` in `json.gdn`) slices a
backslash out of `"\x"`, which tokenizes correctly:

```
fun backslash(): String {
  "\\x".substring(0, 1)
}
```

---

## 3. All binary operators have the same precedence (High)

There is no operator precedence; every binary operator has equal
precedence and is left-associative.

```
$ garden run -c 'dbg(2 + 3 * 4)'   #=> 20    (parsed as (2 + 3) * 4)
$ garden run -c 'dbg(2 == 1 + 1)'
Exception: Expected `Int` but `False` has type `Bool`.   # parsed as (2 == 1) + 1
```

This is already a known issue: the unreleased section of `CHANGELOG.md`
lists "**Goal: Fix precedence of `2 + 1 * 3`**", and it is acknowledged
in `src/parser.rs` (a `TODO` near line 23 to use Pratt parsing, and a
comment at the infix-parsing site: "We currently assume that every
operator has the same precedence and is left-associative"). I'm
including it here because it had a real, repeated impact on writing the
parser.

**Impact on the parser.** Compound boolean and arithmetic conditions
must be fully parenthesized. For example `c == "t" || c == "f"` is
parsed as `((c == "t") || c) == "f"` and fails to type-check; it has to
be written `(c == "t") || (c == "f")`. Every `&&`/`||` condition in
`json.gdn` is parenthesized for this reason.

---

## 4. Floats cannot be ordered (Medium)

The comparison operators only work on `Int`; there are no float
comparison operators (no `<.` etc.), and applying `<`/`<=`/`>`/`>=` to a
float throws at runtime.

```
$ garden run -c 'dbg(1.5 < 2.5)'
Exception: Expected `Int` but `1.5` has type `Float`. ...
```

There is no obvious workaround: subtraction yields another `Float`,
which also cannot be compared to `0.0`. This makes general float
ordering impossible today. The parser avoids it (positions and digit
values are all `Int`).

---

## 5. No `String` → `Float` parsing (Medium)

The standard library has `String.as_int(): Option<Int>` but no
equivalent for floats (`as_float` only goes `Int` → `Float`). A JSON
parser has to convert numeric text to a float by hand.

`json.gdn` implements `str_to_float` itself: split off the exponent and
sign, parse the integer and fractional digit groups with `as_int`, and
recombine using float arithmetic and a hand-written `pow10`. A library
`String.as_float(): Option<Float>` would remove ~40 lines of the
parser.

---

## 6. No codepoint conversion, and only four string escapes (Medium)

Garden string literals support exactly four escapes — `\n`, `\t`, `\\`
and `\"` — per `unescape_string` in `src/parser.rs`. There is also no
function anywhere to turn a codepoint into a character/string (no `chr`
/ `char_from_code` / `ord`).

Consequences for a JSON parser:

* The JSON escapes `\r`, `\b`, `\f` and `\uXXXX` cannot be produced at
  all, because there is no way to construct those characters.
* Carriage return cannot be matched or emitted, so it cannot be treated
  as JSON whitespace.

`json.gdn` therefore handles `\" \\ \/ \n \t` and returns a clear
`Err` for `\r`, `\b`, `\f` and `\uXXXX`. This is the parser's main
correctness gap, and it is a language limitation rather than a choice.

---

## 7. `match` rejects bare tuple patterns (Medium)

Tuple patterns work in `let` and `for`, but not as a top-level `match`
arm.

```
let (a, b) = (1, 2)                       # ok
for (k, v) in [(1, 2)] { ... }            # ok

match (1, 2) {
  (x, y) => x + y                         # Error: Expected a variant name after this.
}
```

Tuples can only be destructured in `match` when nested inside a variant
pattern, e.g. `Some((x, y))`. The asymmetry is surprising. In
`json.gdn` I iterate dict entries with `for (key, item) in
entries.items()` instead of matching the tuple.

---

## 8. The dead-code check ignores `test` blocks (Low)

A private function called only from a `test` block is reported as
"never called", and because warnings make `garden check` exit non-zero,
this fails the check.

```
fun used_only_in_test(x: Int): Int { x + 1 }

test t { assert(used_only_in_test(1) == 2) }
```

```
Warning: `used_only_in_test` is never called.
```

Marking the function `public` suppresses the warning (which is what
`is_parse_error` does in `json.gdn`), but a test-only helper arguably
should not need to be public.

---

## 9. Misleading "use a float operator" hint (Low)

Using a non-`Float` operator on a float suggests switching to a float
operator, even in cases where none exists (comparisons, exponent):

```
$ garden run -c 'dbg(1.5 < 2.5)'
Exception: Expected `Int` but `1.5` has type `Float`. Consider using a
float operator (such as `+.` instead of `+`) or using rounding methods
like `.floor()`.

$ garden run -c 'dbg(2.0 ** 3)'
Exception: ... Consider using a float operator ...
```

There is no `<.` or float `**`, so the suggestion sends the reader
looking for an operator that does not exist. The hint is correct for
`+ - * /` but not for comparisons or `**`.

---

## 10. No `is_ok` / `is_err` on `Result` (Low)

`Result` only has `or_throw`. Checking success without unwrapping
requires a `match`. `Option` has `is_some` / `is_none`; `Result` having
`is_ok` / `is_err` would be a natural parallel. (`json.gdn` defines its
own `is_parse_error` helper.)

---

## 11. A binary operator cannot start a continuation line (Low)

End of line terminates an expression, so splitting a long expression
before a binary operator is a parse error:

```
assert(
  stringify(value)
    == "expected"          # Error: Expected `)` after this.
)
```

The operator has to stay on the left operand's line (or the operands
have to be bound to locals first, which is what `json.gdn` does for one
long assertion). This follows from the "EOL terminates expressions"
design, but it is an easy gotcha when formatting long lines.

---

## 12. The `**` operator prints as `^` in messages (Low)

In `src/parser/ast.rs`, `Display for BinaryOperatorKind` maps
`Exponent => "^"`, but the source operator for exponent is `**` (`^` is
string concatenation). Any diagnostic that prints the exponent operator
will show the wrong symbol, and it collides with `StringConcat`, which
also prints as `^`.

---

## 13. The required Rust toolchain is undocumented (Low)

`Cargo.toml` sets `rust-version = "1.95.0"`, but `CLAUDE.md` only says
"use `cargo build`". On a machine with an older stable (1.94.1 here),
`cargo build` fails with a version error until 1.95.0 is installed. A
one-line note (or a `rust-toolchain.toml`) would save the round-trip.
