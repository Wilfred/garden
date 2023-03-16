## Pervasive Re-Evaluation

Build the language around a REPL experience.

Syntax where every subexpression has a closing marker that can show
the last result of that expression.

## Immutable Default Data Types

Vector, hashmap.

Numeric types based on hardware reality (i64, f64).

Type organised around modules with OO-style `.` dispatch.

## Gradual Types

Sum and product types that support redefinitions.

A type checker that allows type or function redefinition.

## First Class Sandboxes

Lighweight sandboxing (in the style of OpenBSD pledge).

## Interactive Experience

Values are preserved and can be inspected. Their string representation
is secondary.

Pervasive docs that can be explored interactively.

Distinguish values that must be checked (i.e. Result) from resumable,
developer-oriented exceptions.

Expressions evaluated are written to a log, so users can copy them
(although the primary workflow should be in an editor).

Unbounded stack (stored on heap).

Combine type and value information when inspecting bad stacks.
