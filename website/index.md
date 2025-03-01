# 🌻 The Garden Programming Language

Garden is an experimental programming language focused on interactive
tooling.

```
fun main(_args: List<String>) {
  print("hello world")
}
```

## Taking REPLs Seriously

Common Lisp and Smalltalk have a superpower: you can modify running
programs. You can redefine functions and modify class definitions
without restarting your program.

Garden aims to provide that superpower in a conventional language
design.

## Tooling First

Garden prioritises developer experience over language
functionality. The syntax and semantics are deliberately small, so a
small team (or just me!) can build a delightful, credible language
experience.

## Just Enough Typing

Most gradual type systems are intended for incremental adoption in a
dynamically typed language. Garden is new, but requires a typing model that
works with code of the form:

```garden nocheck
fun foo(): String {
  return ""
}

fun bar(): Unit {
  eval("fun foo(): Int { return 1 }")
  let x = foo() + 1
}
```

## Sandboxed Execution

Garden supports sandbox execution without third-party libraries. This
is important for two workflows:

(1) The IDE can speculatively evaluate snippets of code safely. This
is convenient and enables 'mutation testing' for code completion.

(2) Libraries can declare what capabilities they need, such as network
or disk. This makes third-party libraries safer to install. This will
be coarse permissions, similar to OpenBSD's Pledge.

## Tooling Oriented Dynamism

Garden splits its semantics into (1) simple evaluation that is
amenable to static analysis, and (2) reflective, dynamic evaluation
that is friendly to developer experience.

This means that errors in userland are plain `Result` types, but
unhandled errors can be fixed at runtime by a programmer.
