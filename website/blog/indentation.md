```metadata
published: false
date: 2026-02-02
title: Devlog 3: Indents and Indentability
```

There are few engineering debates more heated and more shallow than
code style debates. Unfortunately you have to pick something for a
branch new language.

(c.f. the haskell bikeshedding scale.)

Early designs tend to stick. I initially chose four spaces, and then
switched to two spaces. Design changes are always interesting.

## What's the goal here?

I want an indentation size that works best for Garden code. I started
with four spaces, as a safe bet considering how popular it is with
other languages.

The Linux kernel is the only major project I've seen with eight space
indentation, and that feels like too much. Horizontal space is a
finite resource.

However, garden uses `match` statements extensively. When writing
`match` in Rust, there's a ton of syntactic sugar to reduce the
indentation level for common `match` patterns. `if let`, `let
Some(...)` etc.

I don't want to add those: `match` is a lovely, general feature and
I'd rather use it directly.

I've realised that other languages that use `match` extensively, such
as OCaml, use two spaces. I ended up doing the same thing in garden
and feel much better about it.

## Tooling support

One wonderful feature of brace-structured syntax is that you can
delete an if statement and just re-indent. You can't do this with
significant whitespace like e.g. python.

Garden now has a basic autoformatter. It only fixes blocks so far, but
it gives most of the devx win.
