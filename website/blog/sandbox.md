```metadata
published: false
date: 2025-11-21
title: Devlog 2: The Importance of Sandboxing
```

A core feature of Garden is the ability to run untrusted code in a
sandbox. This is really hard to add to an existing language, but it
enables a ton of wonderful workflows.

## Interactive Documentation

Every code snippet on garden-lang.org has Run and Edit buttons. If you
want to confirm your understanding of a feature, you can experiment.

Here's a really simple example.

```
1 + 2
```

Since we're allowing arbitrary code, the user might write something
more problematic (accidentally or otherwise).

```
while True {}
```

```
Path { p: "/etc/passwd" }.read()
```

If you don't have a sandbox, you might choose to compile your
interpreter to wasm and run it in the browser. This is secure -- the
user is running code on their machine -- but it's slow on mobile. wasm
builds are also fiddly in my experience.

Snippets on the Garden website are executed on my server. This is
scary, but the experience is much nicer to use. In addition to the
interpreter level sandbox, the playground is running on docker on a
dedicated $1 per month server. Security is hard :)

If you do find a security bug, feel free to open an issue on the bug
tracker or email me.

## Eager evaluation

JavaScript is one of the few languages with a mature sandbox, and it's
a crucial part of the modern web. JavaScript also has the ability to
eagerly execute snippets in the console, safely.

```js
// As gif
"foo".toUpperCase()
```

This is small but nice to work with. It saves keystrokes and gets you
feedback faster.

Garden doesn't yet have eager evaluation in the REPL, but it can
eagerly evaluate tests in the sandbox.

(demo)

This will also enable things like safe mutation testing. A scary
number of mutation test frameworks just assume that a small change to
a test suite is safe to execute without review.

This also leverages your compute more. My machine is often mostly idle
when I'm writing code, whereas it could be helping!

## Speculative Library Loading

In an ideal world, writing `require('underscore')` in JavaScript would
be sufficient. The IDE could install the library, provide code
completion, and I'd be ready to to start using it.

There are also projects that do this (JS example). Again, without a
sandbox, this isn't safe. You can reduce the risk with a list of
permitted packages or forbidding post-install scripts, but it's janky.

With a sandbox you can support this workflow properly.

## AI Sandboxing

LLMs are pretty good at iterating on code. Agentic tools rely on an
LLM loop that processes output (compiler errors, test failures etc). This
is often sufficient to guide the LLM to write a working
implementation.

This use case is so compelling that some users will run
`--dangerously-skip-permissions` on their machine directly. Claude
provides guidance for running it in a Docker container, and
claude.ai/code also provides an isolated VM to safely allow this
workflow.

These workflows prevent the AI from e.g. deleting all your files, but
make it harder to interactively guide the AI towards the desired
solution. A sandbox lets you run the LLM and interpreter as a normal
program, getting the best of both worlds.

## Program Synthesis

I find myself writing programs where the rest of the program is
obvious.

```
test max {
  assert(max(2, 3) == 3)
  assert(max(4, 3) == 4)
}

fun max(x: Int, y: Int): Int {
  if x > y {
    return x
  }

  // There aren't many possibilities here!
  todo()
}
```

This is a 'program synthesis' problem, asking the computer to write
code for you based on examples. You can apply a simple brute-force
solution ('enumerative synthesis), a smarter type-driven search, or
just feed it to an LLM.

## Security

Enforcing memory liimtations.

Not defending against side channel attacks like rowhammer or spectre.

## Open Questions

Should this be a userland feature?
