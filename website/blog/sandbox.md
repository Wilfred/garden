```metadata
published: false
date: 2026-01-05
title: Devlog 2: The Importance of Sandboxing
```

A core feature of Garden is the ability to run untrusted code in a
sandbox. Untrusted evaluation is extremely hard to add to an existing
language, but Garden is brand new. Sandboxing enables some wonderful
workflows.

## Interactive Documentation

Every code snippet on this website has Run and Edit buttons. If you
want to confirm your understanding of a feature, you can experiment.

Here's a really simple example.

```
1 + 2
```

Since we're allowing arbitrary code, the user might write something
more problematic (accidentally or otherwise). The sandbox ensures we
can run this safely.

```
while True {}
```

```
Path{ p: "/etc/passwd" }.read()
```

Some programming languages choose to compile their interpreter to
wasm, so the execution happens in the browser. This is secure --
everything occurs on the reader's machine -- but it's slow on
mobile. wasm builds also impose additional constraints on the
interpreter.

Snippets on this website are executed on my server, using the
interpreter's sandbox feature. This was scary to deploy, but the
experience is really nice to use. I've locked down the execution
process in a Docker container on a dedicated $1 per month server.

If you *do* find a security bug, feel free to open an issue on the bug
tracker or email me.

## Eager evaluation

JavaScript is one of the few languages with a mature sandbox, and it's
a crucial part of the modern web. JavaScript also has the ability to
eagerly execute snippets in the console, safely.

<video controls>
  <source src="/speculative_js.mp4" type="video/mp4" />
</video>

You can see that the interpreter shows `"FOO"` before we press enter.
This is a small feature, but lovely to work with. It saves keystrokes
and gets you feedback faster.

Garden doesn't yet have eager evaluation in the REPL, but it can
eagerly evaluate tests in the sandbox. Eager tests also leverage your
hardware better. My machine is often mostly idle when I'm writing
code, whereas it could be helping!

<video controls>
  <source src="/speculative_assertions.mp4" type="video/mp4" />
</video>

Note how the failing assertion immediately changes as I edit the code
in this video.

Sandboxed testing also enables safe mutation testing. A scary number
of mutation test frameworks just assume that a small change to a test
suite is safe to execute without review.

## Speculative Library Loading

In an ideal world, writing `require("lodash")` in JavaScript would
be sufficient. The IDE could install the library and no additional
keystrokes would be required.

There are also projects that do this (such as [auto-install for
JS](https://github.com/siddharthkp/auto-install)). Again, without a
sandbox, this isn't safe. You can reduce the risk with an allowlist of
permitted packages or forbidding post-install scripts, but it's janky.

With a sandbox you can support this workflow properly.

## Program Synthesis

I frequently find myself writing programs where the rest of the
program is obvious.

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
solution ('enumerative synthesis'), a smarter type-driven search, or
just feed it to an LLM.

With a sandbox, you could evaluate all the obvious possibilities and
-- if they pass the tests -- suggest them to the user.

## AI Sandboxing

LLMs are pretty good at iterating on code. Agentic tools rely on an
LLM loop that processes output (static errors, runtime errors, test
failures etc). This is often sufficient to guide the LLM to write a
working implementation.

This use case is so compelling that some users will run
`--dangerously-skip-permissions` on their machine directly. Claude
provides guidance for running it in a Docker container, and
[claude.ai/code](https://claude.ai/code/) also provides an isolated VM
to safely allow this workflow.

These workflows prevent the AI from e.g. deleting all your files, but
make it harder to interactively guide the AI towards the desired
solution. A sandbox lets you run the LLM and interpreter as normal
programs, getting the best of both worlds.

## Security

Safely supporting arbitrary code execution is the scariest, most
security-sensitive code I've ever written. I've spent a good while
trying to figure out my security posture.

The Garden interpreter sandbox prevents I/O (disk, network) as well as
enforcing resource limits. The current implementation is slightly lax
on memory limits, so a sufficiently problematic program could still
OOM the interpreter.

The sandbox does not provide protection against side channel attacks,
such as rowhammer or spectre.

## Future Work

I'm delighted to have live evaluation on the website, but there's
still a ton of features that could be built on top of this sandbox.

If you've made it to the end of this long post, thanks for reading!
Garden is still very much alpha software, but it's all on GitHub if
you want to try it.
