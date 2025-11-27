```metadata
published: true
date: 2025-11-21
title: Devlog 2: The Importance of Sandboxing
```

Sandboxing is a feature that's almost impossible to add to a language
after the fact. It enables a bunch of interesting ideas.

## Features

* Safe eager evaluation in the REPL.

* Safe eager evaluation of tests.

* Safe evaluation of arbitrary LLM-generated snippets.

* Safe speculative installation of external libraries.

* Inline evaluation on the official website.

You can do these with a Docker image or a VM, but it's slower and less
convenient. People rarely do this in practice.

## Existing Languages

Today, the only major language with a decent sandbox is JS.
