# An Experiment in Developer Experience

Garden is a programming language created to explore developer
tooling. It's very slow, very incomplete, but enables some interesting
experiences.

## Incremental Programming

Very few programming languages allow you to redefine functions and
types whilst your program runs. Smalltalk and Lisp actively encourage
this. A few other languages kinda allow it, if you're willing to
commit lovecraftian horrors and accept the limitations.

Incremental programming is a feature that requires runtime support to
work properly. Typechecking code written incrementally is also
challenging.

It's a wonderful way to write code, and I want more options in this
space. Garden is designed to use incremental programming everywhere.

## Sandboxing

It's also really hard to add a sandbox to an existing language. Having
a sandbox enables a bunch of interesting experiences.

You can speculatively execute work-in-progress code to see if tests
pass. You can safely do mutation testing, program synthesis,
speculative library installation, or LLM experiments.

Garden has a basic sandbox today, and I hope to add a pledge-style
granular sandbox in future.

## Can I use it?

Garden is extremely incomplete and slow. Its focus on tooling
means that I added an 'extract function' operation before I added a garbage
collector! Even now, the GC is bad and doesn't handle cycles at all.

Garden is being fully developed in the open, but please set your
expectations low.

## Why 'Garden'?

If you have a vegetable garden and one vegetable dies because of
frost, you don't raze the whole plot and start over. Programming
should be like this, rather than starting from fresh every time.

If Garden succeeds, it should be a pleasant environment to work in
too.
