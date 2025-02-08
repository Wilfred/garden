# Introducing the Garden Programming Language

I've decided to create a programming language!

I enjoy contributing to other programming languages, but I've never
been entirely happy with any language I've used. Programming languages
are missing features that I want, and I can't add these features to an
existing language.

## Interactive Evaluation

Most programming languages require you to start over when you change
something.

Fixed an off-by-one error? Compile and re-run the program to see if it
works.

Added a field to a struct? Compile and re-run the program to see if it
works.

Defined a new function? Add a call to the function, compile, and
re-run the program.

Programming doesn't have to be like this. Programming in Pharo
Smalltalk or Common Lisp lets you make change against a running
program.

This is sometimes called a REPL, after the Read-Eval-Print-Loop
environment in Lisp, but that term leads to confusion. GHCI for
Haskell is a REPL of sorts, but you can't redefine types and call
existing functions.

Interactive evaluation requires language-level support for very late
binding. It can't be added to an existing language.

## Sandboxing

My computer doesn't try hard enough when I'm coding. I rarely use more
than one core, and I'm writing this on a 12 core machine.

If I start writing a function, my IDE should speculatively evaluate it
and see if it passes all my tests. Consider a max function:

```
fun max(x: Int, y: Int): Int {
  if x > y {
    return x
  }
  
  // cursor is here
}

test max {
  assert(max(1, 2) == 2)
  assert(max(3, 2) == 3)
}

```

There are very few possibilities here, and only one is correct. Why
can't my IDE help me here?

Doing this properly requires a reliable sandbox. To safely run
arbitrary code, the language implementation needs to help. JavaScript
is the only major sandboxed language today.

## Why Garden?

What happens when plants in a garden don't grow exactly how you hoped?

You don't level the whole area and plant seeds again. You modify the
existing situation until it meets your goals anyway.

A good garden is also a pleasant environment to be in.

So that's Garden. A tooling-first, interactive, sandboxed programming
language. Wish me luck.
