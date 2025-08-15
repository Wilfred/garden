```metadata
published: true
date: 2025-07-13
title: Choosing a Syntax
```

To kick things off, I want to talk about the basic syntax of
Garden. Garden has a deliberately conventional syntax.

```
fun fib(i: Int): Int {
  if i < 2 {
    return 1
  }

  fib(i - 1) + fib(i - 2)
}
```

I'm trying to avoid spending my [strangeness
budget](https://steveklabnik.com/writing/the-language-strangeness-budget/)
on syntax.

## Keywords

I want keywords to be short. This follows [Stroustrup's
Rule](https://buttondown.com/hillelwayne/archive/stroustrups-rule/),
which favours short syntax for common operations. There's little
benefit for `function` in JavaScript, it's just more typing on the
keyboard.

However, I want keywords to be pronounceable, hence `fun` instead of
`fn`. This is based on the Strange Loop talk [How to teach programming
(and other things)?](https://youtu.be/g1ib43q3uXQ?t=2165) by Felienne
Hermans (creator of [Hedy](https://hedy.org/)). Felienne talks about
the advantages of reading code aloud when teaching, and I want to
support that. I say code aloud sometimes too, especially in
conversations.

## Lisp and Nesting

Lisp is a big influence on Garden, and I enjoy using s-expression
syntax. Unfortunately it's outside my strangeness budget. I also think
the uniformity can make some code patterns harder to read, because
code with vastly different semantics can appear quite similar visually.

Parentheses-focused syntax has some interesting advantages though.

(1) S-expressions make macro systems easier. You have a natural
correspondence between surface syntax and the data structures that
macros see. Macros aren't a priority in Garden as they make developer
tooling harder, and tooling is my primary focus.

(2) An expression-oriented language with a simple nested syntax makes
it really easy to evaluate snippets. You can evaluate both definitions
and expressions in the interactive interpreter ("REPL").

You often see subexpressions that you can evaluate in
isolation too!

```lisp
(defun garden-send-input (string &optional path offset end-offset)
  "Send STRING to the current garden session for evaluation."
  (let ((buf (garden--active-buffer)))
    (garden--send-run (get-buffer-process buf) string path offset end-offset)))
```

I can run `(garden--active-buffer)` on its own, which is super
convenient. Unfortunately I can't do this for all expressions. There's
no easy way to see the output of `(get-buffer-process buf)` without
e.g. setting a breakpoint, because `buf` is bound outside the function
call.

I plan to support this 'evaluate this subexpression' feature in
Garden's syntax.

## Smalltalk

Smalltalk is another big influence on Garden. The Smalltalk syntax is
small, but it's not widely known. It would be costly to my strangeness
budget.

Smalltalk syntax also has a few interesting advantages.

(1) Block syntax works like a closure, but it doesn't support early
return. Instead, `^` is early return anywhere inside a method
definition, even inside blocks.

`^` and blocks are an elegant way to express control flow primitives.
However, if we're doing conventional loops anyway, there's not much benefit
for Garden.

(2) Method arguments are almost always keyword arguments, and often
resemble sentences. This is often very readable.

```smalltalk
10 to: 1 by: -1 do: [:x | x printNl ]
```

It's also explicit that the keywords are part of the API. I've worked
with languages (particularly Python) where you rename a parameter and
accidentally break your call sites, because they're using keyword
rather than positional arguments.

Garden currently uses positional arguments, as it was conventional and
simpler to implement. It's also less verbose, especially when
experimenting in the REPL.

## Unconventional Bits

I'm happy to break convention where it makes sense for Garden's
design. For example, methods are not defined nested inside the
relevant type.

```
struct Person {
  name: String,
}

method greet(this: Person): String {
  "Hello " ^ this.name ^ "!"
}
```

This aligns with Garden's goals of allowing incremental program
definition. You don't want to re-evaluate the whole type definition
when you're just changing a single method.

## Next Steps

Every example in the docs is type checked, so you can always read this
website to see the exact syntax supported.

I'm relatively happy with the overall syntax, but the real test will
be in the tooling I can build around it.
