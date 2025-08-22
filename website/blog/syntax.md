```metadata
published: true
date: 2025-08-23
title: Choosing a Syntax
```

Traditionally, a language announcement starts with a grandiose plan to
build something that everyone will use.

This isn't that.

Garden is a small programming language focused on tooling. It's a
playground for my ideas.

Instead of grand declarations, let's talk about something unavoidable:
basic syntax.

```
fun fib(i: Int): Int {
  if i < 2 {
    return 1
  }

  fib(i - 1) + fib(i - 2)
}
```

## Choosing Punctuation

Garden is a curly-brace language. I don't want to spend my
[strangeness
budget](https://steveklabnik.com/writing/the-language-strangeness-budget/)
on syntax, so it's deliberately conventional.

```
let i = 10
while i > 0 {
  println(string_repr(i))
  i -= 1
}
println("Blast off!")
```

Hopefully this code snippet is pretty readable to programmers from a
range of backgrounds.

## Choosing Keywords

I want keywords to be short. This follows [Stroustrup's
Rule](https://buttondown.com/hillelwayne/archive/stroustrups-rule/),
which favours short syntax for common operations. There's no real
advantage to writing `function` in JavaScript, it's just more typing
on the keyboard.

However, I want keywords to be pronounceable, hence `fun` instead of
`fn`. This is based on the Strange Loop talk [How to teach programming
(and other things)?](https://youtu.be/g1ib43q3uXQ?t=2165) by Felienne
Hermans (creator of [Hedy](https://hedy.org/)). Felienne talks about
the advantages of reading code aloud when teaching, and I want to
support that. I say code aloud sometimes too, especially in
conversations.

```
fun say_hello(): Unit {
  println("Hello, World!")
}
```

## Breaking Convention

Garden is focusing on tooling, so tooling features take precedence
over traditional syntax.

One of those features is incremental program definition. Garden
supports redefining individual methods (like Lisp or Smalltalk), which
doesn't suit a traditional class definition. Nesting all your methods
inside one big class definition doesn't make sense.

Instead, methods are defined at the same indentation level as
functions, similar to Go.

```
struct Person {
  name: String,
}

method greet(this: Person): String {
  "Hello " ^ this.name ^ "!"
}

fun demo(): Unit {
  let p = Person{ name: "Wilfred" }
  println(p.greet())
}
```

## Next Steps

With the basics out of the way, we can start talking about more
interesting things next time.

In the meantime, please explore the website and [let me know](https://www.wilfred.me.uk/contact/) if you
have any initial feedback.
