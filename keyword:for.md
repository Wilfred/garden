# `for` keyword

`for` runs a block code repeatedly, once for each occurrence in a list.

```title:example
let nums = [1, 2, 3]

for num in nums {
  dbg(num)
}
```

To stop loop evaluation, or to skip a loop iteration, see
[`break`](./keyword:break.html) and
[`continue`](./keyword:continue.html).

As with [`let`](./keyword:let.html), you can use destructuring in `for`
loop headers.

```title:example
let game_scores = [(123, "Alice"), (45, "Bob")]

for (score, name) in game_scores {
  dbg(score)
  dbg(name)
}
```

For looping when you don't have a list, see
[while](./keyword:while.html).
