# `for` keyword

`for` runs a block code repeatedly, once for each occurrence in a list.

```
let nums = [1, 2, 3]

for num in nums {
  dbg(num)
}
```

The keywords `break` and `continue` are also available in loops.

As with [`let`](./keyword:let.html), you can use destructuring in `for`
loop headers.

```
let game_scores = [(123, "Alice"), (45, "Bob")]

for (score, name) in game_scores {
  dbg(score)
  dbg(name)
}
```
For looping when you don't have a list, see
[while](./keyword:while.html).
