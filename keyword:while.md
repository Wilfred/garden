# `while` keyword

`for` runs a block code repeatedly, while an expression evaluates to
`True`.

```title:Example
let nums = [1, 2, 3]

let i = 0
while i < nums.len() {
  let num = nums.get(i)
  dbg(num)
  
  i += 1
}
```

For looping when you have a list, see
[`for`](./keyword:for.html).
