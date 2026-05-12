# Warning

```garden
fun foo(x: Int): Int {
  let unused = 99
  x
}
foo(1)
```

// args: check-snippets
// expected exit status: 1
// expected stdout:
// Warning: `unused` is unused.
// --| src/test_files/check_snippets/warning.md:5:7
//  3| ```garden
//  4| fun foo(x: Int): Int {
//  5|   let unused = 99
//  6|   x   ^^^^^^
//  7| }

