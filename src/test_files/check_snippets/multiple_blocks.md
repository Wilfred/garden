# Multiple blocks

A clean block:

```garden
1 + 1
```

A bad block:

```garden
println(missing)
```

Another bad block:

```garden
let x = unknown
```

// args: check-snippets
// expected exit status: 1
// expected stdout:
// Error: Unbound symbol: `missing`
// --| src/test_files/check_snippets/multiple_blocks.md:12:9
// 11| ```garden
// 12| println(missing)
// 13| ```     ^^^^^^^
// 
// Error: Unbound symbol: `unknown`
// --| src/test_files/check_snippets/multiple_blocks.md:18:9
// 17| ```garden
// 18| let x = unknown
// 19| ```     ^^^^^^^

