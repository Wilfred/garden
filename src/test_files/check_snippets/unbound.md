# Unbound symbol

```garden
println(undefined_var)
```

// args: check-snippets
// expected exit status: 1
// expected stdout:
// Error: Unbound symbol: `undefined_var`
// --| src/test_files/check_snippets/unbound.md:4:9
//  3| ```garden
//  4| println(undefined_var)
//  5| ```     ^^^^^^^^^^^^^

