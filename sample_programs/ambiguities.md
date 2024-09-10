Garden now requires return to always have a value to avoid ambiguity
with expresssions immediately after.

```
return 1

return 
1
```

There is still an ambiguity with struct literals and blocks, however.

```
// struct literal
b {}

// block
{}

// ambiguous
let a = b
{}
```

There is also an ambiguity with tuples/parentheses and function calls.

```
foo()

(1, 2)


let x = foo
(1, 2)
```

Proposal: consider EOL, EOF or } to be an expression terminator.
Or require types to begin with a capital letter?
