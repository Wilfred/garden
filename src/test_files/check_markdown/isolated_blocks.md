# Isolated Blocks

Each code block should start with a fresh environment.

```garden
let x = 10
x //-> 10
```

This block should not have access to x from the previous block.

```garden
let y = 20
y //-> 20
```

// args: check-markdown
// expected stdout:
// Unit
// Unit
// All checks passed in isolated_blocks.md (2 code blocks, 4 expressions)

