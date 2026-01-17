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

// args: run-code-blocks
