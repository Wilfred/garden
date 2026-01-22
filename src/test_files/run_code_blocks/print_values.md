# Print Values

Expressions without `//->` should be evaluated silently.

```garden
"hello world"
dbg("goodbye world")
```

This makes print debugging harder, but avoids noise in dbg() examples
in blocks.

// args: run-code-blocks
// expected stderr: Checked 1 block in 1 file.

