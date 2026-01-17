# Skip Nocheck

Blocks with the nocheck attribute should be skipped.

```garden nocheck
this is invalid syntax and should be skipped
```

This block should be checked.

```garden
1 + 1 //-> 2
```

// args: run-code-blocks
