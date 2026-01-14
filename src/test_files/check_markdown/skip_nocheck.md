# Skip Nocheck

Blocks with the nocheck attribute should be skipped.

```garden nocheck
this is invalid syntax and should be skipped
```

This block should be checked.

```garden
1 + 1 //-> 2
```

// args: check-markdown
// expected stdout: All checks passed in skip_nocheck.md (1 code blocks, 1 expressions)
