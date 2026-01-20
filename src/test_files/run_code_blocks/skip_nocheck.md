# Skip Nocheck

Blocks with the nocheck attribute should be skipped.

```garden nocheck
this is invalid syntax and should be skipped
```

// args: run-code-blocks
// expected stderr: Checked 0 blocks in 1 file.

