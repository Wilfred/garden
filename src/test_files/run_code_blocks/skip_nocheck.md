# Skip Nocheck

Blocks with the nocheck attribute should be skipped.

```garden nocheck
this is invalid syntax and should be skipped
```

// expected exit status: 2
// expected stderr:
// error: unrecognized subcommand 'src/test_files/run_code_blocks/skip_nocheck.md'
// 
// Usage: garden <COMMAND>
// 
// For more information, try '--help'.

