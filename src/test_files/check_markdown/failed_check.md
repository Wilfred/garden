# Failed Check

This assertion should fail.

```garden
1 + 1 //-> 3
```

// args: check-markdown
// expected exit status: 1
// expected stderr:
// src/test_files/check_markdown/failed_check.md:6: Check failed
//   Expected: 3
//   Got:      2

