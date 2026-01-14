# Failed Check

This assertion should fail.

```garden
1 + 1 //-> 3
```

// args: check-markdown
// expected exit status: 1
// expected stderr:
// /home/claudebox/garden/src/test_files/check_markdown/failed_check.md:1: Check failed
//   Expected: 3
//   Got:      2
// 
// 1 checks failed, 0 passed

