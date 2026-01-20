# Missing Exception

Test that we detect when an expression is marked as throwing an exception
but actually succeeds.

```garden
1 + 1 //-> *exception*
```

// Checked 1 block in 1 file.

// args: run-code-blocks
// expected exit status: 1
// expected stderr:
// src/test_files/run_code_blocks/missing_exception.md:7: Expected exception but expression succeeded
//   Got:      2
// 
// Checked 1 block in 1 file.

