# Exception Test

Test that expected exceptions are handled correctly.

```garden
fun divide(a: Int, b: Int): Int {
  a / b
}

divide(10, 0) //-> *exception*
```

Mixed success and exception:

```garden
1 + 1 //-> 2

fun divide3(a: Int, b: Int): Int {
  a / b
}

divide3(8, 0) //-> *exception*

3 + 3 //-> 6
```

// args: run-code-blocks
// expected stderr: Checked 2 blocks in 1 file.

