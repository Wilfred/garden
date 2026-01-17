# Exception Test

Test that expected exceptions are handled correctly.

```garden
fun divide(a: Int, b: Int): Int {
  a / b
}

divide(10, 0) //-> *exception*
```

Multiple blocks with exceptions:

```garden
[1, 2, 3][10] //-> *exception*
```

Mixed success and exception:

```garden
1 + 1 //-> 2
[][0] //-> *exception*
3 + 3 //-> 6
```

// args: run-code-blocks
