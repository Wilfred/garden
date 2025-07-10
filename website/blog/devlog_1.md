# Devlog 1: Arguments to `main()`

Some languages pass arguments to `main`, such as Java and C.

```java
public class Demo {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

```c
#include <stdio.h>

int main(int argc, char **argv) {
  printf("Hello, World!\n");
  return 0;
}
```

Garden was previously like this, but I'm switching to a syntax where
`main` never takes arguments. This has several advantages:

(1) Small programs are much less verbose, making experimenting easier.

(2) It makes programs more consistent. Cleaning up the sample code
made me realise how little consistency I had.

```
fun main(_) {}
fun main(_: List<String>) {}
fun main(_args: List<String>) {}
```

(3) Most programs don't want to use CLI arguments anyway.
