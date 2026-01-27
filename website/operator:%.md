# `%` Operator

`%` calculates the remainder when dividing the first number by the
second number.

```
6 % 3 //-> 0
7 % 3 //-> 1
```

As with division, `% 0` will throw an exception.

```
6 % 0 //-> *exception*
```

`%` also works with negative numbers, using the Euclidean
remainder.

When the first operand is negative, the result is the difference from
the next smallest multiple.

```
// From -4, the next smallest multiple of 3
// is -6, and -4 is 2 greater than -6.
-4 % 3 //-> 2
```
