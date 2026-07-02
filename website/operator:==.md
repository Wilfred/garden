# `==` and `!=` Operators

`==` compares two values, and returns `True` if they are equal.

```
1 + 1 == 2 //-> True
1 == 2 //-> False
```

Compound values are compared by their elements.

```
[1, 2] == [1, 1 + 1] //-> True
```

`!=` compares whether two values are not equal.

```
1 != 2 //-> True
```

`Float` values are compared by value, including positive and negative
zero.

```
1.5 == 1.5 //-> True
0.0 == -0.0 //-> True
```

Floats have limited precision, so arithmetic can produce results that
are close but not exactly equal. Comparing computed floats with `==`
is usually a mistake.

```
0.1 +. 0.2 == 0.3 //-> False
```
