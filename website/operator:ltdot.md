# `<.`, `<=.`, `>.` and `>=.` Operators

These operators compare `Float` values. For `Int` values, see `<`.

`<.` evaluates to `True` if the first float is strictly less than the
second.

```
1.0 <. 2.5 //-> True
1.5 <. 1.5 //-> False
```

`<=.` evaluates to `True` if the first float is less than or equal to
the second.

```
1.0 <=. 2.5 //-> True
1.5 <=. 1.5 //-> True
```

`>.` evaluates to `True` if the first float is strictly greater than
the second.

```
2.5 >. 1.0 //-> True
1.5 >. 1.5 //-> False
```

`>=.` evaluates to `True` if the first float is greater than or equal
to the second.

```
2.5 >=. 1.0 //-> True
1.5 >=. 1.5 //-> True
```
