# `test` keyword

`test` defines a named test.

```title:example
test arithmetic {
  assert((1 + 2) == 3)
}
```

To run all the tests in a file, run the `garden` interpreter with the
`test` command.

```text
$ garden test my_file.gdn
```
