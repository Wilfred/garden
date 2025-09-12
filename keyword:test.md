# `test` keyword

`test` defines a named test.

```title:Example
test arithmetic {
  assert((1 + 2) == 3)
}
```

To run all the tests in a file, run the `garden` interpreter with the
`test` command.

```text title:"Running tests"
$ garden test my_file.gdn
```
