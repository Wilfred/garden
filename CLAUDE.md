Garden is a programming language with a focus on novel developer
tooling and very high quality documentation.

Hello World in Garden looks like this:

```
println("Hello World")
```

A function with a unit test looks like this:

```
fun add_one(i: Int): Int {
  i + 1
}

test add_one_to_two {
  assert(add_one(2) == 3)
}
```

The built-in namespaces available are `__fs`, `__random` and
`__reflect`. You can import and use them like this:

```
import "__fs.gdn" as fs

fs::list_directory(Path{ p: "/"})
```

# Verifying changes
- cargo run -- check yourfile.gdn: Check the test program named yourfile.gdn
- cargo run -- run yourfile.gdn: Run the code in yourfile.gdn.
- cargo run -- test yourfile.gdn: Run unit tests in yourfile.gdn.

Remember that Garden does not treat `main` functions specially. You'll
need to write a toplevel call to any functions you want to call at
runtime, e.g. `main()`.

You do not need to ask permission for check commands.

# Site Builder

The site builder lives at `website/build_site.gdn`.
