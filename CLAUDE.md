Garden is a programming language with a focus on developer tooling and
high quality documentation.

Hello World in Garden looks like this:

```
println("Hello World")
```

Remember that Garden does not treat `main` functions specially. You
can just write expressions in the file directly, such as
`println("foo")` or `some_func()`.

A function with a unit test looks like this:

```
fun add_one(i: Int): Int {
  i + 1
}

test add_one_to_two {
  assert(add_one(2) == 3)
}
```

The built-in files available are `__fs`, `__random` and
`__reflect`. You can import and use them like this:

```
import "__fs.gdn" as fs

fs::list_directory(Path{ p: "/"})
```

When adding built-in functions, always update the CHANGELOG.md.

# Writing Error Messages

Error messages in Garden are extensively marked up so they can be
highlighted in the terminal output. An error message is composed of
`MessagePart` elements, which can be either `Text` or `Code`:

- `Text` parts are rendered as plain text
- `Code` parts are highlighted (wrapped in backticks in plain text,
  or syntax highlighted in color output)

Use the helper macros to construct error messages:

- `msgtext!("Your message here")` - Creates a Text part
- `msgcode!("{}", variable)` - Creates a Code part

Example error message:

```rust
use crate::{msgcode, msgtext};

ErrorMessage(vec![
    msgtext!("Unknown built-in file "),
    msgcode!("{}", path.display()),
    msgtext!(". Available files are: __fs, __random, __reflect."),
])
```

This markup ensures that code elements like variable names, types,
and paths stand out clearly in error messages.

# Checking Changes
- cargo clippy: Check that your Rust code is correct

# Running Tests
Garden uses golden tests in `src/test_files/` that include a sample
program and the expected output. New features or bugfixes should
generally include a new test file.

- cargo test golden: Run all the golden tests
- REGENERATE=y cargo test golden: Update the golden test output

# Running Garden Programs
- ./target/debug/garden check yourfile.gdn: Check the test program named yourfile.gdn
- ./target/debug/garden run yourfile.gdn: Run the code in yourfile.gdn.
- ./target/debug/garden test yourfile.gdn: Run unit tests in yourfile.gdn.

To generate target/debug/garden use `cargo build`. This separation
allows Claude to set permissions on separate Garden subcommands.

# Site Builder

The site builder lives at `website/build_site.gdn`.
