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

When adding built-in functions, always update the CHANGELOG.md with a
concise summary.

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

# Writing Static Analysis Checks

Garden's static analysis checks live in `src/checks/` and use the
`Visitor` trait (`src/parser/visitor.rs`) to traverse the AST. The
Visitor handles recursion automatically — you only override the
methods for the AST nodes you care about.

## Adding a New Check

1. Create a new file in `src/checks/` with a struct that holds a
   `Vec<Diagnostic>` and any state you need.
2. Implement `Visitor` for your struct, overriding only the relevant
   `visit_*` methods.
3. Add a public entry point function that creates the visitor, walks
   all toplevel items, and returns the diagnostics.
4. Register the check in `src/checks.rs` by calling your function
   from `check_toplevel_items_in_env()`.

## Example: A Minimal Check

```rust
use crate::diagnostics::{Diagnostic, Severity};
use crate::parser::ast::ToplevelItem;
use crate::parser::diagnostics::ErrorMessage;
use crate::parser::visitor::Visitor;
use crate::{msgcode, msgtext};

struct MyVisitor {
    diagnostics: Vec<Diagnostic>,
}

impl Visitor for MyVisitor {
    fn visit_expr(&mut self, expr: &Expression) {
        // Your logic here: inspect expr, push to self.diagnostics

        // Call the default to continue recursing into child nodes.
        self.visit_expr_(&expr.expr_);
    }
}

pub(crate) fn check_my_thing(items: &[ToplevelItem]) -> Vec<Diagnostic> {
    let mut visitor = MyVisitor {
        diagnostics: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }
    visitor.diagnostics
}
```

## Key Visitor Methods to Override

- `visit_toplevel_item` — top-level definitions (functions, methods,
  tests, enums, structs, imports)
- `visit_fun_info` / `visit_method_info` — function and method bodies
- `visit_expr` — all expressions (call `visit_expr_` at the end to
  keep recursing)
- `visit_expr_while` / `visit_expr_for_in` — loop constructs
- `visit_expr_let` / `visit_expr_assign` — variable bindings and
  assignments
- `visit_expr_variable` — variable references
- `visit_symbol` / `visit_type_symbol` — identifiers and type names
- `visit_type_hint` — type annotations
- `visit_block` — blocks of expressions

Each `visit_*` method has a `_default` variant (e.g.
`visit_fun_info_default`) that performs the standard recursive
traversal. Call the `_default` method when you want to add logic
before or after the default recursion:

```rust
fn visit_fun_info(&mut self, fun_info: &FunInfo) {
    // Custom logic before recursion.
    self.enter_scope();

    // Use the _default method to recurse as normal.
    self.visit_fun_info_default(fun_info);

    // Custom logic after recursion.
    self.leave_scope();
}
```

See `src/checks/loops.rs` for a concise real example.

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
- ./target/debug/garden run -c 'println("Hello")': Run a code snippet directly without a file.
- ./target/debug/garden test yourfile.gdn: Run unit tests in yourfile.gdn.

To generate target/debug/garden use `cargo build`. This separation
allows Claude to set permissions on separate Garden subcommands.

# Site Builder

The site builder lives at `website/build_site.gdn`.
