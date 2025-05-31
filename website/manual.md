# Manual

## Keywords

__KEYWORDS

## Built-in Types

__BUILTIN_TYPES

## Prelude

These functions are available in all Garden code.

__PRELUDE_FUNS

## Filesystem Namespace `__fs.gdn`

__FS_FUNS

Example usage:

```
import "__fs.gdn" as fs
fs::list_directory(Path{ p: "/" })
```

## Garden Reflection Namespace `__garden.gdn`

__GARDEN_FUNS

Example usage:

```
import "__garden.gdn" as garden
garden::check_snippet("let x = 1")
```

