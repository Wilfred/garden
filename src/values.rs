use std::fmt::Display;

use strum_macros::EnumIter;

use crate::ast::{EnumInfo, FunInfo, Symbol, TypeName};
use crate::eval::BlockBindings;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// An integer value.
    Integer(i64),
    /// A boolean value.
    Boolean(bool),
    /// A reference to a user-defined function.
    Fun(Symbol, FunInfo),
    /// A closure value.
    Closure(Vec<BlockBindings>, FunInfo),
    /// A reference to a built-in function.
    BuiltinFunction(BuiltinFunctionKind),
    /// A string value.
    String(String),
    /// A list value.
    List(Vec<Value>),
    /// The void/unit value.
    Void,
    // A value in a user-defined enum.
    Enum(TypeName, usize),
}

pub fn type_representation(value: &Value) -> TypeName {
    TypeName(
        match value {
            Value::Integer(_) => "Int",
            Value::Boolean(_) => "Bool",
            Value::Fun(_, _) => "Fun",
            Value::Closure(_, _) => "Fun",
            Value::BuiltinFunction(_) => "Fun",
            Value::String(_) => "String",
            Value::List(_) => "List",
            Value::Void => "Void",
            Value::Enum(name, _) => &name.0,
        }
        .to_owned(),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub enum BuiltinFunctionKind {
    DebugPrint,
    Error,
    ListDirectory,
    Shell,
    StringRepr,
    PathExists,
    Print,
    Println,
    WorkingDirectory,
}

impl Display for BuiltinFunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BuiltinFunctionKind::DebugPrint => "dbg",
            BuiltinFunctionKind::Error => "error",
            BuiltinFunctionKind::ListDirectory => "list_directory",
            BuiltinFunctionKind::Shell => "shell",
            BuiltinFunctionKind::StringRepr => "string_repr",
            BuiltinFunctionKind::PathExists => "path_exists",
            BuiltinFunctionKind::Print => "print",
            BuiltinFunctionKind::Println => "println",
            BuiltinFunctionKind::WorkingDirectory => "working_directory",
        };
        write!(f, "{}", name)
    }
}

pub fn builtin_fun_doc(kind: &BuiltinFunctionKind) -> &str {
    match kind {
        BuiltinFunctionKind::DebugPrint => {
            "Write an arbitrary value to stdout, along with debugging metadata.

```
dbg([1, 2]);
```"
        }
        BuiltinFunctionKind::Error => {
            "Stop the program immediately, and report the error message given.

```
error(\"Computer is melting!\");
```"
        }
        BuiltinFunctionKind::ListDirectory => {
            "List the contents of the specified directory.

```
list_directory(\"/\");
```"
        }
        BuiltinFunctionKind::Shell =>{
            "Execute the given string as a shell command, and return stdout concatenated with stderr.

```
shell(\"ls\", [\"-l\", \"/\"]);
```"
        }
        BuiltinFunctionKind::StringRepr => {
            "Pretty print a value.

```
string_repr(123); // \"123\"
```"
        }
        BuiltinFunctionKind::PathExists =>{
            "Return true if this path exists.
Note that a path may exist without the current user having permission to read it.

```
path_exists(\"/\"); // true
```"
        }
        BuiltinFunctionKind::Print => {
            "Write a string to stdout.

```
print(\"hello world\n\");
```"
        }
        BuiltinFunctionKind::Println => {
            "Write a string to stdout, with a newline appended.

```
print(\"hello world\");
```"
        }
        BuiltinFunctionKind::WorkingDirectory => {
            "Return the path of the current working directory.

```
working_directory(); // \"/home/yourname/awesome_garden_project\"
```"
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(name, _) => write!(f, "(function: {})", name.name.0),
            Value::Closure(..) => write!(f, "(closure)"),
            Value::BuiltinFunction(kind) => write!(f, "(function: {})", kind),
            Value::Void => write!(f, "void"),
            Value::String(s) => {
                write!(f, "{}", escape_string_literal(s))
            }
            Value::List(items) => {
                write!(f, "[")?;

                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", item)?;
                }

                write!(f, "]")
            }
            Value::Enum(name, variant_idx) => {
                write!(f, "{}::{}", name.0, variant_idx)
            },
        }
    }
}

/// Convert "foo" to "\"foo\"", a representation that we can print as
/// a valid Garden string literal.
pub fn escape_string_literal(s: &str) -> String {
    let mut res = String::new();
    res.push('"');

    // Escape inner double quotes and backslashes.
    for c in s.chars() {
        match c {
            '"' => res.push_str("\\\""),
            '\n' => res.push_str("\\n"),
            '\\' => res.push_str("\\\\"),
            _ => res.push(c),
        }
    }

    res.push('"');
    res
}

#[derive(Debug)]
pub enum BuiltinType {
    Int,
    Bool,
    String,
    Void,
    // TODO: these require a type parameter.
    Fun,
    List,
}

#[derive(Debug)]
pub enum Type {
    Builtin(BuiltinType),
    Enum(EnumInfo),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(format!("{}", value), "\"foo \\\\ \\\" \\n bar\"");
    }
}
