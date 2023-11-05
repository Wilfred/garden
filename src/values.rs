use std::fmt::Display;

use strum_macros::EnumIter;

use crate::ast::{EnumInfo, FunInfo, Symbol, TypeName};
use crate::env::Env;
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
    // A value in a user-defined enum.
    Enum(TypeName, usize),
}

/// A helper for creating a unit value.
pub fn unit_value() -> Value {
    // We can assume that Unit is always defined because it's in the
    // prelude.
    Value::Enum(TypeName("Unit".to_owned()), 0)
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

impl Value {
    /// Pretty-print `self` in a human friendly way.
    pub fn display(&self, env: &Env) -> String {
        match self {
            Value::Integer(i) => format!("{}", i),
            Value::Boolean(b) => format!("{}", b),
            Value::Fun(name, _) => format!("(function: {})", name.name),
            Value::Closure(..) => "(closure)".to_string(),
            Value::BuiltinFunction(kind) => format!("(function: {})", kind),
            Value::String(s) => escape_string_literal(s),
            Value::List(items) => {
                let mut s = String::new();

                s.push('[');

                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&item.display(env));
                }

                s.push(']');

                s
            }
            Value::Enum(name, variant_idx) => match env.types.get(name) {
                Some(type_) => match type_ {
                    Type::Builtin(_) => unreachable!(),
                    Type::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => format!("{}", variant_sym.name),
                        None => format!("{}::__OLD_VARIANT_{}", name, variant_idx),
                    },
                },
                None => format!("{}__OLD_DEFINITION::{}", name, variant_idx),
            },
        }
    }

    pub fn display_unless_unit(&self, env: &Env) -> Option<String> {
        match self {
            Value::Enum(name, variant_idx) if name.0 == "Unit" && *variant_idx == 0 => None,
            _ => Some(self.display(env)),
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
        let env = Env::default();
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(value.display(&env), "\"foo \\\\ \\\" \\n bar\"");
    }
}
