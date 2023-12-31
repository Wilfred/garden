use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use strum_macros::EnumIter;

use crate::ast::{FunInfo, Symbol, TypeName};
use crate::SymbolName;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// An integer value.
    Integer(i64),
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
    Enum(TypeName, usize, Option<Box<Value>>),
}

/// A helper for creating a unit value.
pub fn unit_value() -> Value {
    // We can assume that Unit is always defined because it's in the
    // prelude.
    Value::Enum(TypeName("Unit".to_owned()), 0, None)
}

pub fn bool_value(b: bool) -> Value {
    // We can assume that Bool is always defined because it's in the
    // prelude.
    Value::Enum(TypeName("Bool".to_owned()), if b { 0 } else { 1 }, None)
}

// TODO: Is it correct to define equality here? Closures should only
// have reference equality probably.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockBindings(pub Rc<RefCell<HashMap<SymbolName, Value>>>);

impl Default for BlockBindings {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(HashMap::new())))
    }
}

pub fn type_representation(value: &Value) -> TypeName {
    TypeName(
        match value {
            Value::Integer(_) => "Int",
            Value::Fun(_, _) => "Fun",
            Value::Closure(_, _) => "Fun",
            Value::BuiltinFunction(_) => "Fun",
            Value::String(_) => "String",
            Value::List(_) => "List",
            Value::Enum(name, _, _) => &name.0,
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
