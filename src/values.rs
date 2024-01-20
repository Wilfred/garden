use std::fmt::Display;

use strum_macros::EnumIter;

use crate::env::Env;
use crate::eval::BlockBindings;
use crate::types::Type;
use garden_lang_parser::ast::{FunInfo, Symbol, TypeName};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Value {
    /// An integer value.
    Integer(i64),
    /// A reference to a user-defined function.
    Fun(Symbol, FunInfo),
    /// A closure value.
    Closure(Vec<BlockBindings>, FunInfo),
    /// A reference to a built-in function.
    BuiltinFunction(BuiltinFunctionKind, Option<FunInfo>),
    /// A string value.
    String(String),
    /// A list value.
    List(Vec<Value>),
    // A value in a user-defined enum.
    Enum(TypeName, usize, Option<Box<Value>>),
}

/// A helper for creating a unit value.
pub(crate) fn unit_value() -> Value {
    // We can assume that Unit is always defined because it's in the
    // prelude.
    Value::Enum(
        TypeName {
            name: "Unit".to_owned(),
        },
        0,
        None,
    )
}

pub(crate) fn bool_value(b: bool) -> Value {
    // We can assume that Bool is always defined because it's in the
    // prelude.
    Value::Enum(
        TypeName {
            name: "Bool".to_owned(),
        },
        if b { 0 } else { 1 },
        None,
    )
}

pub(crate) fn result_ok_value(v: Value) -> Value {
    // We can assume that Result is always defined because it's in the
    // prelude.
    Value::Enum(
        TypeName {
            name: "Result".to_owned(),
        },
        0,
        Some(Box::new(v)),
    )
}

pub(crate) fn result_err_value(v: Value) -> Value {
    // We can assume that Result is always defined because it's in the
    // prelude.
    Value::Enum(
        TypeName {
            name: "Result".to_owned(),
        },
        1,
        Some(Box::new(v)),
    )
}

pub(crate) struct RuntimeType {
    name: TypeName,
    args: Vec<RuntimeType>,
}

impl Display for RuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.name.name)
        } else {
            write!(
                f,
                "{}<{}>",
                self.name.name,
                self.args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

pub(crate) fn runtime_type(value: &Value) -> RuntimeType {
    match value {
        Value::Integer(_) => RuntimeType {
            name: TypeName {
                name: "Int".to_owned(),
            },
            args: vec![],
        },
        Value::Fun(_, _) | Value::Closure(_, _) | Value::BuiltinFunction(_, _) => RuntimeType {
            name: TypeName {
                name: "Fun".to_owned(),
            },
            // TODO: Fun should be a parameterized type.
            args: vec![],
        },
        Value::String(_) => RuntimeType {
            name: TypeName {
                name: "String".to_owned(),
            },
            args: vec![],
        },
        Value::List(_) => RuntimeType {
            name: TypeName {
                name: "List".to_owned(),
            },
            // TODO
            args: vec![runtime_type(&Value::Integer(0))],
        },
        Value::Enum(name, _, _) => RuntimeType {
            name: name.clone(),
            // TODO
            args: vec![],
        },
    }
}

pub(crate) fn type_representation(value: &Value) -> TypeName {
    TypeName {
        name: match value {
            Value::Integer(_) => "Int",
            Value::Fun(_, _) => "Fun",
            Value::Closure(_, _) => "Fun",
            Value::BuiltinFunction(_, _) => "Fun",
            Value::String(_) => "String",
            Value::List(_) => "List",
            Value::Enum(name, _, _) => &name.name,
        }
        .to_owned(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub(crate) enum BuiltinFunctionKind {
    DebugPrint,
    Error,
    ListDirectory,
    Shell,
    StringRepr,
    PathExists,
    Print,
    Println,
    ReadFile,
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
            BuiltinFunctionKind::ReadFile => "read_file",
            BuiltinFunctionKind::WorkingDirectory => "working_directory",
        };
        write!(f, "{}", name)
    }
}

impl Value {
    /// Pretty-print `self` in a human friendly way.
    pub(crate) fn display(&self, env: &Env) -> String {
        match self {
            Value::Integer(i) => format!("{}", i),
            Value::Fun(name_sym, _) => format!("(function: {})", name_sym.name),
            Value::Closure(..) => "(closure)".to_string(),
            Value::BuiltinFunction(kind, _) => format!("(function: {})", kind),
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
            Value::Enum(name, variant_idx, payload) => {
                let type_ = match env.types.get(name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{}__OLD_DEFINITION::{}", name, variant_idx);
                    }
                };

                let mut variant_takes_payload = false;
                let variant_name = match type_ {
                    Type::Builtin(_) => {
                        unreachable!("Enum type names should never map to built-in types")
                    }
                    Type::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => {
                            variant_takes_payload = variant_sym.has_payload;
                            format!("{}", variant_sym)
                        }
                        None => format!("{}::__OLD_VARIANT_{}", name, variant_idx),
                    },
                };

                match payload {
                    Some(value) => format!("{variant_name}({})", value.display(env)),
                    None => {
                        if variant_takes_payload {
                            format!("{variant_name} (constructor)")
                        } else {
                            variant_name
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn display_unless_unit(&self, env: &Env) -> Option<String> {
        match self {
            Value::Enum(name, variant_idx, _) if name.name == "Unit" && *variant_idx == 0 => None,
            _ => Some(self.display(env)),
        }
    }
}

/// Convert "foo" to "\"foo\"", a representation that we can print as
/// a valid Garden string literal.
pub(crate) fn escape_string_literal(s: &str) -> String {
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
