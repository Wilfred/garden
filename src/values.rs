use std::fmt::Display;

use strum_macros::EnumIter;

use crate::env::Env;
use crate::eval::BlockBindings;
use crate::garden_type::Type;
use crate::garden_type::TypeDefKind;
use crate::types::TypeDef;
use garden_lang_parser::ast::{FunInfo, Symbol, SymbolName, TypeName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    /// An integer value.
    Integer(i64),
    /// A reference to a user-defined function, along with its return
    /// type.
    Fun { name_sym: Symbol, fun_info: FunInfo },
    /// A closure value.
    Closure(Vec<BlockBindings>, FunInfo),
    /// A reference to a built-in function.
    BuiltinFunction(BuiltinFunctionKind, Option<FunInfo>),
    /// A string value.
    String(String),
    /// A list value, along with the type of its elements.
    List { items: Vec<Value>, elem_type: Type },
    /// A tuple value, along with type of each item.
    Tuple {
        items: Vec<Value>,
        item_types: Vec<Type>,
    },
    /// A value in a user-defined enum, such as `True` or `Some(123)`.
    EnumVariant {
        type_name: TypeName,
        runtime_type: Type,
        variant_idx: usize,
        payload: Option<Box<Value>>,
    },
    // A function that takes one argument, the payload of this enum
    // variant, and returns an enum value.
    EnumConstructor {
        type_name: TypeName,
        runtime_type: Type,
        variant_idx: usize,
    },
    /// A value with the type of a user-defined struct. Fields are
    /// ordered according to the definition of the type.
    Struct {
        type_name: TypeName,
        fields: Vec<(SymbolName, Value)>,
        runtime_type: Type,
    },
}

impl Value {
    /// A helper for creating a unit value.
    pub(crate) fn unit() -> Self {
        // We can assume that Unit is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Unit".to_owned(),
            },
            runtime_type: Type::unit(),
            variant_idx: 0,
            payload: None,
        }
    }

    pub(crate) fn bool(b: bool) -> Self {
        // We can assume that Bool is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Bool".to_owned(),
            },
            runtime_type: Type::bool(),
            variant_idx: if b { 0 } else { 1 },
            payload: None,
        }
    }

    pub(crate) fn as_rust_bool(&self) -> Option<bool> {
        match self {
            Value::EnumVariant {
                runtime_type,
                variant_idx,
                ..
            } if runtime_type == &Type::bool() => Some(*variant_idx == 0),
            _ => None,
        }
    }

    pub(crate) fn some(v: Value, env: &Env) -> Self {
        let value_type = Type::from_value(&v, &env.types, &env.stack.type_bindings());

        // We can assume that Option is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Option".to_owned(),
            },
            variant_idx: 0,
            payload: Some(Box::new(v)),
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    name: "Option".to_owned(),
                },
                args: vec![value_type],
            },
        }
    }

    pub(crate) fn none(value_ty: Type) -> Self {
        // We can assume that Option is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Option".to_owned(),
            },
            variant_idx: 1,
            payload: None,
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    name: "Option".to_owned(),
                },
                args: vec![value_ty],
            },
        }
    }

    pub(crate) fn ok(v: Value, env: &Env) -> Self {
        let value_type = Type::from_value(&v, &env.types, &env.stack.type_bindings());

        // We can assume that Result is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Result".to_owned(),
            },
            variant_idx: 0,
            payload: Some(Box::new(v)),
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    name: "Result".to_owned(),
                },
                args: vec![value_type, Type::no_value()],
            },
        }
    }

    pub(crate) fn err(v: Value, env: &Env) -> Self {
        let value_type = Type::from_value(&v, &env.types, &env.stack.type_bindings());

        // We can assume that Result is always defined because it's in the
        // prelude.
        Value::EnumVariant {
            type_name: TypeName {
                name: "Result".to_owned(),
            },
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    name: "Result".to_owned(),
                },
                args: vec![Type::no_value(), value_type],
            },
            variant_idx: 1,
            payload: Some(Box::new(v)),
        }
    }

    pub(crate) fn path(inner: String) -> Self {
        Value::Struct {
            type_name: TypeName {
                name: "Path".to_owned(),
            },
            fields: vec![(SymbolName::from("p"), Value::String(inner))],
            runtime_type: Type::path(),
        }
    }
}

pub(crate) fn type_representation(value: &Value) -> TypeName {
    TypeName {
        name: match value {
            Value::Integer(_) => "Int",
            Value::Fun { .. } => "Fun",
            Value::Closure(_, _) => "Fun",
            Value::BuiltinFunction(_, _) => "Fun",
            Value::String(_) => "String",
            Value::List { .. } => "List",
            Value::Tuple { .. } => "Tuple",
            Value::EnumVariant { type_name, .. } | Value::EnumConstructor { type_name, .. } => {
                &type_name.name
            }
            Value::Struct { type_name, .. } => &type_name.name,
        }
        .to_owned(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub(crate) enum BuiltinFunctionKind {
    Assert,
    Error,
    ListDirectory,
    Shell,
    StringRepr,
    Print,
    Println,
    SourceDirectory,
    WorkingDirectory,
    WriteFile,
}

impl Display for BuiltinFunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BuiltinFunctionKind::Assert => "assert",
            BuiltinFunctionKind::Error => "error",
            BuiltinFunctionKind::ListDirectory => "list_directory",
            BuiltinFunctionKind::Shell => "shell",
            BuiltinFunctionKind::StringRepr => "string_repr",
            BuiltinFunctionKind::Print => "print",
            BuiltinFunctionKind::Println => "println",
            BuiltinFunctionKind::SourceDirectory => "source_directory",
            BuiltinFunctionKind::WorkingDirectory => "working_directory",
            BuiltinFunctionKind::WriteFile => "write_file",
        };
        write!(f, "{}", name)
    }
}

impl Value {
    /// Pretty-print `self` in a human friendly way.
    pub(crate) fn display(&self, env: &Env) -> String {
        match self {
            Value::Integer(i) => format!("{}", i),
            Value::Fun { name_sym, .. } => format!("(function: {})", name_sym.name),
            Value::Closure(..) => "(closure)".to_owned(),
            Value::BuiltinFunction(kind, _) => format!("(function: {})", kind),
            Value::String(s) => escape_string_literal(s),
            Value::List { items, .. } => {
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
            Value::Tuple { items, .. } => {
                let mut s = String::new();

                s.push('(');

                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&item.display(env));
                }
                if items.len() == 1 {
                    s.push(',');
                }

                s.push(')');

                s
            }
            Value::EnumVariant {
                type_name,
                variant_idx,
                payload,
                ..
            } => {
                let type_ = match env.get_type_def(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{}__OLD_DEFINITION::{}", type_name, variant_idx);
                    }
                };

                let variant_name = match type_ {
                    TypeDef::Builtin(_, _) => {
                        unreachable!("Enum type names should never map to built-in types")
                    }
                    TypeDef::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => {
                            format!("{}", variant_sym.name_sym.name)
                        }
                        None => format!("{}::__OLD_VARIANT_{}", type_name, variant_idx),
                    },
                    TypeDef::Struct(struct_info) => {
                        format!("{}__OLD_DEFINITION", struct_info.name_sym)
                    }
                };

                match payload {
                    Some(value) => format!("{variant_name}({})", value.display(env)),
                    None => variant_name,
                }
            }
            Value::EnumConstructor {
                type_name,
                variant_idx,
                ..
            } => {
                let type_ = match env.get_type_def(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!(
                            "{}__OLD_DEFINITION::{} (constructor)",
                            type_name, variant_idx
                        );
                    }
                };

                match type_ {
                    TypeDef::Builtin(_, _) => {
                        unreachable!("Enum type names should never map to built-in types")
                    }
                    TypeDef::Enum(enum_info) => match enum_info.variants.get(*variant_idx) {
                        Some(variant_sym) => {
                            format!("{} (constructor)", variant_sym.name_sym.name)
                        }
                        None => {
                            format!("{}::__OLD_VARIANT_{} (constructor)", type_name, variant_idx)
                        }
                    },
                    TypeDef::Struct(struct_info) => {
                        format!("{}__OLD_DEFINITION (constructor)", struct_info.name_sym)
                    }
                }
            }
            Value::Struct {
                type_name, fields, ..
            } => {
                let mut s = format!("{type_name} {{ ");

                for (i, (field_name, value)) in fields.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&format!("{}: {}", field_name, value.display(env)));
                }

                s.push_str(" }");

                s
            }
        }
    }

    pub(crate) fn display_unless_unit(&self, env: &Env) -> Option<String> {
        match self {
            Value::EnumVariant {
                type_name,
                variant_idx,
                ..
            } if type_name.name == "Unit" && *variant_idx == 0 => None,
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
    use garden_lang_parser::ast::SyntaxIdGenerator;

    use super::*;

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let mut id_gen = SyntaxIdGenerator::default();
        let env = Env::new(&mut id_gen);

        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(value.display(&env), "\"foo \\\\ \\\" \\n bar\"");
    }
}
