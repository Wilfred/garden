use std::fmt::Display;

use itertools::Itertools as _;
use strum_macros::EnumIter;

use crate::env::Env;
use crate::eval::BlockBindings;
use crate::types::TypeDef;
use garden_lang_parser::ast::{FunInfo, Symbol, SymbolName, TypeHint, TypeName};

#[derive(Debug, Clone, PartialEq)]
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
    List {
        items: Vec<Value>,
        elem_type: RuntimeType,
    },
    /// A value in a user-defined enum.
    Enum {
        type_name: TypeName,
        runtime_type: RuntimeType,
        variant_idx: usize,
        payload: Option<Box<Value>>,
    },
    // A function that takes one argument, the payload of this enum
    // variant, and returns an enum value.
    EnumConstructor {
        type_name: TypeName,
        variant_idx: usize,
    },
    /// A value with the type of a user-defined struct. Fields are
    /// ordered according to the definition of the type.
    Struct {
        type_name: TypeName,
        fields: Vec<(SymbolName, Value)>,
        runtime_type: RuntimeType,
    },
}

/// A helper for creating a unit value.
pub(crate) fn unit_value() -> Value {
    // We can assume that Unit is always defined because it's in the
    // prelude.
    Value::Enum {
        type_name: TypeName {
            name: "Unit".to_owned(),
        },
        runtime_type: RuntimeType::unit(),
        variant_idx: 0,
        payload: None,
    }
}

pub(crate) fn bool_value(b: bool) -> Value {
    // We can assume that Bool is always defined because it's in the
    // prelude.
    Value::Enum {
        type_name: TypeName {
            name: "Bool".to_owned(),
        },
        runtime_type: RuntimeType::bool(),
        variant_idx: if b { 0 } else { 1 },
        payload: None,
    }
}

pub(crate) fn result_ok_value(v: Value) -> Value {
    let value_type = RuntimeType::from_value(&v);

    // We can assume that Result is always defined because it's in the
    // prelude.
    Value::Enum {
        type_name: TypeName {
            name: "Result".to_owned(),
        },
        variant_idx: 0,
        payload: Some(Box::new(v)),
        runtime_type: RuntimeType::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                name: "Result".to_owned(),
            },
            args: vec![value_type, RuntimeType::NoValue],
        },
    }
}

pub(crate) fn result_err_value(v: Value) -> Value {
    let value_type = RuntimeType::from_value(&v);

    // We can assume that Result is always defined because it's in the
    // prelude.
    Value::Enum {
        type_name: TypeName {
            name: "Result".to_owned(),
        },
        runtime_type: RuntimeType::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                name: "Result".to_owned(),
            },
            args: vec![RuntimeType::NoValue, value_type],
        },
        variant_idx: 1,
        payload: Some(Box::new(v)),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeDefKind {
    Enum,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RuntimeType {
    /// The bottom type, no runtime values can have this type.
    NoValue,
    /// The top type, which includes all values.
    Top,
    String,
    Int,
    List(Box<RuntimeType>),
    Fun {
        params: Vec<RuntimeType>,
        return_: Box<RuntimeType>,
    },
    UserDefined {
        kind: TypeDefKind,
        name: TypeName,
        args: Vec<RuntimeType>,
    },
}

impl RuntimeType {
    pub(crate) fn unit() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                name: "Unit".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn bool() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                name: "Bool".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn empty_list() -> Self {
        Self::List(Box::new(Self::NoValue))
    }

    pub(crate) fn string_list() -> Self {
        Self::List(Box::new(Self::String))
    }

    pub(crate) fn from_hint(hint: &TypeHint) -> Self {
        let name = &hint.sym.name;
        if name.name == "NoValue" {
            return RuntimeType::NoValue;
        }
        if name.name == "String" {
            return RuntimeType::String;
        }
        if name.name == "Int" {
            return RuntimeType::Int;
        }
        if name.name == "List" {
            let elem_type = match hint.args.first() {
                Some(arg) => RuntimeType::from_hint(arg),
                None => RuntimeType::Top,
            };

            return RuntimeType::List(Box::new(elem_type));
        }

        let args: Vec<_> = hint.args.iter().map(RuntimeType::from_hint).collect();

        RuntimeType::UserDefined {
            // TODO: Look up this named type in the env.
            kind: TypeDefKind::Enum,
            name: name.clone(),
            args,
        }
    }

    pub(crate) fn from_value(value: &Value) -> Self {
        match value {
            Value::Integer(_) => RuntimeType::Int,
            Value::Fun { fun_info, .. } | Value::Closure(_, fun_info) => {
                Self::from_fun_info(fun_info)
            }
            Value::BuiltinFunction(_, fun_info) => match fun_info {
                Some(fun_info) => Self::from_fun_info(fun_info),
                None => RuntimeType::Top,
            },
            Value::String(_) => RuntimeType::String,
            Value::List { elem_type, .. } => RuntimeType::List(Box::new(elem_type.clone())),
            Value::Enum { runtime_type, .. } => runtime_type.clone(),
            Value::EnumConstructor { type_name, .. } => RuntimeType::Fun {
                // TODO: store the type for the expected argument of this variant.
                params: vec![RuntimeType::Top],
                return_: Box::new(RuntimeType::UserDefined {
                    kind: TypeDefKind::Enum,
                    name: type_name.clone(),
                    // TODO: look up type arguments.
                    args: vec![],
                }),
            },
            Value::Struct { runtime_type, .. } => runtime_type.clone(),
        }
    }

    fn from_fun_info(fun_info: &FunInfo) -> Self {
        let mut param_types = vec![];
        for param in &fun_info.params {
            let type_ = match &param.type_ {
                Some(hint) => RuntimeType::from_hint(hint),
                None => RuntimeType::Top,
            };
            param_types.push(type_);
        }

        RuntimeType::Fun {
            // TODO: use fun_info
            params: param_types,
            return_: Box::new(RuntimeType::Top),
        }
    }
}

impl Display for RuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeType::UserDefined { name, args, .. } => {
                if args.is_empty() {
                    write!(f, "{}", name.name)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name.name,
                        args.iter()
                            .map(|arg| format!("{}", arg))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            RuntimeType::NoValue => write!(f, "NoValue"),
            RuntimeType::String => write!(f, "String"),
            RuntimeType::Int => write!(f, "Int"),
            RuntimeType::List(elem_type) => write!(f, "List<{}>", elem_type),
            RuntimeType::Fun {
                params: args,
                return_,
            } => {
                let formatted_args = args.iter().map(|a| format!("{a}")).join(", ");
                write!(f, "Fun<({}), {}>", formatted_args, return_)
            }
            RuntimeType::Top => write!(f, "_"),
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
            Value::Enum { type_name, .. } | Value::EnumConstructor { type_name, .. } => {
                &type_name.name
            }
            Value::Struct { type_name, .. } => &type_name.name,
        }
        .to_owned(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub(crate) enum BuiltinFunctionKind {
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
            Value::Fun { name_sym, .. } => format!("(function: {})", name_sym.name),
            Value::Closure(..) => "(closure)".to_string(),
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
            Value::Enum {
                type_name,
                variant_idx,
                payload,
                ..
            } => {
                let type_ = match env.get_type(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{}__OLD_DEFINITION::{}", type_name, variant_idx);
                    }
                };

                let variant_name = match type_ {
                    TypeDef::Builtin(_) => {
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
            } => {
                let type_ = match env.get_type(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!(
                            "{}__OLD_DEFINITION::{} (constructor)",
                            type_name, variant_idx
                        );
                    }
                };

                match type_ {
                    TypeDef::Builtin(_) => {
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
            Value::Enum {
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
    use super::*;

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let env = Env::default();
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(value.display(&env), "\"foo \\\\ \\\" \\n bar\"");
    }
}
