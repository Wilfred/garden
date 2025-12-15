use std::cell::RefCell;
use std::fmt::Display;
use std::path::PathBuf;
use std::rc::Rc;

use rustc_hash::FxHashMap;
use strum_macros::EnumIter;

use crate::env::Env;
use crate::eval::BlockBindings;
use crate::garden_type::{Type, TypeDefKind};
use crate::namespaces::NamespaceInfo;
use crate::parser::ast::{FunInfo, Symbol, SymbolName, TypeName};
use crate::parser::vfs::to_project_relative;
use crate::types::TypeDef;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Value(pub(crate) Rc<Value_>);

impl Value {
    pub(crate) fn as_ref(&self) -> &Value_ {
        self.0.as_ref()
    }

    pub(crate) fn new(v: Value_) -> Self {
        Self(Rc::new(v))
    }

    /// If this value is a function, return its info.
    pub(crate) fn fun_info(&self) -> Option<&FunInfo> {
        match self.as_ref() {
            Value_::Fun { fun_info, .. } => Some(fun_info),
            Value_::Closure(_, fun_info, _) => Some(fun_info),
            Value_::BuiltInFunction(_, fun_info, _) => fun_info.as_ref(),
            _ => None,
        }
    }

    /// If this value is a function, return its doc comment.
    pub(crate) fn doc_comment(&self) -> Option<String> {
        let fun_info = self.fun_info()?;
        fun_info.doc_comment.clone()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Value_ {
    /// An integer value.
    Integer(i64),
    /// A reference to a user-defined function, along with its return
    /// type.
    Fun {
        name_sym: Symbol,
        fun_info: FunInfo,
        runtime_type: Type,
    },
    /// A closure value.
    Closure(Vec<BlockBindings>, FunInfo, Type),
    /// A reference to a built-in function.
    BuiltInFunction(BuiltInFunctionKind, Option<FunInfo>, Option<Type>),
    /// A string value.
    String(String),
    /// A list value, along with the type of its elements.
    List { items: Vec<Value>, elem_type: Type },
    /// A tuple value, along with type of each item.
    Tuple {
        items: Vec<Value>,
        item_types: Vec<Type>,
    },
    /// A dictionary, a hash map with string keys.
    Dict {
        items: FxHashMap<String, Value>,
        /// The type of the values in this dict, e.g. Int in `Dict["x" => 1]`.
        value_type: Type,
    },
    /// A value in a user-defined enum, such as `True` or `Some(123)`.
    EnumVariant {
        type_name: TypeName,
        runtime_type: Type,
        variant_idx: usize,
        payload: Option<Box<Value>>,
    },
    // A function that takes one argument (the payload of this enum
    // variant), and returns an enum value. For example, `Some`.
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
    /// When we import a namespace with `import "./foo.gdn" as f`,
    /// this is the value that is stored in `f`.
    Namespace {
        /// The imported namespace.
        ns_info: Rc<RefCell<NamespaceInfo>>,
        /// The name that this namespace value has in the current
        /// scope (`f` in the above example).
        imported_name_sym: Symbol,
    },
}

impl PartialEq for Value_ {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value_::Integer(i1), Value_::Integer(i2)) => i1 == i2,
            (
                Value_::Fun { name_sym, .. },
                Value_::Fun {
                    name_sym: other_name_sym,
                    ..
                },
            ) => name_sym == other_name_sym,
            (Value_::Closure(_, _, _), Value_::Closure(_, _, _)) => {
                // TODO: we should probably use reference equality on
                // closures in Value, instead of always returning
                // false.
                false
            }
            (
                Value_::BuiltInFunction(self_kind, _, _),
                Value_::BuiltInFunction(other_kind, _, _),
            ) => self_kind == other_kind,
            (Value_::String(s1), Value_::String(s2)) => s1 == s2,
            (
                Value_::List {
                    items: self_items,
                    elem_type: _,
                },
                Value_::List {
                    items: other_items,
                    elem_type: _,
                },
            ) => {
                // We don't consider list type when comparing list
                // values. This ensures that [] == [] regardless of
                // how the lists were constructed (a source of runtime
                // bugs previously).
                self_items == other_items
            }
            (
                Value_::Tuple {
                    items: self_items,
                    item_types: _,
                },
                Value_::Tuple {
                    items: other_items,
                    item_types: _,
                },
            ) => {
                // We don't consider type when comparing tuple
                // values.
                self_items == other_items
            }
            (
                Value_::EnumVariant {
                    runtime_type: self_runtime_type,
                    variant_idx: self_variant_idx,
                    payload: self_payload,
                    ..
                },
                Value_::EnumVariant {
                    runtime_type: other_runtime_type,
                    variant_idx: other_variant_idx,
                    payload: other_payload,
                    ..
                },
            ) => {
                self_runtime_type == other_runtime_type
                    && self_variant_idx == other_variant_idx
                    && self_payload == other_payload
            }
            (
                Value_::EnumConstructor {
                    runtime_type: self_runtime_type,
                    variant_idx: self_variant_idx,
                    ..
                },
                Value_::EnumConstructor {
                    runtime_type: other_runtime_type,
                    variant_idx: other_variant_idx,
                    ..
                },
            ) => self_runtime_type == other_runtime_type && self_variant_idx == other_variant_idx,
            (
                Value_::Struct {
                    fields: self_fields,
                    runtime_type: self_runtime_type,
                    ..
                },
                Value_::Struct {
                    fields: other_fields,
                    runtime_type: other_runtime_type,
                    ..
                },
            ) => self_runtime_type == other_runtime_type && self_fields == other_fields,
            _ => false,
        }
    }
}

impl Eq for Value_ {}

impl Value {
    /// A helper for creating a unit value.
    pub(crate) fn unit() -> Self {
        // We can assume that Unit is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Unit".to_owned(),
            },
            runtime_type: Type::unit(),
            variant_idx: 0,
            payload: None,
        })
    }

    pub(crate) fn bool(b: bool) -> Self {
        // We can assume that Bool is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Bool".to_owned(),
            },
            runtime_type: Type::bool(),
            variant_idx: if b { 0 } else { 1 },
            payload: None,
        })
    }

    pub(crate) fn as_rust_bool(&self) -> Option<bool> {
        match self.as_ref() {
            Value_::EnumVariant {
                runtime_type,
                variant_idx,
                ..
            } if runtime_type == &Type::bool() => Some(*variant_idx == 0),
            _ => None,
        }
    }

    pub(crate) fn some(v: Value) -> Self {
        let value_type = Type::from_value(&v);

        // We can assume that Option is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Option".to_owned(),
            },
            variant_idx: 0,
            payload: Some(Box::new(v)),
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    text: "Option".to_owned(),
                },
                args: vec![value_type],
            },
        })
    }

    pub(crate) fn none() -> Self {
        // We can assume that Option is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Option".to_owned(),
            },
            variant_idx: 1,
            payload: None,
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    text: "Option".to_owned(),
                },
                args: vec![Type::no_value()],
            },
        })
    }

    pub(crate) fn ok(v: Value) -> Self {
        let value_type = Type::from_value(&v);

        // We can assume that Result is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Result".to_owned(),
            },
            variant_idx: 0,
            payload: Some(Box::new(v)),
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    text: "Result".to_owned(),
                },
                args: vec![value_type, Type::no_value()],
            },
        })
    }

    pub(crate) fn err(v: Value) -> Self {
        let value_type = Type::from_value(&v);

        // We can assume that Result is always defined because it's in the
        // prelude.
        Self::new(Value_::EnumVariant {
            type_name: TypeName {
                text: "Result".to_owned(),
            },
            runtime_type: Type::UserDefined {
                kind: TypeDefKind::Enum,
                name: TypeName {
                    text: "Result".to_owned(),
                },
                args: vec![Type::no_value(), value_type],
            },
            variant_idx: 1,
            payload: Some(Box::new(v)),
        })
    }

    pub(crate) fn path(inner: String) -> Self {
        Self::new(Value_::Struct {
            type_name: TypeName {
                text: "Path".to_owned(),
            },
            fields: vec![(SymbolName::from("p"), Value::new(Value_::String(inner)))],
            runtime_type: Type::path(),
        })
    }
}

/// The name of type associated with this value. We will use this type
/// name to look up available methods.
pub(crate) fn type_representation(value: &Value) -> TypeName {
    TypeName {
        text: match value.as_ref() {
            Value_::Integer(_) => "Int",
            Value_::Fun { .. } => "Fun",
            Value_::Closure(_, _, _) => "Fun",
            Value_::BuiltInFunction(_, _, _) => "Fun",
            Value_::String(_) => "String",
            Value_::List { .. } => "List",
            Value_::Tuple { .. } => "Tuple",
            Value_::Dict { .. } => "Dict",
            Value_::EnumVariant { type_name, .. } | Value_::EnumConstructor { type_name, .. } => {
                &type_name.text
            }
            Value_::Struct { type_name, .. } => &type_name.text,
            Value_::Namespace { .. } => "Namespace",
        }
        .to_owned(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub(crate) enum BuiltInFunctionKind {
    Throw,
    ListDirectory,
    Shell,
    StringRepr,
    Print,
    Println,
    SourceDirectory,
    // TODO: It's a little confusing that we have both shell() and
    // shell_arguments(), these should go in separate namespaces once
    // we have namespaces.
    ShellArguments,
    WorkingDirectory,
    SetWorkingDirectory,
    WriteFile,
    CheckSnippet,
    Lex,
    DocComment,
    DocCommentForType,
    DocCommentForMethod,
    SourceForFun,
    SourceForMethod,
    SourceForType,
    PreludeTypes,
    NamespaceFunctions,
    MethodsForType,
    GetEnv,
    Keywords,
    RandomInt,
    CreateDir,
    RemoveDir,
    CopyFile,
    RemoveFile,
    Unixtime,
    BuiltinFiles,
}

impl BuiltInFunctionKind {
    pub(crate) fn namespace_path(&self) -> PathBuf {
        match self {
            BuiltInFunctionKind::Throw
            | BuiltInFunctionKind::Shell
            | BuiltInFunctionKind::StringRepr
            | BuiltInFunctionKind::Print
            | BuiltInFunctionKind::Println
            | BuiltInFunctionKind::SourceDirectory
            | BuiltInFunctionKind::ShellArguments
            | BuiltInFunctionKind::GetEnv => PathBuf::from("__prelude.gdn"),
            BuiltInFunctionKind::WriteFile
            | BuiltInFunctionKind::ListDirectory
            | BuiltInFunctionKind::WorkingDirectory
            | BuiltInFunctionKind::SetWorkingDirectory
            | BuiltInFunctionKind::CreateDir
            | BuiltInFunctionKind::RemoveDir
            | BuiltInFunctionKind::CopyFile
            | BuiltInFunctionKind::RemoveFile => PathBuf::from("__fs.gdn"),
            BuiltInFunctionKind::SourceForFun
            | BuiltInFunctionKind::SourceForMethod
            | BuiltInFunctionKind::SourceForType
            | BuiltInFunctionKind::PreludeTypes
            | BuiltInFunctionKind::Lex
            | BuiltInFunctionKind::DocComment
            | BuiltInFunctionKind::DocCommentForType
            | BuiltInFunctionKind::DocCommentForMethod
            | BuiltInFunctionKind::CheckSnippet
            | BuiltInFunctionKind::MethodsForType
            | BuiltInFunctionKind::NamespaceFunctions
            | BuiltInFunctionKind::Keywords
            | BuiltInFunctionKind::BuiltinFiles => PathBuf::from("__reflect.gdn"),
            BuiltInFunctionKind::RandomInt => PathBuf::from("__random.gdn"),
            BuiltInFunctionKind::Unixtime => PathBuf::from("__time.gdn"),
        }
    }
}

impl Display for BuiltInFunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BuiltInFunctionKind::Throw => "throw",
            BuiltInFunctionKind::ListDirectory => "list_directory",
            BuiltInFunctionKind::Shell => "shell",
            BuiltInFunctionKind::StringRepr => "string_repr",
            BuiltInFunctionKind::Print => "print",
            BuiltInFunctionKind::Println => "println",
            BuiltInFunctionKind::SourceDirectory => "source_directory",
            BuiltInFunctionKind::ShellArguments => "shell_arguments",
            BuiltInFunctionKind::SetWorkingDirectory => "set_working_directory",
            BuiltInFunctionKind::WorkingDirectory => "working_directory",
            BuiltInFunctionKind::WriteFile => "write_file",
            BuiltInFunctionKind::CheckSnippet => "check_snippet",
            BuiltInFunctionKind::Lex => "lex",
            BuiltInFunctionKind::DocComment => "doc_comment",
            BuiltInFunctionKind::DocCommentForType => "doc_comment_for_type",
            BuiltInFunctionKind::DocCommentForMethod => "doc_comment_for_method",
            BuiltInFunctionKind::SourceForFun => "source_for_fun",
            BuiltInFunctionKind::SourceForMethod => "source_for_method",
            BuiltInFunctionKind::SourceForType => "source_for_type",
            BuiltInFunctionKind::PreludeTypes => "prelude_types",
            BuiltInFunctionKind::NamespaceFunctions => "namespace_functions",
            BuiltInFunctionKind::MethodsForType => "methods_for_type",
            BuiltInFunctionKind::GetEnv => "get_env",
            BuiltInFunctionKind::Keywords => "keywords",
            BuiltInFunctionKind::RandomInt => "int",
            BuiltInFunctionKind::CreateDir => "create_dir",
            BuiltInFunctionKind::RemoveDir => "remove_dir",
            BuiltInFunctionKind::CopyFile => "copy_file",
            BuiltInFunctionKind::RemoveFile => "remove_file",
            BuiltInFunctionKind::Unixtime => "unixtime",
            BuiltInFunctionKind::BuiltinFiles => "builtin_files",
        };
        write!(f, "{name}")
    }
}

impl Value {
    /// Pretty-print `self` in a human friendly way.
    pub(crate) fn display(&self, env: &Env) -> String {
        match self.as_ref() {
            Value_::Integer(i) => format!("{i}"),
            Value_::Fun { name_sym, .. } => format!("{}", name_sym.name),
            Value_::Closure(..) => "(closure)".to_owned(),
            Value_::BuiltInFunction(kind, _, _) => format!("{kind}"),
            Value_::String(s) => escape_string_literal(s),
            Value_::List { items, .. } => {
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
            Value_::Tuple { items, .. } => {
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
            Value_::Dict { items, .. } => {
                let mut s = String::new();
                s.push_str("Dict[");

                let mut keys_and_values = items.iter().collect::<Vec<_>>();
                keys_and_values.sort_by_key(|(k, _v)| *k);

                for (i, (key, value)) in keys_and_values.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&format!(
                        "{} => {}",
                        escape_string_literal(key),
                        value.display(env)
                    ));
                }

                s.push(']');
                s
            }
            Value_::EnumVariant {
                type_name,
                variant_idx,
                payload,
                ..
            } => {
                let type_ = match env.get_type_def(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{type_name}__OLD_DEFINITION::{variant_idx}");
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
                        None => format!("{type_name}::__OLD_VARIANT_{variant_idx}"),
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
            Value_::EnumConstructor {
                type_name,
                variant_idx,
                ..
            } => {
                let type_ = match env.get_type_def(type_name) {
                    Some(type_) => type_,
                    None => {
                        return format!("{type_name}__OLD_DEFINITION::{variant_idx} (constructor)");
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
                            format!("{type_name}::__OLD_VARIANT_{variant_idx} (constructor)")
                        }
                    },
                    TypeDef::Struct(struct_info) => {
                        format!("{}__OLD_DEFINITION (constructor)", struct_info.name_sym)
                    }
                }
            }
            Value_::Struct {
                type_name, fields, ..
            } => {
                let mut s = format!("{type_name}{{ ");

                for (i, (field_name, value)) in fields.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&format!("{}: {}", field_name, value.display(env)));
                }

                s.push_str(" }");

                s
            }
            Value_::Namespace { ns_info, .. } => {
                let ns_info = ns_info.borrow();

                let mut names = ns_info
                    .exported_syms
                    .iter()
                    .map(|sym| format!("  ::{}", sym.text))
                    .collect::<Vec<_>>();
                names.sort();

                let names_str = names.join("\n");

                format!(
                    "namespace:{}{}{}",
                    to_project_relative(&ns_info.abs_path, &env.project_root).display(),
                    if names_str.is_empty() { "" } else { "\n" },
                    names_str
                )
            }
        }
    }

    pub(crate) fn display_unless_unit(&self, env: &Env) -> Option<String> {
        match self.as_ref() {
            Value_::EnumVariant {
                type_name,
                variant_idx,
                ..
            } if type_name.text == "Unit" && *variant_idx == 0 => None,
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
    use crate::parser::ast::IdGenerator;
    use crate::Vfs;

    use super::*;

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let id_gen = IdGenerator::default();
        let vfs = Vfs::default();
        let env = Env::new(id_gen, vfs);

        let value = Value::new(Value_::String("foo \\ \" \n bar".into()));
        assert_eq!(value.display(&env), "\"foo \\\\ \\\" \\n bar\"");
    }
}
