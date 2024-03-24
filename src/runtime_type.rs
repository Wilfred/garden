use std::fmt::Display;

use itertools::Itertools as _;

use garden_lang_parser::ast::{FunInfo, TypeHint, TypeName};

use crate::{
    env::Env,
    types::{BuiltinType, TypeDef},
    values::Value,
};

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

    pub(crate) fn from_hint(hint: &TypeHint, env: &Env) -> Result<Self, ()> {
        let name = &hint.sym.name;
        if name.name == "NoValue" {
            return Ok(RuntimeType::NoValue);
        }
        if name.name == "String" {
            return Ok(RuntimeType::String);
        }
        if name.name == "Int" {
            return Ok(RuntimeType::Int);
        }
        if name.name == "List" {
            let elem_type = match hint.args.first() {
                Some(arg) => RuntimeType::from_hint(arg, env)?,
                None => RuntimeType::Top,
            };

            return Ok(RuntimeType::List(Box::new(elem_type)));
        }

        let args: Result<Vec<_>, _> = hint
            .args
            .iter()
            .map(|hint_arg| RuntimeType::from_hint(hint_arg, env))
            .collect();

        let typedef_kind = match env.get_type(name) {
            Some(type_) => match type_ {
                TypeDef::Builtin(builtin_type) => match builtin_type {
                    BuiltinType::Int => todo!(),
                    BuiltinType::String => todo!(),
                    BuiltinType::Fun => todo!(),
                    BuiltinType::List => todo!(),
                },
                TypeDef::Enum(_) => TypeDefKind::Enum,
                TypeDef::Struct(_) => TypeDefKind::Struct,
            },
            None => {
                return Err(());
            }
        };

        Ok(RuntimeType::UserDefined {
            kind: typedef_kind,
            name: name.clone(),
            args: args?,
        })
    }

    pub(crate) fn from_value(value: &Value) -> Self {
        match value {
            Value::Integer(_) => RuntimeType::Int,
            Value::Fun { fun_info, .. } | Value::Closure(_, fun_info) => {
                // TODO: store runtime type information in closures,
                // and this shouldn't be necessary?
                let env = Env::default();
                Self::from_fun_info(fun_info, &env).unwrap_or(RuntimeType::Top)
            }
            Value::BuiltinFunction(_, fun_info) => match fun_info {
                Some(fun_info) => {
                    // TODO: store runtime type information in closures,
                    // and this shouldn't be necessary?
                    let env = Env::default();
                    Self::from_fun_info(fun_info, &env).unwrap_or(RuntimeType::Top)
                }
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

    fn from_fun_info(fun_info: &FunInfo, env: &Env) -> Result<Self, ()> {
        let mut param_types = vec![];
        for param in &fun_info.params {
            let type_ = match &param.type_ {
                Some(hint) => RuntimeType::from_hint(hint, env)?,
                None => RuntimeType::Top,
            };
            param_types.push(type_);
        }

        let return_ = match &fun_info.return_type {
            Some(hint) => Self::from_hint(hint, env)?,
            None => RuntimeType::Top,
        };

        Ok(RuntimeType::Fun {
            params: param_types,
            return_: Box::new(return_),
        })
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
