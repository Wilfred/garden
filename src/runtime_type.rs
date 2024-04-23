use std::{collections::HashMap, fmt::Display};

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
    pub(crate) fn is_no_value(&self) -> bool {
        match self {
            RuntimeType::UserDefined { name, .. } => name.name == "NoValue",
            _ => false,
        }
    }

    pub(crate) fn no_value() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                name: "NoValue".to_owned(),
            },
            args: vec![],
        }
    }

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

    pub(crate) fn string_list() -> Self {
        Self::List(Box::new(Self::String))
    }

    pub(crate) fn from_hint(
        hint: &TypeHint,
        env: &Env,
        type_bindings: &HashMap<TypeName, RuntimeType>,
    ) -> Result<Self, String> {
        let name = &hint.sym.name;

        let args = hint
            .args
            .iter()
            .map(|hint_arg| RuntimeType::from_hint(hint_arg, env, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(runtime_type) = type_bindings.get(name) {
            return Ok(runtime_type.clone());
        }

        match env.get_type_def(name) {
            Some(type_) => match type_ {
                TypeDef::Builtin(builtin_type) => match builtin_type {
                    BuiltinType::Int => Ok(RuntimeType::Int),
                    BuiltinType::String => Ok(RuntimeType::String),
                    BuiltinType::List => {
                        let elem_type = match args.first() {
                            Some(type_) => type_.clone(),
                            None => RuntimeType::Top,
                        };

                        Ok(RuntimeType::List(Box::new(elem_type)))
                    }
                    BuiltinType::Fun => {
                        unreachable!("Currently no userland syntax for function types")
                    }
                },
                TypeDef::Enum(_) => Ok(RuntimeType::UserDefined {
                    kind: TypeDefKind::Enum,
                    name: name.clone(),
                    args,
                }),
                TypeDef::Struct(_) => Ok(RuntimeType::UserDefined {
                    kind: TypeDefKind::Struct,
                    name: name.clone(),
                    args,
                }),
            },
            None => Err(format!("No such type: {}", name)),
        }
    }

    pub(crate) fn from_value(value: &Value) -> Self {
        match value {
            Value::Integer(_) => RuntimeType::Int,
            Value::Fun { fun_info, .. } | Value::Closure(_, fun_info) => {
                // TODO: store runtime type information in closures,
                // and this shouldn't be necessary?
                let env = Env::default();

                let mut type_bindings: HashMap<TypeName, RuntimeType> = HashMap::new();
                for type_param in &fun_info.type_params {
                    type_bindings.insert(type_param.name.clone(), RuntimeType::Top);
                }

                Self::from_fun_info(fun_info, &env, &type_bindings).unwrap_or(RuntimeType::Top)
            }
            Value::BuiltinFunction(_, fun_info) => match fun_info {
                Some(fun_info) => {
                    // TODO: store runtime type information in closures,
                    // and this shouldn't be necessary?
                    let env = Env::default();

                    let mut type_bindings: HashMap<TypeName, RuntimeType> = HashMap::new();
                    for type_param in &fun_info.type_params {
                        type_bindings.insert(type_param.name.clone(), RuntimeType::Top);
                    }

                    Self::from_fun_info(fun_info, &env, &type_bindings).unwrap_or(RuntimeType::Top)
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

    pub(crate) fn from_fun_info(
        fun_info: &FunInfo,
        env: &Env,
        type_bindings: &HashMap<TypeName, RuntimeType>,
    ) -> Result<Self, String> {
        let mut param_types = vec![];
        for param in &fun_info.params {
            let type_ = match &param.type_ {
                Some(hint) => RuntimeType::from_hint(hint, env, type_bindings)?,
                None => RuntimeType::Top,
            };
            param_types.push(type_);
        }

        let return_ = match &fun_info.return_type {
            Some(hint) => Self::from_hint(hint, env, type_bindings)?,
            None => RuntimeType::Top,
        };

        Ok(RuntimeType::Fun {
            params: param_types,
            return_: Box::new(return_),
        })
    }

    pub(crate) fn type_name(&self) -> Option<TypeName> {
        match self {
            RuntimeType::Top => None,
            RuntimeType::String => Some(TypeName {
                name: "String".to_owned(),
            }),
            RuntimeType::Int => Some(TypeName {
                name: "Int".to_owned(),
            }),
            RuntimeType::List(_) => Some(TypeName {
                name: "List".to_owned(),
            }),
            RuntimeType::Fun { .. } => Some(TypeName {
                name: "Fun".to_owned(),
            }),
            RuntimeType::UserDefined { kind: _, name, .. } => Some(name.clone()),
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

pub(crate) fn is_subtype(lhs: &RuntimeType, rhs: &RuntimeType) -> bool {
    match (lhs, rhs) {
        (_, RuntimeType::Top) => true,
        (_, _) if lhs.is_no_value() => true,
        (RuntimeType::Int, RuntimeType::Int) => true,
        (RuntimeType::Int, _) => false,
        (RuntimeType::String, RuntimeType::String) => true,
        (RuntimeType::String, _) => false,
        (RuntimeType::List(lhs_elem), RuntimeType::List(rhs_elem)) => {
            // List is covariant in its element.
            // List<NoValue> <: List<Int>
            is_subtype(lhs_elem, rhs_elem)
        }
        (RuntimeType::List(_), _) => false,
        (
            RuntimeType::Fun {
                params: lhs_params,
                return_: lhs_return,
            },
            RuntimeType::Fun {
                params: rhs_params,
                return_: rhs_return,
            },
        ) => {
            if lhs_params.len() != rhs_params.len() {
                return false;
            }

            // Functions are contravariant in their arguments.
            // Fun<(Top,), Unit> <: Fun<(Int,), Unit>
            for (lhs_param, rhs_param) in lhs_params.iter().zip(rhs_params) {
                if !is_subtype(lhs_param, rhs_param) {
                    return false;
                }
            }

            // Functions are covariant in their return types, so flip the arguments.
            // Fun<(), NoValue> <: Fun<(), Int>
            is_subtype(rhs_return, lhs_return)
        }
        (RuntimeType::Fun { .. }, _) => false,
        (
            RuntimeType::UserDefined {
                kind: _,
                name: lhs_name,
                args: lhs_args,
            },
            RuntimeType::UserDefined {
                kind: _,
                name: rhs_name,
                args: rhs_args,
            },
        ) => {
            // Values in Garden are nominally typed, so we only need
            // to compare type names.
            if lhs_name != rhs_name {
                return false;
            }

            if lhs_args.len() != rhs_args.len() {
                return false;
            }

            // Garden values are currently exclusively immutable, so
            // we can assume that all user-defined types have
            // covariant arguments.
            // Foo<NoValue> <: Foo<Int>
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args) {
                if !is_subtype(lhs_arg, rhs_arg) {
                    return false;
                }
            }

            true
        }
        (RuntimeType::UserDefined { .. }, _) => false,
        (RuntimeType::Top, _) => {
            // Top is only a subtype of itself, but we've already
            // matched the case where RHS is Top.
            false
        }
    }
}
