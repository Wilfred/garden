use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::Itertools as _;

use garden_lang_parser::ast::{FunInfo, Symbol, TypeHint, TypeName};

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

/// The current type variable environment. When new type variables are
/// defined, they're added with a value of None.
pub(crate) type TypeVarEnv = HashMap<TypeName, Option<RuntimeType>>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RuntimeType {
    /// The top type, which includes all values.
    Top,
    String,
    Int,
    List(Box<RuntimeType>),
    Fun {
        /// If this function has a defined name (i.e. not a closure),
        /// the name used.
        name: Option<Symbol>,
        /// E.g. if a function's return type depends on argument
        /// types, we need type_parameters.
        type_params: Vec<TypeName>,
        params: Vec<RuntimeType>,
        return_: Box<RuntimeType>,
    },
    UserDefined {
        kind: TypeDefKind,
        name: TypeName,
        args: Vec<RuntimeType>,
    },
    TypeParameter(TypeName),
    /// Represents a type checker error. The string is the internal
    /// reason we had an error, intended for debugging the type
    /// checker.
    Error(String),
}

impl RuntimeType {
    pub(crate) fn is_no_value(&self) -> bool {
        match self {
            RuntimeType::UserDefined { name, .. } => name.name == "NoValue",
            _ => false,
        }
    }

    pub(crate) fn error<T: AsRef<str>>(msg: T) -> Self {
        RuntimeType::Error(msg.as_ref().to_owned())
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
        type_bindings: &TypeVarEnv,
    ) -> Result<Self, String> {
        let name = &hint.sym.name;

        let args = hint
            .args
            .iter()
            .map(|hint_arg| RuntimeType::from_hint(hint_arg, env, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(type_var_value) = type_bindings.get(name) {
            return match type_var_value {
                Some(runtime_type) => Ok(runtime_type.clone()),
                None => Ok(RuntimeType::TypeParameter(name.clone())),
            };
        }

        match env.get_type_def(name) {
            Some(type_) => match type_ {
                TypeDef::Builtin(builtin_type) => match builtin_type {
                    BuiltinType::Int => Ok(RuntimeType::Int),
                    BuiltinType::String => Ok(RuntimeType::String),
                    BuiltinType::List => {
                        let elem_type = match args.first() {
                            Some(type_) => type_.clone(),
                            None => RuntimeType::error("Missing type argument to List<>"),
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

    pub(crate) fn from_value(value: &Value, env: &Env, type_bindings: &TypeVarEnv) -> Self {
        match value {
            Value::Integer(_) => RuntimeType::Int,
            Value::Fun { fun_info, .. } | Value::Closure(_, fun_info) => {
                Self::from_fun_info(fun_info, env, type_bindings).unwrap_or_err_ty()
            }
            Value::BuiltinFunction(_, fun_info) => match fun_info {
                Some(fun_info) => {
                    Self::from_fun_info(fun_info, env, type_bindings).unwrap_or_err_ty()
                }
                None => RuntimeType::error("No fun_info for built-in function"),
            },
            Value::String(_) => RuntimeType::String,
            Value::List { elem_type, .. } => RuntimeType::List(Box::new(elem_type.clone())),
            Value::Enum { runtime_type, .. } => runtime_type.clone(),
            Value::EnumConstructor { type_name, .. } => {
                // TODO: store type information on EnumConstructor
                // instead of hoping that the prelude has the
                // information we need.
                let env = Env::default();

                let (type_params, type_args_on_enum) = match env.get_type_def(type_name) {
                    Some(type_def) => {
                        let type_params: Vec<TypeName> = match type_def {
                            TypeDef::Builtin(_) => vec![],
                            TypeDef::Enum(enum_info) => enum_info.type_params.clone(),
                            TypeDef::Struct(struct_info) => struct_info.type_params.clone(),
                        }
                        .iter()
                        .map(|tp| tp.name.clone())
                        .collect();

                        let type_args_on_enum: Vec<RuntimeType> = type_params
                            .clone()
                            .into_iter()
                            .map(RuntimeType::TypeParameter)
                            .collect::<Vec<_>>();

                        (type_params, type_args_on_enum)
                    }
                    None => (vec![], vec![]),
                };

                RuntimeType::Fun {
                    type_params,
                    // TODO: this is assuming the variant is exactly
                    // Foo(T), not e.g. Foo(Int) or Foo(Option(T)).
                    params: vec![type_args_on_enum.first().cloned().unwrap_or_err_ty()],
                    return_: Box::new(RuntimeType::UserDefined {
                        kind: TypeDefKind::Enum,
                        name: type_name.clone(),
                        args: type_args_on_enum,
                    }),
                    name: None,
                }
            }
            Value::Struct { runtime_type, .. } => runtime_type.clone(),
        }
    }

    pub(crate) fn from_fun_info(
        fun_info: &FunInfo,
        env: &Env,
        type_bindings: &TypeVarEnv,
    ) -> Result<Self, String> {
        let type_params = fun_info
            .type_params
            .iter()
            .map(|tp| tp.name.clone())
            .collect::<Vec<_>>();

        let type_params_set = type_params.iter().collect::<HashSet<_>>();

        let mut param_types = vec![];
        for param in &fun_info.params {
            let type_ = match &param.hint {
                Some(hint) => {
                    if type_params_set.contains(&hint.sym.name) {
                        RuntimeType::TypeParameter(hint.sym.name.clone())
                    } else {
                        RuntimeType::from_hint(hint, env, type_bindings)?
                    }
                }
                None => RuntimeType::Top,
            };
            param_types.push(type_);
        }

        let return_ = match &fun_info.return_hint {
            Some(hint) => Self::from_hint(hint, env, &env.type_bindings())?,
            None => RuntimeType::Top,
        };

        Ok(RuntimeType::Fun {
            type_params,
            params: param_types,
            return_: Box::new(return_),
            name: fun_info.name.clone(),
        })
    }

    pub(crate) fn type_name(&self) -> Option<TypeName> {
        match self {
            RuntimeType::Top | RuntimeType::Error(_) => None,
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
            RuntimeType::TypeParameter(name) => Some(name.clone()),
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
                type_params,
                ..
            } => {
                let formatted_type_params = if type_params.is_empty() {
                    "".to_owned()
                } else {
                    format!("({})", type_params.iter().map(|tp| &tp.name).join(", "))
                };

                let formatted_args = args.iter().map(|a| format!("{a}")).join(", ");
                write!(
                    f,
                    "Fun{}<({}), {}>",
                    formatted_type_params, formatted_args, return_
                )
            }
            RuntimeType::Top => write!(f, "_"),
            RuntimeType::TypeParameter(name) => write!(f, "{}", name.name),
            RuntimeType::Error(reason) => write!(f, "__ERROR({})", reason),
        }
    }
}

pub(crate) fn is_subtype(lhs: &RuntimeType, rhs: &RuntimeType) -> bool {
    match (lhs, rhs) {
        (_, RuntimeType::Top) => true,
        (_, _) if lhs.is_no_value() => true,
        (RuntimeType::Error(_), _) => {
            // Error is equivalent to NoValue: it's a bottom type that
            // is a subtype of everything.
            true
        }
        (_, RuntimeType::Error(_)) => {
            // Also allow Error to be a supertype of everything,
            // because we've already emitted a type error elsewhere
            // and we don't want duplicate errors.
            true
        }
        (RuntimeType::Int, RuntimeType::Int) => true,
        (RuntimeType::Int, _) => false,
        (RuntimeType::String, RuntimeType::String) => true,
        (RuntimeType::String, _) => false,
        // A type parameter is only a subtype of itself.
        // TODO: what if the parameters are in different scopes?
        (RuntimeType::TypeParameter(lhs_name), RuntimeType::TypeParameter(rhs_name)) => {
            lhs_name == rhs_name
        }
        (RuntimeType::TypeParameter(_), _) => false,
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
                ..
            },
            RuntimeType::Fun {
                params: rhs_params,
                return_: rhs_return,
                ..
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

pub(crate) trait UnwrapOrErrTy {
    fn unwrap_or_err_ty(&self) -> RuntimeType;
}

impl UnwrapOrErrTy for Result<RuntimeType, String> {
    fn unwrap_or_err_ty(&self) -> RuntimeType {
        match self {
            Ok(ty) => ty.clone(),
            Err(msg) => RuntimeType::Error(msg.clone()),
        }
    }
}

impl UnwrapOrErrTy for Option<RuntimeType> {
    fn unwrap_or_err_ty(&self) -> RuntimeType {
        match self {
            Some(ty) => ty.clone(),
            None => RuntimeType::Error("Got None".to_owned()),
        }
    }
}
