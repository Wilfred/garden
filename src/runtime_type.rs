use std::{collections::HashMap, fmt::Display};

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
pub(crate) type TypeVarEnv = HashMap<TypeName, Option<Type>>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    /// The top type, which includes all values.
    Top,
    String,
    Int,
    List(Box<Type>),
    Fun {
        /// If this function has a defined name (i.e. not a closure),
        /// the name used.
        name: Option<Symbol>,
        /// E.g. if a function's return type depends on argument
        /// types, we need type_parameters.
        type_params: Vec<TypeName>,
        params: Vec<Type>,
        return_: Box<Type>,
    },
    UserDefined {
        kind: TypeDefKind,
        name: TypeName,
        args: Vec<Type>,
    },
    #[allow(clippy::enum_variant_names)]
    TypeParameter(TypeName),
    /// Represents a type checker error. The string is the internal
    /// reason we had an error, intended for debugging the type
    /// checker.
    Error(String),
}

impl Type {
    pub(crate) fn is_no_value(&self) -> bool {
        match self {
            Type::UserDefined { name, .. } => name.name == "NoValue",
            _ => false,
        }
    }

    pub(crate) fn error<T: AsRef<str>>(msg: T) -> Self {
        Type::Error(msg.as_ref().to_owned())
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
            .map(|hint_arg| Type::from_hint(hint_arg, env, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(type_var_value) = type_bindings.get(name) {
            return match type_var_value {
                Some(runtime_type) => Ok(runtime_type.clone()),
                None => Ok(Type::TypeParameter(name.clone())),
            };
        }

        match env.get_type_def(name) {
            Some(type_) => match type_ {
                TypeDef::Builtin(builtin_type) => match builtin_type {
                    BuiltinType::Int => Ok(Type::Int),
                    BuiltinType::String => Ok(Type::String),
                    BuiltinType::List => {
                        let elem_type = match args.first() {
                            Some(type_) => type_.clone(),
                            None => Type::error("Missing type argument to List<>"),
                        };

                        Ok(Type::List(Box::new(elem_type)))
                    }
                    BuiltinType::Fun => {
                        unreachable!("Currently no userland syntax for function types")
                    }
                },
                TypeDef::Enum(_) => Ok(Type::UserDefined {
                    kind: TypeDefKind::Enum,
                    name: name.clone(),
                    args,
                }),
                TypeDef::Struct(_) => Ok(Type::UserDefined {
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
            Value::Integer(_) => Type::Int,
            Value::Fun { fun_info, .. } | Value::Closure(_, fun_info) => {
                Self::from_fun_info(fun_info, env, type_bindings).unwrap_or_err_ty()
            }
            Value::BuiltinFunction(_, fun_info) => match fun_info {
                Some(fun_info) => {
                    Self::from_fun_info(fun_info, env, type_bindings).unwrap_or_err_ty()
                }
                None => Type::error("No fun_info for built-in function"),
            },
            Value::String(_) => Type::String,
            Value::List { elem_type, .. } => Type::List(Box::new(elem_type.clone())),
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

                        let type_args_on_enum: Vec<Type> = type_params
                            .clone()
                            .into_iter()
                            .map(Type::TypeParameter)
                            .collect::<Vec<_>>();

                        (type_params, type_args_on_enum)
                    }
                    None => (vec![], vec![]),
                };

                Type::Fun {
                    type_params,
                    // TODO: this is assuming the variant is exactly
                    // Foo(T), not e.g. Foo(Int) or Foo(Option(T)).
                    params: vec![type_args_on_enum.first().cloned().unwrap_or_err_ty()],
                    return_: Box::new(Type::UserDefined {
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
        let mut type_bindings = type_bindings.clone();

        let type_params = fun_info
            .type_params
            .iter()
            .map(|tp| tp.name.clone())
            .collect::<Vec<_>>();

        // Update type variable environment.
        for type_param in &type_params {
            type_bindings.insert(type_param.clone(), None);
        }

        let mut param_types = vec![];
        for param in &fun_info.params {
            let type_ = match &param.hint {
                Some(hint) => Type::from_hint(hint, env, &type_bindings)?,
                None => Type::Top,
            };
            param_types.push(type_);
        }

        let return_ = match &fun_info.return_hint {
            Some(hint) => Self::from_hint(hint, env, &type_bindings)?,
            None => Type::Top,
        };

        Ok(Type::Fun {
            type_params,
            params: param_types,
            return_: Box::new(return_),
            name: fun_info.name.clone(),
        })
    }

    pub(crate) fn type_name(&self) -> Option<TypeName> {
        match self {
            Type::Top | Type::Error(_) => None,
            Type::String => Some(TypeName {
                name: "String".to_owned(),
            }),
            Type::Int => Some(TypeName {
                name: "Int".to_owned(),
            }),
            Type::List(_) => Some(TypeName {
                name: "List".to_owned(),
            }),
            Type::Fun { .. } => Some(TypeName {
                name: "Fun".to_owned(),
            }),
            Type::UserDefined { kind: _, name, .. } => Some(name.clone()),
            Type::TypeParameter(name) => Some(name.clone()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UserDefined { name, args, .. } => {
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
            Type::String => write!(f, "String"),
            Type::Int => write!(f, "Int"),
            Type::List(elem_type) => write!(f, "List<{}>", elem_type),
            Type::Fun {
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
            Type::Top => write!(f, "_"),
            Type::TypeParameter(name) => write!(f, "{}", name.name),
            Type::Error(reason) => write!(f, "__ERROR({})", reason),
        }
    }
}

pub(crate) fn is_subtype(lhs: &Type, rhs: &Type) -> bool {
    match (lhs, rhs) {
        (_, Type::Top) => true,
        (_, _) if lhs.is_no_value() => true,
        (Type::Error(_), _) => {
            // Error is equivalent to NoValue: it's a bottom type that
            // is a subtype of everything.
            true
        }
        (_, Type::Error(_)) => {
            // Also allow Error to be a supertype of everything,
            // because we've already emitted a type error elsewhere
            // and we don't want duplicate errors.
            true
        }
        (Type::Int, Type::Int) => true,
        (Type::Int, _) => false,
        (Type::String, Type::String) => true,
        (Type::String, _) => false,
        // A type parameter is only a subtype of itself.
        // TODO: what if the parameters are in different scopes?
        (Type::TypeParameter(lhs_name), Type::TypeParameter(rhs_name)) => lhs_name == rhs_name,
        (Type::TypeParameter(_), _) => false,
        (Type::List(lhs_elem), Type::List(rhs_elem)) => {
            // List is covariant in its element.
            // List<NoValue> <: List<Int>
            is_subtype(lhs_elem, rhs_elem)
        }
        (Type::List(_), _) => false,
        (
            Type::Fun {
                params: lhs_params,
                return_: lhs_return,
                ..
            },
            Type::Fun {
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
        (Type::Fun { .. }, _) => false,
        (
            Type::UserDefined {
                kind: _,
                name: lhs_name,
                args: lhs_args,
            },
            Type::UserDefined {
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
        (Type::UserDefined { .. }, _) => false,
        (Type::Top, _) => {
            // Top is only a subtype of itself, but we've already
            // matched the case where RHS is Top.
            false
        }
    }
}

pub(crate) trait UnwrapOrErrTy {
    fn unwrap_or_err_ty(&self) -> Type;
}

impl UnwrapOrErrTy for Result<Type, String> {
    fn unwrap_or_err_ty(&self) -> Type {
        match self {
            Ok(ty) => ty.clone(),
            Err(msg) => Type::Error(msg.clone()),
        }
    }
}

impl UnwrapOrErrTy for Option<Type> {
    fn unwrap_or_err_ty(&self) -> Type {
        match self {
            Some(ty) => ty.clone(),
            None => Type::Error("Got None".to_owned()),
        }
    }
}
