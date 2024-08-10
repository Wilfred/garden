use std::{collections::HashMap, fmt::Display};

use itertools::Itertools as _;

use garden_lang_parser::{
    ast::{FunInfo, Symbol, SyntaxId, TypeHint, TypeName, TypeSymbol},
    position::Position,
};

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
    /// Tuples, e.g. `(Int, String)`.
    Tuple(Vec<Type>),
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
        /// The symbol where this type was mentioned, not where it was
        /// defined.
        name_sym: TypeSymbol,
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
            Type::UserDefined { name_sym, .. } => name_sym.name.name == "NoValue",
            _ => false,
        }
    }

    pub(crate) fn error<T: AsRef<str>>(msg: T) -> Self {
        Type::Error(msg.as_ref().to_owned())
    }

    pub(crate) fn no_value() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name_sym: TypeSymbol {
                name: TypeName {
                    name: "NoValue".to_owned(),
                },
                position: Position::todo(),
                id2: SyntaxId(0),
            },
            args: vec![],
        }
    }

    pub(crate) fn unit() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name_sym: TypeSymbol {
                name: TypeName {
                    name: "Unit".to_owned(),
                },
                position: Position::todo(),
                id2: SyntaxId(0),
            },
            args: vec![],
        }
    }

    pub(crate) fn bool() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name_sym: TypeSymbol {
                name: TypeName {
                    name: "Bool".to_owned(),
                },
                position: Position::todo(),
                id2: SyntaxId(0),
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
            .map(|hint_arg| Self::from_hint(hint_arg, env, type_bindings))
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
                            None => Self::error("Missing type argument to List<>"),
                        };

                        Ok(Type::List(Box::new(elem_type)))
                    }
                    BuiltinType::Tuple => Ok(Type::Tuple(args)),
                    BuiltinType::Fun => match &args[..] {
                        [input_ty, return_] => {
                            let params = match input_ty {
                                Type::Tuple(items) => items.clone(),
                                Type::Error(_) => vec![],
                                _ => {
                                    return Err("The first argument to Fun<> must be a tuple, e.g. `Fun<(Int, Int), String>`.".to_owned());
                                }
                            };

                            Ok(Type::Fun {
                                name: None,
                                type_params: vec![],
                                params,
                                return_: Box::new(return_.clone()),
                            })
                        }
                        _ => Err(format!(
                            "Fun<> takes two type arguments, but got {}",
                            args.len()
                        )),
                    },
                },
                TypeDef::Enum(_) => Ok(Type::UserDefined {
                    kind: TypeDefKind::Enum,
                    name_sym: hint.sym.clone(),
                    args,
                }),
                TypeDef::Struct(_) => Ok(Type::UserDefined {
                    kind: TypeDefKind::Struct,
                    name_sym: hint.sym.clone(),
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
                None => Self::error("No fun_info for built-in function"),
            },
            Value::String(_) => Type::String,
            Value::List { elem_type, .. } => Type::List(Box::new(elem_type.clone())),
            Value::Enum { runtime_type, .. } => runtime_type.clone(),
            Value::EnumConstructor { runtime_type, .. } => runtime_type.clone(),
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
                Some(hint) => Self::from_hint(hint, env, &type_bindings)?,
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

    /// The name of this type. If a type isn't user-denotable, return
    /// None.
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
            Type::Tuple(_) => None,
            Type::Fun { .. } => Some(TypeName {
                name: "Fun".to_owned(),
            }),
            Type::UserDefined {
                kind: _, name_sym, ..
            } => Some(name_sym.name.clone()),
            Type::TypeParameter(name) => Some(name.clone()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UserDefined { name_sym, args, .. } => {
                if args.is_empty() {
                    write!(f, "{}", name_sym.name)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name_sym.name,
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
            Type::Tuple(elem_tys) => write!(
                f,
                "({})",
                elem_tys.iter().map(|ty| format!("{ty}")).join(", ")
            ),
            Type::Fun {
                params: args,
                return_,
                ..
            } => {
                let formatted_args = args.iter().map(|a| format!("{a}")).join(", ");
                write!(f, "Fun<({}), {}>", formatted_args, return_)
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
        (Type::Tuple(lhs_elems), Type::Tuple(rhs_elems)) => {
            if lhs_elems.len() == rhs_elems.len() {
                lhs_elems
                    .iter()
                    .zip(rhs_elems.iter())
                    .all(|(lhs_elem, rhs_elem)| is_subtype(lhs_elem, rhs_elem))
            } else {
                false
            }
        }
        (Type::Tuple(_), _) => false,
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
                if !is_subtype(rhs_param, lhs_param) {
                    return false;
                }
            }

            // Functions are covariant in their return types, so flip the arguments.
            // Fun<(), NoValue> <: Fun<(), Int>
            is_subtype(lhs_return, rhs_return)
        }
        (Type::Fun { .. }, _) => false,
        (
            Type::UserDefined {
                kind: _,
                name_sym: lhs_name_sym,
                args: lhs_args,
            },
            Type::UserDefined {
                kind: _,
                name_sym: rhs_name_sym,
                args: rhs_args,
            },
        ) => {
            // Values in Garden are nominally typed, so we only need
            // to compare type names.
            if lhs_name_sym.name != rhs_name_sym.name {
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
            Err(msg) => Type::error(msg),
        }
    }
}

impl UnwrapOrErrTy for Option<Type> {
    fn unwrap_or_err_ty(&self) -> Type {
        match self {
            Some(ty) => ty.clone(),
            None => Type::error("Got None"),
        }
    }
}
