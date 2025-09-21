use std::fmt::Display;

use itertools::Itertools as _;

use rustc_hash::FxHashMap;

use crate::env::Env;
use crate::parser::ast::{FunInfo, Symbol, TypeHint, TypeName};
use crate::parser::position::Position;
use crate::types::{BuiltinType, TypeDef, TypeDefAndMethods};
use crate::values::{Value, Value_};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeDefKind {
    Enum,
    Struct,
}

/// The current type variable environment. When new type variables are
/// defined, they're added with a value of None.
pub(crate) type TypeVarEnv = FxHashMap<TypeName, Option<Type>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
    /// The top type, which includes all values.
    Any,
    /// Tuples, e.g. `(Int, String)`.
    Tuple(Vec<Type>),
    /// Function type, e.g. `Fun<(Int, String), Unit>`
    Fun {
        /// If this function has a defined name (i.e. not a closure),
        /// the name used.
        name_sym: Option<Symbol>,
        /// E.g. if a function's return type depends on argument
        /// types, we need type_parameters.
        type_params: Vec<TypeName>,
        params: Vec<Type>,
        return_: Box<Type>,
    },
    /// A user-defined type, e.g. `Foo<Bar>`.
    UserDefined {
        kind: TypeDefKind,
        name: TypeName,
        args: Vec<Type>,
    },
    /// An unsolved type parameter, e.g. `T` in `List<T>`.
    #[allow(clippy::enum_variant_names)]
    TypeParameter(TypeName),
    /// Represents a type checker error. This is rendered as `_` when
    /// printing types.
    Error {
        /// The internal reason we had an error, intended for
        /// debugging the type checker.
        internal_reason: String,
        /// If we inferred a type but it didn't match the surrounding
        /// context, we emit a type error and mark the inferred type
        /// as Error. This is important for avoiding cascading type
        /// errors.
        ///
        /// However, for IDE features like hover and
        /// extract function, we want know the originally inferred
        /// type.
        inferred_type: Option<Box<Type>>,
    },
}

impl Type {
    pub(crate) fn is_no_value(&self) -> bool {
        match self {
            Type::UserDefined { name, .. } => name.text == "NoValue",
            _ => false,
        }
    }

    pub(crate) fn is_error(&self) -> bool {
        matches!(self, Type::Error { .. })
    }

    pub(crate) fn is_unit(&self) -> bool {
        let Type::UserDefined { name, .. } = self else {
            return false;
        };

        name.text == "Unit"
    }

    pub(crate) fn error<T: AsRef<str>>(msg: T) -> Self {
        Type::Error {
            internal_reason: msg.as_ref().to_owned(),
            inferred_type: None,
        }
    }

    pub(crate) fn no_value() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                text: "NoValue".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn unit() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                text: "Unit".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn bool() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Enum,
            name: TypeName {
                text: "Bool".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn int() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "Int".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn string() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "String".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn namespace() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "Namespace".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn path() -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "Path".to_owned(),
            },
            args: vec![],
        }
    }

    pub(crate) fn list(ty: Type) -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "List".to_owned(),
            },
            args: vec![ty],
        }
    }

    pub(crate) fn dict(ty: Type) -> Self {
        Self::UserDefined {
            kind: TypeDefKind::Struct,
            name: TypeName {
                text: "Dict".to_owned(),
            },
            args: vec![ty],
        }
    }

    pub(crate) fn from_hint(
        hint: &TypeHint,
        global_tys: &FxHashMap<TypeName, TypeDefAndMethods>,
        type_bindings: &TypeVarEnv,
    ) -> Result<Self, String> {
        let name = &hint.sym.name;

        let args = hint
            .args
            .iter()
            .map(|hint_arg| Self::from_hint(hint_arg, global_tys, type_bindings))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(type_var_value) = type_bindings.get(name) {
            return match type_var_value {
                Some(runtime_type) => Ok(runtime_type.clone()),
                None => Ok(Type::TypeParameter(name.clone())),
            };
        }

        match global_tys.get(name) {
            Some(type_) => match &type_.def {
                TypeDef::Builtin(builtin_type, _) => match builtin_type {
                    BuiltinType::Int => Ok(Type::int()),
                    BuiltinType::String => Ok(Type::string()),
                    BuiltinType::Namespace => Ok(Type::namespace()),
                    BuiltinType::List => {
                        let elem_type = match args.first() {
                            Some(type_) => type_.clone(),
                            None => Self::error("Missing type argument to List<>"),
                        };

                        Ok(Type::list(elem_type))
                    }
                    BuiltinType::Dict => {
                        let elem_type = match args.first() {
                            Some(type_) => type_.clone(),
                            None => Self::error("Missing type argument to Dict<>"),
                        };

                        Ok(Type::dict(elem_type))
                    }
                    BuiltinType::Tuple => Ok(Type::Tuple(args)),
                    BuiltinType::Fun => match &args[..] {
                        [input_ty, return_] => {
                            let params = match input_ty {
                                Type::Tuple(items) => items.clone(),
                                Type::Error { .. } => vec![],
                                _ => {
                                    return Err("The first argument to Fun<> must be a tuple, e.g. `Fun<(Int, Int), String>`.".to_owned());
                                }
                            };

                            Ok(Type::Fun {
                                name_sym: None,
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
                    name: hint.sym.name.clone(),
                    args,
                }),
                TypeDef::Struct(_) => Ok(Type::UserDefined {
                    kind: TypeDefKind::Struct,
                    name: hint.sym.name.clone(),
                    args,
                }),
            },
            None => Err(format!("No such type: {name}")),
        }
    }

    pub(crate) fn from_value(value: &Value) -> Self {
        match value.as_ref() {
            Value_::Integer(_) => Type::int(),
            Value_::Fun { runtime_type, .. } => runtime_type.clone(),
            Value_::Closure(_, _, runtime_type) => runtime_type.clone(),
            Value_::BuiltinFunction(_, _, Some(runtime_type)) => runtime_type.clone(),
            Value_::BuiltinFunction(_, _, None) => Self::error("No fun_info for built-in function"),
            Value_::String(_) => Type::string(),
            Value_::List { elem_type, .. } => Type::list(elem_type.clone()),
            Value_::Tuple { item_types, .. } => Type::Tuple(item_types.clone()),
            Value_::Dict { elem_type, .. } => Type::dict(elem_type.clone()),
            Value_::EnumVariant { runtime_type, .. } => runtime_type.clone(),
            Value_::EnumConstructor { runtime_type, .. } => runtime_type.clone(),
            Value_::Struct { runtime_type, .. } => runtime_type.clone(),
            Value_::Namespace { .. } => Type::namespace(),
        }
    }

    pub(crate) fn from_fun_info(
        fun_info: &FunInfo,
        global_tys: &FxHashMap<TypeName, TypeDefAndMethods>,
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
        for param in &fun_info.params.params {
            let type_ = match &param.hint {
                Some(hint) => Self::from_hint(hint, global_tys, &type_bindings)?,
                None => Type::Any,
            };
            param_types.push(type_);
        }

        let return_ = match &fun_info.return_hint {
            Some(hint) => Self::from_hint(hint, global_tys, &type_bindings)?,
            None => {
                if fun_info.body.exprs.is_empty() {
                    Self::unit()
                } else {
                    // We don't necessarily know what values are being
                    // passed in to this closure, so conservatively
                    // use Any.
                    Type::Any
                }
            }
        };

        Ok(Type::Fun {
            type_params,
            params: param_types,
            return_: Box::new(return_),
            name_sym: fun_info.name_sym.clone(),
        })
    }

    /// The name of this type. If a type isn't user-denotable, return
    /// None.
    pub(crate) fn type_name(&self) -> Option<TypeName> {
        match self {
            Type::Any | Type::Error { .. } => None,
            Type::Tuple(_) => None,
            Type::Fun { .. } => Some(TypeName {
                text: "Fun".to_owned(),
            }),
            Type::UserDefined {
                kind: _,
                name: name_sym,
                ..
            } => Some(name_sym.clone()),
            Type::TypeParameter(name) => Some(name.clone()),
        }
    }

    /// The position of the definition of this type. If a type isn't
    /// user-denotable, return None.
    pub(crate) fn def_sym_pos(&self, env: &Env) -> Option<Position> {
        match self {
            Type::Any => None,
            Type::Error { inferred_type, .. } => match inferred_type {
                Some(ty) => ty.def_sym_pos(env),
                None => None,
            },
            Type::Tuple(_) => None,
            Type::Fun { .. } => None,
            Type::UserDefined { name, .. } => {
                let def = env.get_type_def(name)?;
                let name_sym = match def {
                    TypeDef::Builtin(_, Some(struct_info)) => Some(&struct_info.name_sym),
                    TypeDef::Builtin(_, None) => None,
                    TypeDef::Enum(enum_info) => Some(&enum_info.name_sym),
                    TypeDef::Struct(struct_info) => Some(&struct_info.name_sym),
                }?;
                Some(name_sym.position.clone())
            }
            Type::TypeParameter(_) => {
                // TODO: type parameters should store positions too.
                None
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UserDefined {
                name: name_sym,
                args,
                ..
            } => {
                if args.is_empty() {
                    write!(f, "{}", name_sym.text)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name_sym.text,
                        args.iter()
                            .map(|arg| format!("{arg}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
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
                write!(f, "Fun<({formatted_args}), {return_}>")
            }
            Type::Any => write!(f, "Any"),
            Type::TypeParameter(name) => write!(f, "{}", name.text),
            Type::Error {
                internal_reason: reason,
                inferred_type,
            } => match inferred_type {
                Some(inferred_type) => write!(f, "__ERROR({inferred_type}, {reason})"),
                None => write!(f, "__ERROR({reason})"),
            },
        }
    }
}

/// Is `lhs` a subtype of `rhs`, i.e. is `lhs <: rhs` in type system
/// notation?
pub(crate) fn is_subtype(lhs: &Type, rhs: &Type) -> bool {
    match (lhs, rhs) {
        (_, Type::Any) => true,
        (_, _) if lhs.is_no_value() => true,
        (Type::Error { .. }, _) => {
            // Error is equivalent to NoValue: it's a bottom type that
            // is a subtype of everything.
            true
        }
        (_, Type::Error { .. }) => {
            // Also allow Error to be a supertype of everything,
            // because we've already emitted a type error elsewhere
            // and we don't want duplicate errors.
            true
        }
        // A type parameter is only a subtype of itself.
        // TODO: what if the parameters are in different scopes?
        (Type::TypeParameter(lhs_name), Type::TypeParameter(rhs_name)) => lhs_name == rhs_name,
        (Type::TypeParameter(_), _) => false,
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

            // Functions are contravariant in their arguments, so flip the arguments.
            // Fun<(Top,), Unit> <: Fun<(Int,), Unit>
            for (lhs_param, rhs_param) in lhs_params.iter().zip(rhs_params) {
                if !is_subtype(rhs_param, lhs_param) {
                    return false;
                }
            }

            // Functions are covariant in their return types.
            // Fun<(), NoValue> <: Fun<(), Int>
            is_subtype(lhs_return, rhs_return)
        }
        (Type::Fun { .. }, _) => false,
        (
            Type::UserDefined {
                kind: _,
                name: lhs_name_sym,
                args: lhs_args,
            },
            Type::UserDefined {
                kind: _,
                name: rhs_name_sym,
                args: rhs_args,
            },
        ) => {
            // Values in Garden are nominally typed, so we only need
            // to compare type names.
            if lhs_name_sym.text != rhs_name_sym.text {
                return false;
            }

            // Garden values are currently exclusively immutable, so
            // we can assume that all user-defined types have
            // covariant arguments.
            // Foo<NoValue> <: Foo<Int>
            // List<NoValue> <: List<Int>
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args) {
                if !is_subtype(lhs_arg, rhs_arg) {
                    return false;
                }
            }

            true
        }
        (Type::UserDefined { .. }, _) => false,
        (Type::Any, _) => {
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
