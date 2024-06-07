use std::collections::HashMap;

use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, Expression, Expression_, FunInfo, MethodInfo, Pattern, Symbol,
    SymbolName, ToplevelItem, TypeHint, TypeName, VariantInfo,
};
use garden_lang_parser::position::Position;

use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::garden_type::{is_subtype, Type, TypeDefKind, TypeVarEnv, UnwrapOrErrTy as _};
use crate::types::TypeDef;
use crate::values::Value;
use crate::visitor::Visitor;

pub(crate) fn check_types(items: &[ToplevelItem], env: &Env) -> Vec<Diagnostic> {
    let mut env = env.clone();

    let mut visitor = TypeCheckVisitor {
        env: &mut env,
        warnings: vec![],
        bindings: LocalBindings::default(),
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.warnings
}

#[derive(Debug)]
struct LocalBindings {
    blocks: Vec<HashMap<SymbolName, Type>>,
}

impl Default for LocalBindings {
    fn default() -> Self {
        Self {
            blocks: vec![HashMap::new()],
        }
    }
}

impl LocalBindings {
    fn enter_block(&mut self) {
        self.blocks.push(HashMap::new());
    }

    fn exit_block(&mut self) {
        self.blocks.pop();
    }

    fn get(&self, name: &SymbolName) -> Option<&Type> {
        for block in self.blocks.iter().rev() {
            if let Some(ty) = block.get(name) {
                return Some(ty);
            }
        }

        None
    }

    fn set(&mut self, name: SymbolName, ty: Type) {
        let block = self.blocks.last_mut().expect("Should be non-empty");
        block.insert(name, ty);
    }
}

#[derive(Debug)]
struct TypeCheckVisitor<'a> {
    env: &'a mut Env,
    warnings: Vec<Diagnostic>,
    bindings: LocalBindings,
}

impl Visitor for TypeCheckVisitor<'_> {
    fn visit_method_info(&mut self, method_info: &MethodInfo) {
        self.bindings.enter_block();

        let mut type_bindings = self.env.type_bindings();
        if let Some(fun_info) = method_info.fun_info() {
            for type_param in &fun_info.type_params {
                type_bindings.insert(type_param.name.clone(), None);
            }
        }

        let self_ty = Type::from_hint(&method_info.receiver_hint, self.env, &type_bindings)
            .unwrap_or_err_ty();
        self.bindings
            .set(method_info.receiver_sym.name.clone(), self_ty);

        // TODO: generic variables are bound here.

        self.visit_method_info_default(method_info);

        self.bindings.exit_block();
    }

    fn visit_fun_info(&mut self, fun_info: &FunInfo) {
        // Skip typechecking builtins and prelude to help print debugging.
        if let Some(n) = &fun_info.name {
            let path = n.position.path.clone();
            if path.display().to_string().ends_with("prelude.gdn") {
                return;
            }
            if path.display().to_string().ends_with("builtins.gdn") {
                return;
            }
        }

        self.bindings.enter_block();

        for param in &fun_info.params {
            let param_ty = match &param.hint {
                Some(hint) => {
                    Type::from_hint(hint, self.env, &self.env.type_bindings()).unwrap_or_err_ty()
                }
                None => Type::Top,
            };
            self.bindings.set(param.symbol.name.clone(), param_ty);
        }

        check_fun_info(fun_info, self.env, &mut self.bindings, &mut self.warnings);

        self.bindings.exit_block();
    }

    fn visit_block(&mut self, block: &Block) {
        assign_expr_ids(block);

        // check_block recurses, so don't recurse in the visitor
        check_block(
            block,
            self.env,
            &mut self.bindings,
            &mut self.warnings,
            None,
        );
    }
}

fn assign_expr_ids(block: &Block) {
    let mut visitor = AssignExprIds::default();
    visitor.visit_block(block);
}

#[derive(Debug, Default, Clone)]
struct AssignExprIds {
    next_id: usize,
}

impl Visitor for AssignExprIds {
    fn visit_expr(&mut self, expr: &Expression) {
        expr.id
            .set(self.next_id)
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.expr_)
    }
}

fn check_block(
    block: &Block,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
    expected_return_ty: Option<&Type>,
) -> Type {
    bindings.enter_block();

    let mut ty = Type::unit();
    for expr in &block.exprs {
        ty = check_expr(expr, env, bindings, warnings, expected_return_ty);
    }

    bindings.exit_block();
    ty
}

fn enum_payload_type(env: &Env, scrutinee_ty: &Type, pattern_sym: &Symbol) -> Type {
    let Some(scrutinee_ty_name) = scrutinee_ty.type_name() else {
        return Type::error(
            "No type name for match scrutinee, we should have errored elsewhere already.",
        );
    };

    let Some(type_def) = env.get_type_def(&scrutinee_ty_name) else {
        return Type::error("No type definition found with this type name.");
    };

    let TypeDef::Enum(enum_info) = type_def else {
        return Type::error("Matching on a type that isn't an enum.");
    };

    let mut relevant_variant = None;
    for variant in &enum_info.variants {
        if variant.name_sym.name == pattern_sym.name {
            relevant_variant = Some(variant.clone());
        }
    }

    let Some(variant) = relevant_variant else {
        return Type::error(format!(
            "No variant found in `{}` named `{}`.",
            scrutinee_ty_name, pattern_sym.name
        ));
    };

    let Some(payload_hint) = variant.payload_hint else {
        return Type::error("This enum variant does not have a payload.");
    };

    // If this is a variant like `Some(T)`, find the value for this
    // generic parameter.
    let Type::UserDefined { args, .. } = scrutinee_ty else {
        return Type::error("Match scrutinee value is not an enum.");
    };

    // If the payload is a generic type from the enum definition, use
    // the value for that generic from the value.
    for (type_def_param, value_type_param) in type_def.params().iter().zip(args) {
        if payload_hint.sym.name == type_def_param.name {
            return value_type_param.clone();
        }
    }

    // The payload is not a generic type, so the type hint is
    // referring to a defined type.
    Type::from_hint(&payload_hint, env, &env.type_bindings()).unwrap_or_err_ty()
}

fn check_expr(
    expr: &Expression,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
    expected_return_ty: Option<&Type>,
) -> Type {
    match &expr.expr_ {
        Expression_::Match(scrutinee, cases) => {
            let scrutinee_ty = check_expr(scrutinee, env, bindings, warnings, expected_return_ty);
            let scrutinee_ty_name = scrutinee_ty.type_name();

            let mut case_tys = vec![];

            if let Some(scrutinee_ty_name) = &scrutinee_ty_name {
                let patterns: Vec<_> = cases.iter().map(|(p, _)| p.clone()).collect();
                check_match_exhaustive(env, &scrutinee.pos, scrutinee_ty_name, &patterns, warnings);
            }

            for (pattern, case_expr) in cases {
                bindings.enter_block();

                if let Some(payload_sym) = &pattern.argument {
                    if !payload_sym.name.is_underscore() {
                        bindings.set(
                            payload_sym.name.clone(),
                            enum_payload_type(env, &scrutinee_ty, &pattern.symbol),
                        );
                    }
                }

                case_tys.push(check_expr(
                    case_expr,
                    env,
                    bindings,
                    warnings,
                    expected_return_ty,
                ));

                bindings.exit_block();

                // Matching `_` works for any type.
                if pattern.symbol.name.is_underscore() {
                    continue;
                }

                let Some(value) = env.file_scope.get(&pattern.symbol.name) else {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!("No such type `{}`.", pattern.symbol.name),
                        position: pattern.symbol.position.clone(),
                    });
                    continue;
                };

                let pattern_type_name = match value {
                    Value::Enum { type_name, .. } => type_name,
                    Value::EnumConstructor { type_name, .. } => type_name,
                    _ => {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "Expected an enum variant here, but got `{}`.",
                                value.display(env)
                            ),
                            position: pattern.symbol.position.clone(),
                        });
                        continue;
                    }
                };

                let Some(scrutinee_ty_name) = &scrutinee_ty_name else {
                    continue;
                };
                if pattern_type_name != scrutinee_ty_name {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!(
                            "This match case is for `{}`, but you're matching on a `{}`.",
                            pattern_type_name, &scrutinee_ty_name,
                        ),
                        position: pattern.symbol.position.clone(),
                    });
                }
            }

            match unify_all(&case_tys) {
                Some(ty) => ty,
                None => {
                    let last_case = cases
                        .last()
                        .expect("Type errors should require at least one match case.");
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: "`match` cases have different types..".to_string(),
                        position: last_case.1.pos.clone(),
                    });

                    Type::error("Cases had incompatible types.")
                }
            }
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings, expected_return_ty);
            if !is_subtype(&cond_ty, &Type::bool()) {
                warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!(
                        "Expected `Bool` inside an `if` condition, but got `{}`.",
                        cond_ty
                    ),
                    position: cond_expr.pos.clone(),
                });
            }

            let then_ty = check_block(then_block, env, bindings, warnings, expected_return_ty);

            let else_ty = match else_block {
                Some(else_block) => {
                    check_block(else_block, env, bindings, warnings, expected_return_ty)
                }
                None => Type::unit(),
            };

            match unify(&then_ty, &else_ty) {
                Some(ty) => ty,
                None => {
                    let message = if else_block.is_some() {
                        format!(
                            "`if` and `else` have incompatible types: `{}` and `{}`.",
                            then_ty, else_ty
                        )
                    } else {
                        format!(
                            "`if` expressions without `else` should have type `Unit`, but got `{}`.",
                            then_ty
                        )
                    };

                    let position = match then_block.exprs.last() {
                        Some(last_expr) => last_expr.pos.clone(),
                        None => cond_expr.pos.clone(),
                    };

                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message,
                        position,
                    });

                    Type::error("Incompatible if blocks")
                }
            }
        }
        Expression_::While(cond_expr, body) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings, expected_return_ty);
            if !is_subtype(&cond_ty, &Type::bool()) {
                warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!(
                        "Expected `Bool` inside an `while` condition, but got `{}`.",
                        cond_ty
                    ),
                    position: cond_expr.pos.clone(),
                });
            }

            check_block(body, env, bindings, warnings, expected_return_ty);

            Type::unit()
        }
        Expression_::Break => Type::unit(),
        Expression_::Assign(_sym, expr) => {
            check_expr(expr, env, bindings, warnings, expected_return_ty)
        }
        Expression_::Let(sym, hint, expr) => {
            let expr_ty = check_expr(expr, env, bindings, warnings, expected_return_ty);

            let ty = match hint {
                Some(hint) => {
                    let hint_ty =
                        Type::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();

                    if !is_subtype(&expr_ty, &hint_ty) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "Expected `{}` for this let expression, but got `{}`.",
                                hint_ty, expr_ty
                            ),
                            position: hint.position.clone(),
                        });
                    }

                    hint_ty
                }
                None => expr_ty,
            };

            bindings.set(sym.name.clone(), ty);

            Type::unit()
        }
        Expression_::Return(inner_expr) => {
            let (ty, position) = if let Some(inner_expr) = inner_expr {
                (
                    check_expr(inner_expr, env, bindings, warnings, expected_return_ty),
                    inner_expr.pos.clone(),
                )
            } else {
                (Type::unit(), expr.pos.clone())
            };

            if let Some(expected_return_ty) = expected_return_ty {
                if !is_subtype(&ty, expected_return_ty) {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!(
                            "Expected this function to return `{}`, but got `{}`.",
                            expected_return_ty, ty
                        ),
                        position,
                    });
                }
            }

            // `return` terminates the current function, so we can't
            // use this expression. Infer as bottom.
            Type::no_value()
        }
        Expression_::IntLiteral(_) => Type::Int,
        Expression_::StringLiteral(_) => Type::String,
        Expression_::ListLiteral(items) => {
            let mut elem_ty = Type::no_value();

            for item in items {
                let item_ty = check_expr(item, env, bindings, warnings, expected_return_ty);
                // TODO: unify the types of all elements in the
                // list, rather than letting the last win.
                elem_ty = item_ty;
            }

            Type::List(Box::new(elem_ty))
        }
        Expression_::StructLiteral(name_sym, fields) => {
            for (_, expr) in fields {
                check_expr(expr, env, bindings, warnings, expected_return_ty);
            }

            if let Some(TypeDef::Struct(_)) = env.get_type_def(&name_sym.name) {
                Type::UserDefined {
                    kind: TypeDefKind::Struct,
                    name: name_sym.name.clone(),
                    args: vec![],
                }
            } else {
                Type::Error("Unbound struct name".to_owned())
            }
        }
        Expression_::BinaryOperator(lhs, op, rhs) => {
            let lhs_ty = check_expr(lhs, env, bindings, warnings, expected_return_ty);
            let rhs_ty = check_expr(rhs, env, bindings, warnings, expected_return_ty);

            match op {
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Subtract
                | BinaryOperatorKind::Multiply
                | BinaryOperatorKind::Divide => {
                    if !is_subtype(&lhs_ty, &Type::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                            position: lhs.pos.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &Type::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                            position: rhs.pos.clone(),
                        });
                    }

                    Type::Int
                }
                BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessThanOrEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::GreaterThanOrEqual => {
                    if !is_subtype(&lhs_ty, &Type::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                            position: lhs.pos.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &Type::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                            position: rhs.pos.clone(),
                        });
                    }

                    Type::bool()
                }
                BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                    if lhs_ty != rhs_ty {
                        warnings.push(Diagnostic { level: Level::Warning,
                                message: format!("Left hand side has type `{}`, but right hand side has type `{}`, so this will always have the same result.", lhs_ty, rhs_ty),
                                position: rhs.pos.clone(),
                            });
                    }

                    Type::bool()
                }
                BinaryOperatorKind::And | BinaryOperatorKind::Or => {
                    if !is_subtype(&lhs_ty, &Type::bool()) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Bool`, but got `{}`.", lhs_ty),
                            position: lhs.pos.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &Type::bool()) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Bool`, but got `{}`.", rhs_ty),
                            position: rhs.pos.clone(),
                        });
                    }

                    Type::bool()
                }
            }
        }
        Expression_::Variable(sym) => {
            if let Some(value_ty) = bindings.get(&sym.name) {
                return value_ty.clone();
            }

            match env.file_scope.get(&sym.name) {
                Some(value) => Type::from_value(value, env, &env.type_bindings()),
                None => Type::Error("Unbound variable".to_owned()),
            }
        }
        Expression_::Call(recv, paren_args) => {
            let recv_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let arg_tys = paren_args
                .arguments
                .iter()
                .map(|arg| {
                    (
                        check_expr(arg, env, bindings, warnings, expected_return_ty),
                        arg.pos.clone(),
                    )
                })
                .collect::<Vec<_>>();

            match recv_ty {
                Type::Fun {
                    params,
                    return_,
                    name,
                    ..
                } => {
                    let formatted_name = match name {
                        Some(name) => format!("`{}`", name.name),
                        None => "This function".to_owned(),
                    };

                    if params.len() != paren_args.arguments.len() {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "{} expects {} argument{}, but got {}.",
                                formatted_name,
                                params.len(),
                                if params.len() == 1 { "" } else { "s" },
                                paren_args.arguments.len()
                            ),
                            position: recv.pos.clone(),
                        });
                    }

                    let mut ty_var_env = HashMap::default();

                    for (param_ty, (arg_ty, _arg_pos)) in params.iter().zip(arg_tys.iter()) {
                        unify_and_solve_ty(param_ty, arg_ty, &mut ty_var_env);
                    }

                    let params = params
                        .iter()
                        .map(|p| subst_ty_vars(p, &ty_var_env))
                        .collect::<Vec<_>>();

                    for (param_ty, (arg_ty, arg_pos)) in params.iter().zip(arg_tys) {
                        if !is_subtype(&arg_ty, param_ty) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected `{}` argument but got `{}`.",
                                    param_ty, arg_ty
                                ),
                                position: arg_pos,
                            });
                        }
                    }

                    subst_ty_vars(&return_, &ty_var_env)
                }
                _ => {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!("Expected a function, but got a `{}`.", recv_ty),
                        position: recv.pos.clone(),
                    });

                    Type::Error("Calling something that isn't a function".to_owned())
                }
            }
        }
        Expression_::MethodCall(recv, sym, paren_args) => {
            let arg_tys: Vec<(Type, Position)> = paren_args
                .arguments
                .iter()
                .map(|arg| {
                    (
                        check_expr(arg, env, bindings, warnings, expected_return_ty),
                        arg.pos.clone(),
                    )
                })
                .collect::<Vec<_>>();

            let receiver_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let Some(receiver_ty_name) = receiver_ty.type_name() else {
                return Type::error("No type name for this method receiver");
            };

            let methods = env
                .methods
                .get(&receiver_ty_name)
                .cloned()
                .unwrap_or_default();

            match methods.get(&sym.name) {
                Some(method_info) => {
                    let Some(fun_info) = method_info.fun_info() else {
                        return Type::error("This method has no fun_info");
                    };
                    if fun_info.params.len() != paren_args.arguments.len() {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "`{}::{}` requires {} argument{}, but got {}.",
                                receiver_ty_name,
                                sym.name,
                                fun_info.params.len(),
                                if fun_info.params.len() == 1 { "" } else { "s" },
                                paren_args.arguments.len()
                            ),
                            position: sym.position.clone(),
                        });
                    }

                    for (param, (arg_ty, arg_pos)) in fun_info.params.iter().zip(&arg_tys) {
                        let Some(param_hint) = &param.hint else {
                            continue;
                        };
                        let Ok(param_ty) = Type::from_hint(param_hint, env, &env.type_bindings())
                        else {
                            continue;
                        };

                        if !is_subtype(arg_ty, &param_ty) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!(
                                    "Expected `{}` argument but got `{}`.",
                                    param_ty, arg_ty
                                ),
                                position: arg_pos.clone(),
                            });
                        }
                    }

                    let mut ty_var_env = TypeVarEnv::default();
                    for type_param in &fun_info.type_params {
                        ty_var_env.insert(type_param.name.clone(), None);
                    }

                    let (more_warnings, ret_ty) = subst_type_vars_in_meth_return_ty(
                        env,
                        method_info,
                        &recv.pos,
                        &receiver_ty,
                        &arg_tys,
                        &mut ty_var_env,
                    );
                    warnings.extend(more_warnings);

                    ret_ty
                }
                None => {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!("`{}` has no method `{}`.", receiver_ty_name, sym.name),
                        position: sym.position.clone(),
                    });
                    Type::error("No such method on this type")
                }
            }
        }
        Expression_::DotAccess(recv, field_sym) => {
            let recv_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let Some(recv_ty_name) = recv_ty.type_name() else {
                return Type::error("No type name found this receiver");
            };

            match recv_ty {
                Type::UserDefined {
                    kind: TypeDefKind::Struct,
                    name,
                    ..
                } => {
                    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&name) {
                        for field in &struct_info.fields {
                            if field.sym.name == field_sym.name {
                                let field_ty =
                                    Type::from_hint(&field.hint, env, &env.type_bindings())
                                        .unwrap_or_err_ty();
                                return field_ty;
                            }
                        }

                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "Struct `{}` has no field `{}`.",
                                recv_ty_name, field_sym.name
                            ),
                            position: field_sym.position.clone(),
                        });

                        Type::error("No struct field with this name")
                    } else {
                        Type::error("No struct with this name")
                    }
                }
                _ => Type::error("This type is not a struct"),
            }
        }
        Expression_::FunLiteral(fun_info) => check_fun_info(fun_info, env, bindings, warnings),
        Expression_::Block(block) => {
            check_block(block, env, bindings, warnings, expected_return_ty)
        }
    }
}

fn check_fun_info(
    fun_info: &FunInfo,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
) -> Type {
    // Check the function body with the locals bound.
    bindings.enter_block();

    // Only bind and check locals that have an explicit type
    // hint.
    for param in &fun_info.params {
        if let Some(hint) = &param.hint {
            let param_ty = Type::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();
            bindings.set(param.symbol.name.clone(), param_ty);
        }
    }

    let expected_return_ty = match &fun_info.return_hint {
        Some(hint) => {
            let ty = Type::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();
            Some(ty)
        }
        None => None,
    };

    let body_ty = check_block(
        &fun_info.body,
        env,
        bindings,
        warnings,
        expected_return_ty.as_ref(),
    );

    bindings.exit_block();

    // Build the type representation of a function matching this lambda.
    let mut param_tys = vec![];
    for param in &fun_info.params {
        let param_ty = match &param.hint {
            Some(hint) => Type::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty(),
            None => Type::Top,
        };
        param_tys.push(param_ty);
    }

    let return_ty = match &fun_info.return_hint {
        Some(hint) => {
            let return_ty = Type::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();

            if !is_subtype(&body_ty, &return_ty) {
                warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!("Expected to return `{}` but got `{}`.", return_ty, body_ty),
                    position: hint.position.clone(),
                });
            }

            return_ty
        }
        None => body_ty,
    };

    Type::Fun {
        type_params: vec![],
        params: param_tys,
        return_: Box::new(return_ty),
        name: fun_info.name.clone(),
    }
}

/// Solve the type variables in this method, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_meth_return_ty(
    env: &Env,
    method_info: &MethodInfo,
    receiver_pos: &Position,
    receiver_ty: &Type,
    arg_tys: &[(Type, Position)],
    ty_var_env: &mut TypeVarEnv,
) -> (Vec<Diagnostic>, Type) {
    let mut diagnostics = vec![];
    let Some(fun_info) = method_info.fun_info() else {
        return (diagnostics, Type::error("This method has no fun_info"));
    };

    if let Err(diagnostic) = unify_and_solve_hint(
        env,
        &method_info.receiver_hint,
        receiver_pos,
        receiver_ty,
        ty_var_env,
    ) {
        diagnostics.push(diagnostic);
    }

    subst_type_vars_in_fun_info_return_ty(env, &fun_info, arg_tys, ty_var_env)
}

/// Solve the type variables in this function, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_fun_info_return_ty(
    env: &Env,
    fun_info: &FunInfo,
    arg_tys: &[(Type, Position)],
    ty_var_env: &mut TypeVarEnv,
) -> (Vec<Diagnostic>, Type) {
    let mut diagnostics = vec![];

    for ((arg_ty, arg_pos), param) in arg_tys.iter().zip(&fun_info.params) {
        if let Some(param_hint) = &param.hint {
            if let Err(diagnostic) =
                unify_and_solve_hint(env, param_hint, arg_pos, arg_ty, ty_var_env)
            {
                diagnostics.push(diagnostic);
            }
        }
    }

    match &fun_info.return_hint {
        Some(return_hint) => {
            let hint_name = &return_hint.sym.name;
            let ty = if let Some(ty_var_val) = ty_var_env.get(hint_name) {
                match ty_var_val {
                    Some(ty) => ty.clone(),
                    None => {
                        // This type variable was never used, other
                        // than the return position. Solve to bottom.
                        Type::no_value()
                    }
                }
            } else {
                Type::from_hint(return_hint, env, &env.type_bindings()).unwrap_or_err_ty()
            };

            (diagnostics, ty)
        }
        None => (diagnostics, Type::Top),
    }
}

fn subst_ty_vars(ty: &Type, ty_var_env: &TypeVarEnv) -> Type {
    match ty {
        Type::Error(_) | Type::Top | Type::String | Type::Int => ty.clone(),
        Type::List(elem_ty) => Type::List(Box::new(subst_ty_vars(elem_ty, ty_var_env))),
        Type::Fun {
            type_params,
            params,
            return_,
            name,
        } => {
            let params = params
                .iter()
                .map(|p| subst_ty_vars(p, ty_var_env))
                .collect();
            let return_ = subst_ty_vars(return_, ty_var_env);

            Type::Fun {
                type_params: type_params.clone(),
                params,
                return_: Box::new(return_),
                name: name.clone(),
            }
        }
        Type::UserDefined { kind, name, args } => Type::UserDefined {
            kind: kind.clone(),
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| subst_ty_vars(arg, ty_var_env))
                .collect(),
        },
        Type::TypeParameter(name) => {
            match ty_var_env.get(name) {
                Some(ty_var_val) => ty_var_val.as_ref().cloned().unwrap_or(Type::no_value()),
                None => ty.clone(), // TODO: define an error type
            }
        }
    }
}

fn unify_and_solve_ty(expected_ty: &Type, actual_ty: &Type, ty_var_env: &mut TypeVarEnv) {
    match (expected_ty, actual_ty) {
        (Type::TypeParameter(n), _) => {
            ty_var_env.insert(n.clone(), Some(actual_ty.clone()));
        }
        (Type::List(expected_elem_ty), Type::List(actual_elem_ty)) => {
            unify_and_solve_ty(expected_elem_ty, actual_elem_ty, ty_var_env)
        }
        (
            Type::Fun {
                params: expected_params,
                return_: expected_return,
                ..
            },
            Type::Fun {
                params: actual_params,
                return_: actual_return,
                ..
            },
        ) => {
            unify_and_solve_ty(expected_return, actual_return, ty_var_env);
            for (expected_param, actual_param) in expected_params.iter().zip(actual_params.iter()) {
                unify_and_solve_ty(expected_param, actual_param, ty_var_env);
            }
        }
        (
            Type::UserDefined {
                kind: expected_kind,
                name: expected_name,
                args: expected_args,
            },
            Type::UserDefined {
                kind: actual_kind,
                name: actual_name,
                args: actual_args,
            },
        ) if expected_kind == actual_kind && expected_name == actual_name => {
            for (expected_arg, actual_arg) in expected_args.iter().zip(actual_args.iter()) {
                unify_and_solve_ty(expected_arg, actual_arg, ty_var_env);
            }
        }
        _ => {}
    }
}

/// If `hint` is `Option<T>` and `ty` is the type representation of
/// `Option<Int>`, insert `T = Int` into `ty_var_env`.
fn unify_and_solve_hint(
    env: &Env,
    hint: &TypeHint,
    position: &Position,
    ty: &Type,
    ty_var_env: &mut TypeVarEnv,
) -> Result<(), Diagnostic> {
    let hint_name = &hint.sym.name;
    if let Some(ty_var_val) = ty_var_env.get(hint_name) {
        // If the type named in this hint is a generic type, we're done.
        match ty_var_val {
            Some(bound_ty) => {
                // We've already solved this type variable. Confirm
                // that this occurrence of the type variable has the
                // same type.
                if !is_subtype(ty, bound_ty) {
                    return Err(Diagnostic {
                        message: format!(
                            "`{}` has type `{}`, but got `{}`.",
                            hint.as_src(),
                            bound_ty,
                            ty,
                        ),
                        position: position.clone(),
                        level: Level::Warning,
                    });
                }
            }
            None => {
                // First occurrence of this type variable, assume it
                // has the type we're seeing here.
                //
                // TODO: Accumulate constraints and solve later, so we
                // can handle multiple occurrences where some are
                // NoValue.
                ty_var_env.insert(hint_name.clone(), Some(ty.clone()));
                return Ok(());
            }
        }
    }

    if hint_name.name == "List" && hint.args.len() == 1 {
        match ty {
            Type::List(elem_ty) => {
                return unify_and_solve_hint(env, &hint.args[0], position, elem_ty, ty_var_env);
            }
            _ => {
                // No solving to do.
                return Ok(());
            }
        }
    }

    let Some(type_def) = env.get_type_def(hint_name) else {
        // This hint isn't defined, and we check that elsewhere, so give up unifying.
        return Ok(());
    };

    if hint.args.len() != type_def.params().len() {
        // The hint has the wrong number of type arguments for this
        // type, which is checked elsewhere, so give up unifying.
        return Ok(());
    }

    match ty {
        Type::UserDefined { name, args, .. } if name.name == hint_name.name => {
            // TODO: stop assuming that all types are covariant.
            for (hint_arg, arg) in hint.args.iter().zip(args) {
                unify_and_solve_hint(env, hint_arg, position, arg, ty_var_env)?;
            }
        }
        _ => {
            // This value is unrelated to this hint, no solving to do.
        }
    }

    Ok(())
}

/// Unify all the types given, to a single type, if we can find a
/// compatible type.
fn unify_all(tys: &[Type]) -> Option<Type> {
    let mut unified_ty = Type::no_value();

    // Unify the types pairwise.
    for ty in tys {
        unified_ty = unify(&unified_ty, ty)?;
    }

    Some(unified_ty)
}

/// If these two types are compatible, return the most general
/// compatible type.
///
/// (Int, NoValue) -> Int
/// (NoValue, Int) -> Int
/// (List<Int>, List<NoValue>) -> List<Int>
/// (In, String) -> return None
fn unify(ty_1: &Type, ty_2: &Type) -> Option<Type> {
    if ty_1.is_no_value() {
        return Some(ty_2.clone());
    }
    if ty_2.is_no_value() {
        return Some(ty_1.clone());
    }
    if ty_1 == ty_2 {
        return Some(ty_1.clone());
    }

    // TODO: recursively unify, to handle e.g. List<String> with List<NoValue>.

    None
}

fn check_match_exhaustive(
    env: &Env,
    scrutinee_pos: &Position,
    type_name: &TypeName,
    patterns: &[Pattern],
    warnings: &mut Vec<Diagnostic>,
) {
    let Some(type_def) = env.get_type_def(type_name) else {
        return;
    };
    let TypeDef::Enum(enum_info) = type_def else {
        return;
    };

    for pattern in patterns {
        if pattern.symbol.name.is_underscore() {
            return;
        }
    }

    let mut variants_remaining: HashMap<SymbolName, VariantInfo> = HashMap::new();
    for variant in &enum_info.variants {
        variants_remaining.insert(variant.name_sym.name.clone(), variant.clone());
    }

    for pattern in patterns {
        // If any cases are _, this match is exhaustive.
        if pattern.symbol.name.is_underscore() {
            return;
        }

        variants_remaining.remove(&pattern.symbol.name);
    }

    let missing: Vec<_> = variants_remaining.keys().collect();

    if let Some(missing_case) = missing.first() {
        warnings.push(Diagnostic {
            level: Level::Error,
            message: format!(
                "This match expression does not cover all the cases of `{}`. It's missing `{}`.",
                type_name, missing_case
            ),
            position: scrutinee_pos.clone(),
        });
    }
}
