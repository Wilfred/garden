use std::collections::HashMap;

use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, Expression, Expression_, FunInfo, MethodInfo, SymbolName,
    ToplevelItem, TypeHint, TypeName,
};
use garden_lang_parser::position::Position;

use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::runtime_type::{is_subtype, RuntimeType, TypeDefKind, UnwrapOrErrTy as _};
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
    blocks: Vec<HashMap<SymbolName, RuntimeType>>,
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

    fn get(&self, name: &SymbolName) -> Option<&RuntimeType> {
        for block in self.blocks.iter().rev() {
            if let Some(ty) = block.get(name) {
                return Some(ty);
            }
        }

        None
    }

    fn set(&mut self, name: SymbolName, ty: RuntimeType) {
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

        let self_ty = RuntimeType::from_hint(
            &method_info.receiver_hint,
            self.env,
            &self.env.type_bindings(),
        )
        .unwrap_or_err_ty();
        self.bindings
            .set(method_info.receiver_sym.name.clone(), self_ty);

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
                Some(hint) => RuntimeType::from_hint(hint, self.env, &self.env.type_bindings())
                    .unwrap_or_err_ty(),
                None => RuntimeType::Top,
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
        expr.2
            .set(self.next_id)
            .expect("Expressions should not have IDs yet.");
        self.next_id += 1;

        self.visit_expr_(&expr.1)
    }
}

fn check_block(
    block: &Block,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
    expected_return_ty: Option<&RuntimeType>,
) -> RuntimeType {
    bindings.enter_block();

    let mut ty = RuntimeType::unit();
    for expr in &block.exprs {
        ty = check_expr(expr, env, bindings, warnings, expected_return_ty);
    }

    bindings.exit_block();
    ty
}

fn check_expr(
    expr: &Expression,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
    expected_return_ty: Option<&RuntimeType>,
) -> RuntimeType {
    match &expr.1 {
        Expression_::Match(scrutinee, cases) => {
            let scrutinee_ty = check_expr(scrutinee, env, bindings, warnings, expected_return_ty);
            let scrutinee_ty_name = scrutinee_ty.type_name();

            for (pattern, case_expr) in cases {
                check_expr(case_expr, env, bindings, warnings, expected_return_ty);

                // Matching `_` works for any type.
                if pattern.symbol.name.is_underscore() {
                    continue;
                }

                let Some(value) = env.file_scope.get(&pattern.symbol.name) else {
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

            RuntimeType::Error("TODO: return types from match expressions".to_owned())
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings, expected_return_ty);
            if !is_subtype(&cond_ty, &RuntimeType::bool()) {
                warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!(
                        "Expected `Bool` inside an `if` condition, but got `{}`.",
                        cond_ty
                    ),
                    position: cond_expr.0.clone(),
                });
            }

            let then_ty = check_block(then_block, env, bindings, warnings, expected_return_ty);

            // TODO: check if `then_block` and `else_block` are the same type.
            if let Some(else_block) = else_block {
                check_block(else_block, env, bindings, warnings, expected_return_ty);
            }
            then_ty
        }
        Expression_::While(cond_expr, body) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings, expected_return_ty);
            if !is_subtype(&cond_ty, &RuntimeType::bool()) {
                warnings.push(Diagnostic {
                    level: Level::Error,
                    message: format!(
                        "Expected `Bool` inside an `while` condition, but got `{}`.",
                        cond_ty
                    ),
                    position: cond_expr.0.clone(),
                });
            }

            check_block(body, env, bindings, warnings, expected_return_ty);

            RuntimeType::unit()
        }
        Expression_::Break => RuntimeType::unit(),
        Expression_::Assign(_sym, expr) => {
            check_expr(expr, env, bindings, warnings, expected_return_ty)
        }
        Expression_::Let(sym, hint, expr) => {
            let expr_ty = check_expr(expr, env, bindings, warnings, expected_return_ty);

            let ty = match hint {
                Some(hint) => {
                    let hint_ty =
                        RuntimeType::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();

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

            RuntimeType::unit()
        }
        Expression_::Return(expr) => {
            if let Some(expr) = expr {
                let ty = check_expr(expr, env, bindings, warnings, expected_return_ty);

                if let Some(expected_return_ty) = expected_return_ty {
                    if !is_subtype(&ty, expected_return_ty) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "Expected this function to return `{}`, but got `{}`.",
                                expected_return_ty, ty
                            ),
                            position: expr.0.clone(),
                        });
                    }
                }

                ty
            } else {
                RuntimeType::unit()
            }
        }
        Expression_::IntLiteral(_) => RuntimeType::Int,
        Expression_::StringLiteral(_) => RuntimeType::String,
        Expression_::ListLiteral(items) => {
            let mut elem_ty = RuntimeType::no_value();

            for item in items {
                let item_ty = check_expr(item, env, bindings, warnings, expected_return_ty);
                // TODO: unify the types of all elements in the
                // list, rather than letting the last win.
                elem_ty = item_ty;
            }

            RuntimeType::List(Box::new(elem_ty))
        }
        Expression_::StructLiteral(name_sym, fields) => {
            for (_, expr) in fields {
                check_expr(expr, env, bindings, warnings, expected_return_ty);
            }

            if let Some(TypeDef::Struct(_)) = env.get_type_def(&name_sym.name) {
                RuntimeType::UserDefined {
                    kind: TypeDefKind::Struct,
                    name: name_sym.name.clone(),
                    args: vec![],
                }
            } else {
                RuntimeType::Error("Unbound struct name".to_owned())
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
                    if !is_subtype(&lhs_ty, &RuntimeType::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                            position: lhs.0.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &RuntimeType::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                            position: rhs.0.clone(),
                        });
                    }

                    RuntimeType::Int
                }
                BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessThanOrEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::GreaterThanOrEqual => {
                    if !is_subtype(&lhs_ty, &RuntimeType::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                            position: lhs.0.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &RuntimeType::Int) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                            position: rhs.0.clone(),
                        });
                    }

                    RuntimeType::bool()
                }
                BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                    if lhs_ty != rhs_ty {
                        warnings.push(Diagnostic { level: Level::Warning,
                                message: format!("Left hand side has type `{}`, but right hand side has type `{}`, so this will always have the same result.", lhs_ty, rhs_ty),
                                position: rhs.0.clone(),
                            });
                    }

                    RuntimeType::bool()
                }
                BinaryOperatorKind::And | BinaryOperatorKind::Or => {
                    if !is_subtype(&lhs_ty, &RuntimeType::bool()) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Bool`, but got `{}`.", lhs_ty),
                            position: lhs.0.clone(),
                        });
                    }
                    if !is_subtype(&rhs_ty, &RuntimeType::bool()) {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!("Expected `Bool`, but got `{}`.", rhs_ty),
                            position: rhs.0.clone(),
                        });
                    }

                    RuntimeType::bool()
                }
            }
        }
        Expression_::Variable(sym) => {
            if let Some(value_ty) = bindings.get(&sym.name) {
                return value_ty.clone();
            }

            match env.file_scope.get(&sym.name) {
                Some(value) => RuntimeType::from_value(value, env, &env.type_bindings()),
                None => RuntimeType::Error("Unbound variable".to_owned()),
            }
        }
        Expression_::Call(recv, args) => {
            let recv_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let arg_tys = args
                .iter()
                .map(|arg| {
                    (
                        check_expr(arg, env, bindings, warnings, expected_return_ty),
                        arg.0.clone(),
                    )
                })
                .collect::<Vec<_>>();

            match recv_ty {
                RuntimeType::Fun {
                    params, return_, ..
                } => {
                    if params.len() != args.len() {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "This function expects {} argument{}, but got {}.",
                                params.len(),
                                if params.len() == 1 { "" } else { "s" },
                                args.len()
                            ),
                            position: recv.0.clone(),
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

                    *return_
                }
                _ => {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!("Expected a function, but got a `{}`.", recv_ty),
                        position: recv.0.clone(),
                    });

                    RuntimeType::Error("Calling something that isn't a function".to_owned())
                }
            }
        }
        Expression_::MethodCall(recv, sym, args) => {
            let arg_tys: Vec<(RuntimeType, Position)> = args
                .iter()
                .map(|arg| {
                    (
                        check_expr(arg, env, bindings, warnings, expected_return_ty),
                        arg.0.clone(),
                    )
                })
                .collect::<Vec<_>>();

            let receiver_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let Some(receiver_ty_name) = receiver_ty.type_name() else {
                return RuntimeType::error("No type name for this method receiver");
            };

            let methods = env
                .methods
                .get(&receiver_ty_name)
                .cloned()
                .unwrap_or_default();

            match methods.get(&sym.name) {
                Some(method_info) => {
                    let Some(fun_info) = method_info.fun_info() else {
                        return RuntimeType::error("This method has no fun_info");
                    };
                    if fun_info.params.len() != args.len() {
                        warnings.push(Diagnostic {
                            level: Level::Error,
                            message: format!(
                                "`{}::{}` requires {} argument{}, but got {}.",
                                receiver_ty_name,
                                sym.name,
                                fun_info.params.len(),
                                if fun_info.params.len() == 1 { "" } else { "s" },
                                args.len()
                            ),
                            position: sym.position.clone(),
                        });
                    }

                    for (param, (arg_ty, arg_pos)) in fun_info.params.iter().zip(&arg_tys) {
                        let Some(param_hint) = &param.hint else {
                            continue;
                        };
                        let Ok(param_ty) =
                            RuntimeType::from_hint(param_hint, env, &env.type_bindings())
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

                    let mut ty_var_env: HashMap<TypeName, Option<RuntimeType>> = HashMap::default();
                    for type_param in &fun_info.type_params {
                        ty_var_env.insert(type_param.name.clone(), None);
                    }

                    let (more_warnings, ret_ty) = subst_type_vars_in_meth_return_ty(
                        env,
                        method_info,
                        &recv.0,
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
                    RuntimeType::error("No such method on this type")
                }
            }
        }
        Expression_::DotAccess(recv, field_sym) => {
            let recv_ty = check_expr(recv, env, bindings, warnings, expected_return_ty);
            let Some(recv_ty_name) = recv_ty.type_name() else {
                return RuntimeType::error("No type name found this receiver");
            };

            match recv_ty {
                RuntimeType::UserDefined {
                    kind: TypeDefKind::Struct,
                    name,
                    ..
                } => {
                    if let Some(TypeDef::Struct(struct_info)) = env.get_type_def(&name) {
                        for field in &struct_info.fields {
                            if field.sym.name == field_sym.name {
                                let field_ty =
                                    RuntimeType::from_hint(&field.hint, env, &env.type_bindings())
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

                        RuntimeType::error("No struct field with this name")
                    } else {
                        RuntimeType::error("No struct with this name")
                    }
                }
                _ => RuntimeType::error("This type is not a struct"),
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
) -> RuntimeType {
    // Check the function body with the locals bound.
    bindings.enter_block();

    // Only bind and check locals that have an explicit type
    // hint.
    for param in &fun_info.params {
        if let Some(hint) = &param.hint {
            let param_ty =
                RuntimeType::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();
            bindings.set(param.symbol.name.clone(), param_ty);
        }
    }

    let expected_return_ty = match &fun_info.return_hint {
        Some(hint) => {
            let ty = RuntimeType::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();
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
            Some(hint) => {
                RuntimeType::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty()
            }
            None => RuntimeType::Top,
        };
        param_tys.push(param_ty);
    }

    let return_ty = match &fun_info.return_hint {
        Some(hint) => {
            let return_ty =
                RuntimeType::from_hint(hint, env, &env.type_bindings()).unwrap_or_err_ty();

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

    RuntimeType::Fun {
        type_params: vec![],
        params: param_tys,
        return_: Box::new(return_ty),
    }
}

/// Solve the type variables in this method, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_meth_return_ty(
    env: &Env,
    method_info: &MethodInfo,
    receiver_pos: &Position,
    receiver_ty: &RuntimeType,
    arg_tys: &[(RuntimeType, Position)],
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) -> (Vec<Diagnostic>, RuntimeType) {
    let mut diagnostics = vec![];
    let Some(fun_info) = method_info.fun_info() else {
        return (
            diagnostics,
            RuntimeType::error("This method has no fun_info"),
        );
    };

    if let Err(diagnostic) = unify_and_solve_hint(
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
    arg_tys: &[(RuntimeType, Position)],
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) -> (Vec<Diagnostic>, RuntimeType) {
    let mut diagnostics = vec![];

    for ((arg_ty, arg_pos), param) in arg_tys.iter().zip(&fun_info.params) {
        if let Some(param_hint) = &param.hint {
            if let Err(diagnostic) = unify_and_solve_hint(param_hint, arg_pos, arg_ty, ty_var_env) {
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
                        RuntimeType::no_value()
                    }
                }
            } else {
                RuntimeType::from_hint(return_hint, env, &env.type_bindings()).unwrap_or_err_ty()
            };

            (diagnostics, ty)
        }
        None => (diagnostics, RuntimeType::Top),
    }
}

fn subst_ty_vars(
    ty: &RuntimeType,
    ty_var_env: &HashMap<TypeName, Option<RuntimeType>>,
) -> RuntimeType {
    match ty {
        RuntimeType::Error(_) | RuntimeType::Top | RuntimeType::String | RuntimeType::Int => {
            ty.clone()
        }
        RuntimeType::List(elem_ty) => {
            RuntimeType::List(Box::new(subst_ty_vars(elem_ty, ty_var_env)))
        }
        RuntimeType::Fun {
            type_params,
            params,
            return_,
        } => {
            let params = params
                .iter()
                .map(|p| subst_ty_vars(p, ty_var_env))
                .collect();
            let return_ = subst_ty_vars(return_, ty_var_env);

            RuntimeType::Fun {
                type_params: type_params.clone(),
                params,
                return_: Box::new(return_),
            }
        }
        RuntimeType::UserDefined { kind, name, args } => RuntimeType::UserDefined {
            kind: kind.clone(),
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| subst_ty_vars(arg, ty_var_env))
                .collect(),
        },
        RuntimeType::TypeParameter(name) => {
            match ty_var_env.get(name) {
                Some(ty_var_val) => ty_var_val
                    .as_ref()
                    .cloned()
                    .unwrap_or(RuntimeType::no_value()),
                None => ty.clone(), // TODO: define an error type
            }
        }
    }
}

fn unify_and_solve_ty(
    expected_ty: &RuntimeType,
    actual_ty: &RuntimeType,
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) {
    match (expected_ty, actual_ty) {
        (RuntimeType::TypeParameter(n), _) => {
            ty_var_env.insert(n.clone(), Some(actual_ty.clone()));
        }
        (RuntimeType::List(expected_elem_ty), RuntimeType::List(actual_elem_ty)) => {
            unify_and_solve_ty(expected_elem_ty, actual_elem_ty, ty_var_env)
        }
        (
            RuntimeType::Fun {
                params: expected_params,
                return_: expected_return,
                ..
            },
            RuntimeType::Fun {
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
            RuntimeType::UserDefined {
                kind: expected_kind,
                name: expected_name,
                args: expected_args,
            },
            RuntimeType::UserDefined {
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
    hint: &TypeHint,
    position: &Position,
    ty: &RuntimeType,
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) -> Result<(), Diagnostic> {
    let hint_name = &hint.sym.name;
    if let Some(ty_var_val) = ty_var_env.get(hint_name) {
        // If the type named in this hint is a generic type, we're done.
        match ty_var_val {
            Some(bound_ty) => {
                if !is_subtype(ty, bound_ty) {
                    return Err(Diagnostic {
                        message: format!(
                            "Type is not compatible with `{}` is `{}` but got `{}`.",
                            hint_name, bound_ty, ty,
                        ),
                        position: position.clone(),
                        level: Level::Warning,
                    });
                }
            }
            None => {
                ty_var_env.insert(hint_name.clone(), Some(ty.clone()));
                return Ok(());
            }
        }
    }

    if hint_name.name == "List" && hint.args.len() == 1 {
        match ty {
            RuntimeType::List(elem_ty) => {
                return unify_and_solve_hint(&hint.args[0], position, elem_ty, ty_var_env);
            }
            _ => {
                // No solving to do.
                return Ok(());
            }
        }
    }
    if hint_name.name == "Option" && hint.args.len() == 1 {
        match ty {
            RuntimeType::UserDefined { name, args, .. }
                if name.name == "Option" && args.len() == 1 =>
            {
                return unify_and_solve_hint(&hint.args[0], position, &args[0], ty_var_env);
            }
            _ => {
                // No solving to do.
                return Ok(());
            }
        }
    }

    Ok(())
}
