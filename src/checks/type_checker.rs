use std::collections::HashMap;

use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, Expression, Expression_, FunInfo, MethodInfo, SymbolName,
    ToplevelItem, TypeHint, TypeName,
};
use garden_lang_parser::position::Position;

use crate::diagnostics::{Diagnostic, Level};
use crate::env::Env;
use crate::runtime_type::{is_subtype, RuntimeType, TypeDefKind};
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
        .unwrap_or(RuntimeType::Top);
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
                    .unwrap_or(RuntimeType::Top),
                None => RuntimeType::Top,
            };
            self.bindings.set(param.symbol.name.clone(), param_ty);
        }

        self.visit_fun_info_default(fun_info);

        self.bindings.exit_block();
    }

    fn visit_block(&mut self, block: &Block) {
        assign_expr_ids(block);

        // check_block recurses, so don't recurse in the visitor
        check_block(block, self.env, &mut self.bindings, &mut self.warnings);
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
) -> Option<RuntimeType> {
    bindings.enter_block();

    let mut ty = Some(RuntimeType::unit());
    for expr in &block.exprs {
        ty = check_expr(expr, env, bindings, warnings);
    }

    bindings.exit_block();
    ty
}

fn check_expr(
    expr: &Expression,
    env: &mut Env,
    bindings: &mut LocalBindings,
    warnings: &mut Vec<Diagnostic>,
) -> Option<RuntimeType> {
    match &expr.1 {
        Expression_::Match(scrutinee, cases) => {
            let scrutinee_ty = check_expr(scrutinee, env, bindings, warnings);
            let scrutinee_ty_name = scrutinee_ty.and_then(|ty| ty.type_name());

            for (pattern, case_expr) in cases {
                check_expr(case_expr, env, bindings, warnings);

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
            None
        }
        Expression_::If(cond_expr, then_block, else_block) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings);
            if let Some(cond_ty) = cond_ty {
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
            }

            let then_ty = check_block(then_block, env, bindings, warnings);

            // TODO: check if `then_block` and `else_block` are the same type.
            if let Some(else_block) = else_block {
                check_block(else_block, env, bindings, warnings);
            }
            then_ty
        }
        Expression_::While(cond_expr, body) => {
            let cond_ty = check_expr(cond_expr, env, bindings, warnings);
            if let Some(cond_ty) = cond_ty {
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
            }

            check_block(body, env, bindings, warnings);

            Some(RuntimeType::unit())
        }
        Expression_::Break => Some(RuntimeType::unit()),
        Expression_::Assign(_sym, expr) => check_expr(expr, env, bindings, warnings),
        Expression_::Let(sym, expr) => {
            let expr_ty = check_expr(expr, env, bindings, warnings);
            if let Some(expr_ty) = &expr_ty {
                bindings.set(sym.name.clone(), expr_ty.clone());
            }

            expr_ty
        }
        Expression_::Return(expr) => {
            if let Some(expr) = expr {
                check_expr(expr, env, bindings, warnings);
            }
            None
        }
        Expression_::IntLiteral(_) => Some(RuntimeType::Int),
        Expression_::StringLiteral(_) => Some(RuntimeType::String),
        Expression_::ListLiteral(items) => {
            let mut elem_ty = RuntimeType::no_value();

            for item in items {
                if let Some(item_ty) = check_expr(item, env, bindings, warnings) {
                    // TODO: unify the types of all elements in the
                    // list, rather than letting the last win.
                    elem_ty = item_ty;
                }
            }

            Some(RuntimeType::List(Box::new(elem_ty)))
        }
        Expression_::StructLiteral(name_sym, fields) => {
            for (_, expr) in fields {
                check_expr(expr, env, bindings, warnings);
            }

            if let Some(TypeDef::Struct(_)) = env.get_type_def(&name_sym.name) {
                Some(RuntimeType::UserDefined {
                    kind: TypeDefKind::Struct,
                    name: name_sym.name.clone(),
                    args: vec![],
                })
            } else {
                None
            }
        }
        Expression_::BinaryOperator(lhs, op, rhs) => {
            let lhs_ty = check_expr(lhs, env, bindings, warnings);
            let rhs_ty = check_expr(rhs, env, bindings, warnings);

            match op {
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Subtract
                | BinaryOperatorKind::Multiply
                | BinaryOperatorKind::Divide => {
                    if let Some(lhs_ty) = lhs_ty {
                        if !is_subtype(&lhs_ty, &RuntimeType::Int) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if !is_subtype(&rhs_ty, &RuntimeType::Int) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::Int)
                }
                BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessThanOrEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::GreaterThanOrEqual => {
                    if let Some(lhs_ty) = lhs_ty {
                        if !is_subtype(&lhs_ty, &RuntimeType::Int) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if !is_subtype(&rhs_ty, &RuntimeType::Int) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Int`, but got `{}`.", rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::bool())
                }
                BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                    if let (Some(lhs_ty), Some(rhs_ty)) = (lhs_ty, rhs_ty) {
                        if lhs_ty != rhs_ty {
                            warnings.push(Diagnostic { level: Level::Warning,
                                message: format!("Left hand side has type `{}`, but right hand side has type `{}`, so this will always have the same result.", lhs_ty, rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::bool())
                }
                BinaryOperatorKind::And | BinaryOperatorKind::Or => {
                    if let Some(lhs_ty) = lhs_ty {
                        if !is_subtype(&lhs_ty, &RuntimeType::bool()) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Bool`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if !is_subtype(&rhs_ty, &RuntimeType::bool()) {
                            warnings.push(Diagnostic {
                                level: Level::Error,
                                message: format!("Expected `Bool`, but got `{}`.", rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::bool())
                }
            }
        }
        Expression_::Variable(sym) => {
            if let Some(value_ty) = bindings.get(&sym.name) {
                return Some(value_ty.clone());
            }

            let value = env.file_scope.get(&sym.name)?;
            let value_ty = RuntimeType::from_value(value, env, &env.type_bindings());
            Some(value_ty)
        }
        Expression_::Call(recv, args) => {
            let recv_ty = check_expr(recv, env, bindings, warnings)?;
            let arg_tys = args
                .iter()
                .map(|arg| (check_expr(arg, env, bindings, warnings), arg.0.clone()))
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

                    for (param_ty, (arg_ty, arg_pos)) in params.iter().zip(arg_tys) {
                        let Some(arg_ty) = arg_ty else {
                            continue;
                        };

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

                    Some(*return_)
                }
                _ => {
                    warnings.push(Diagnostic {
                        level: Level::Error,
                        message: format!("Expected a function, but got a `{}`.", recv_ty),
                        position: recv.0.clone(),
                    });

                    None
                }
            }
        }
        Expression_::MethodCall(recv, sym, args) => {
            let arg_tys: Vec<(Option<RuntimeType>, Position)> = args
                .iter()
                .map(|arg| (check_expr(arg, env, bindings, warnings), arg.0.clone()))
                .collect::<Vec<_>>();

            let receiver_ty = check_expr(recv, env, bindings, warnings)?;
            let receiver_ty_name = receiver_ty.type_name()?;

            let methods = env
                .methods
                .get(&receiver_ty_name)
                .cloned()
                .unwrap_or_default();

            match methods.get(&sym.name) {
                Some(method_info) => {
                    let fun_info = method_info.fun_info()?;
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
                        let Some(arg_ty) = arg_ty else {
                            continue;
                        };
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
                    None
                }
            }
        }
        Expression_::DotAccess(recv, field_sym) => {
            let recv_ty = check_expr(recv, env, bindings, warnings)?;
            let recv_ty_name = recv_ty.type_name()?;

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
                                        .unwrap_or(RuntimeType::Top);
                                return Some(field_ty);
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
                        None
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        Expression_::FunLiteral(fun_info) => {
            // Check the function body with the locals bound.
            bindings.enter_block();

            // Only bind and check locals that have an explicit type
            // hint.
            for param in &fun_info.params {
                if let Some(hint) = &param.hint {
                    let param_ty = RuntimeType::from_hint(hint, env, &env.type_bindings())
                        .unwrap_or(RuntimeType::Top);
                    bindings.set(param.symbol.name.clone(), param_ty);
                }
            }

            let body_ty = check_block(&fun_info.body, env, bindings, warnings);

            bindings.exit_block();

            // Build the type representation of a function matching this lambda.
            let mut param_tys = vec![];
            for param in &fun_info.params {
                let param_ty = match &param.hint {
                    Some(hint) => RuntimeType::from_hint(hint, env, &env.type_bindings())
                        .unwrap_or(RuntimeType::Top),
                    None => RuntimeType::Top,
                };
                param_tys.push(param_ty);
            }

            let return_ty = match &fun_info.return_hint {
                Some(hint) => RuntimeType::from_hint(hint, env, &env.type_bindings())
                    .unwrap_or(RuntimeType::Top),
                None => body_ty.unwrap_or(RuntimeType::Top),
            };

            let fun_ty = RuntimeType::Fun {
                type_params: vec![],
                params: param_tys,
                return_: Box::new(return_ty),
            };
            Some(fun_ty)
        }
        Expression_::Block(block) => check_block(block, env, bindings, warnings),
    }
}

/// Solve the type variables in this method, and return the resolved
/// type of the return type hint.
fn subst_type_vars_in_meth_return_ty(
    env: &Env,
    method_info: &MethodInfo,
    receiver_pos: &Position,
    receiver_ty: &RuntimeType,
    arg_tys: &[(Option<RuntimeType>, Position)],
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) -> (Vec<Diagnostic>, Option<RuntimeType>) {
    let mut diagnostics = vec![];
    let Some(fun_info) = method_info.fun_info() else {
        return (diagnostics, None);
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
    arg_tys: &[(Option<RuntimeType>, Position)],
    ty_var_env: &mut HashMap<TypeName, Option<RuntimeType>>,
) -> (Vec<Diagnostic>, Option<RuntimeType>) {
    let mut diagnostics = vec![];

    for ((arg_ty, arg_pos), param) in arg_tys.iter().zip(&fun_info.params) {
        if let (Some(param_hint), Some(arg_ty)) = (&param.hint, arg_ty) {
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
                RuntimeType::from_hint(return_hint, env, &env.type_bindings())
                    .unwrap_or(RuntimeType::Top)
            };

            (diagnostics, Some(ty))
        }
        None => (diagnostics, None),
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
                            hint_name,
                            bound_ty.type_name_friendly(),
                            ty.type_name_friendly(),
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
