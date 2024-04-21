use garden_lang_parser::ast::{
    BinaryOperatorKind, Block, Expression, Expression_, FunInfo, ToplevelItem,
};

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::Bindings;
use crate::runtime_type::RuntimeType;
use crate::visitor::Visitor;

pub(crate) fn check_types(items: &[ToplevelItem], env: &mut Env) -> Vec<Warning> {
    let mut visitor = TypeCheckVisitor {
        env,
        warnings: vec![],
    };
    for item in items {
        visitor.visit_toplevel_item(item);
    }

    visitor.warnings
}

#[derive(Debug)]
struct TypeCheckVisitor<'a> {
    env: &'a mut Env,
    warnings: Vec<Warning>,
}

impl Visitor for TypeCheckVisitor<'_> {
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

        self.visit_fun_info_default(fun_info);
    }

    fn visit_block(&mut self, block: &Block) {
        assign_expr_ids(block);

        let mut bindings = Bindings::default();

        // check_block recurses, so don't recurse in the visitor
        check_block(block, self.env, &mut bindings, &mut self.warnings);
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

fn check_block(block: &Block, env: &mut Env, bindings: &mut Bindings, warnings: &mut Vec<Warning>) {
    bindings.push_block();

    for expr in &block.exprs {
        check_expr(expr, env, bindings, warnings);
    }

    bindings.pop_block();
}

fn check_expr(
    expr: &Expression,
    env: &mut Env,
    bindings: &mut Bindings,
    warnings: &mut Vec<Warning>,
) -> Option<RuntimeType> {
    match &expr.1 {
        Expression_::Match(_, _) => None,
        Expression_::If(_, _, _) => None,
        Expression_::While(_, _) => None,
        Expression_::Assign(_sym, expr) => check_expr(expr, env, bindings, warnings),
        Expression_::Let(_sym, expr) => check_expr(expr, env, bindings, warnings),
        Expression_::Return(_) => None,
        Expression_::IntLiteral(_) => Some(RuntimeType::Int),
        Expression_::StringLiteral(_) => Some(RuntimeType::String),
        Expression_::ListLiteral(items) => {
            for item in items {
                check_expr(item, env, bindings, warnings);
            }

            // TODO: accurately calculate list generic type argument.
            Some(RuntimeType::List(Box::new(RuntimeType::Top)))
        }
        Expression_::StructLiteral(_, _) => None,
        Expression_::BinaryOperator(lhs, op, rhs) => {
            let lhs_ty = check_expr(lhs, env, bindings, warnings);
            let rhs_ty = check_expr(rhs, env, bindings, warnings);

            match op {
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Subtract
                | BinaryOperatorKind::Multiply
                | BinaryOperatorKind::Divide => {
                    if let Some(lhs_ty) = lhs_ty {
                        if lhs_ty != RuntimeType::Int {
                            warnings.push(Warning {
                                message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if rhs_ty != RuntimeType::Int {
                            warnings.push(Warning {
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
                        if lhs_ty != RuntimeType::Int {
                            warnings.push(Warning {
                                message: format!("Expected `Int`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if rhs_ty != RuntimeType::Int {
                            warnings.push(Warning {
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
                            warnings.push(Warning {
                                message: format!("Left hand side has type `{}`, but right hand side has type `{}`, so this will always have the same result.", lhs_ty, rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::bool())
                }
                BinaryOperatorKind::And | BinaryOperatorKind::Or => {
                    if let Some(lhs_ty) = lhs_ty {
                        if lhs_ty != RuntimeType::bool() {
                            warnings.push(Warning {
                                message: format!("Expected `Bool`, but got `{}`.", lhs_ty),
                                position: lhs.0.clone(),
                            });
                        }
                    }
                    if let Some(rhs_ty) = rhs_ty {
                        if rhs_ty != RuntimeType::bool() {
                            warnings.push(Warning {
                                message: format!("Expected `Bool`, but got `{}`.", rhs_ty),
                                position: rhs.0.clone(),
                            });
                        }
                    }

                    Some(RuntimeType::bool())
                }
            }
        }
        Expression_::Variable(_) => None,
        Expression_::Call(_, _) => None,
        Expression_::MethodCall(recv, sym, args) => {
            for arg in args {
                check_expr(arg, env, bindings, warnings);
            }

            let recv_ty = check_expr(recv, env, bindings, warnings)?;
            let recv_ty_name = recv_ty.type_name()?;

            let methods = env.methods.get(&recv_ty_name)?;

            match methods.get(&sym.name) {
                Some(method_info) => {
                    let fun_info = method_info.fun_info()?;
                    if fun_info.params.len() != args.len() {
                        warnings.push(Warning {
                            message: format!(
                                "`{}::{}` requires {} arguments, but got {}.",
                                recv_ty_name,
                                sym.name,
                                fun_info.params.len(),
                                args.len()
                            ),
                            position: sym.position.clone(),
                        });
                    }
                    None
                }
                None => {
                    warnings.push(Warning {
                        message: format!("`{}` has no method `{}`.", recv_ty_name, sym.name),
                        position: sym.position.clone(),
                    });
                    None
                }
            }
        }
        Expression_::DotAccess(_, _) => None,
        Expression_::FunLiteral(_) => None,
        Expression_::Block(block) => {
            check_block(block, env, bindings, warnings);
            None
        }
    }
}
