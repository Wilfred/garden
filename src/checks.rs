use std::collections::{HashMap, HashSet};

use crate::diagnostics::Warning;
use crate::env::Env;
use crate::eval::eval_defs;
use crate::types::Type;
use garden_lang_parser::ast::{
    Block, Definition, Definition_, Expression, Expression_, FunInfo, MethodKind, Symbol,
    SymbolName, TypeHint,
};
use garden_lang_parser::position::Position;

pub(crate) fn check_defs(definitions: &[Definition]) -> Vec<Warning> {
    // TODO: define separate checks for things we can check without an
    // environment, and checks that are relative to a given environment
    // (e.g. type is defined).
    let mut env = Env::default();
    eval_defs(definitions, &mut env);

    let mut warnings = vec![];
    for def in definitions {
        warnings.extend(check_def(def, &env));
    }

    warnings
}

pub(crate) fn check_def(def: &Definition, env: &Env) -> Vec<Warning> {
    match &def.2 {
        Definition_::Fun(_, fun_info) => check(fun_info, env),
        Definition_::Method(meth_info) => {
            let fun_info = match &meth_info.kind {
                MethodKind::BuiltinMethod(_, fun_info) => fun_info.as_ref(),
                MethodKind::UserDefinedMethod(fun_info) => Some(fun_info),
            };

            if let Some(fun_info) = fun_info {
                check(fun_info, env)
            } else {
                vec![]
            }
        }
        Definition_::Test(test_info) => check_free_variables_block(&test_info.body, env),
        Definition_::Enum(_) => vec![],
        Definition_::Struct(_) => vec![],
    }
}

fn check(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    warnings.extend(check_types_exist(fun_info, env));
    warnings.extend(check_free_variables(fun_info, env));

    warnings
}

fn check_types_exist(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    // TODO: check type arity too.
    let mut warnings = vec![];

    for param in &fun_info.params {
        if let Some(param_hint) = &param.type_ {
            warnings.extend(check_type_hint(param_hint, env));
        }
    }

    if let Some(return_hint) = &fun_info.return_type {
        warnings.extend(check_type_hint(return_hint, env));
    }

    warnings
}

fn check_type_hint(type_hint: &TypeHint, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    match env.get_type(&type_hint.sym.name) {
        Some(type_) => {
            match type_ {
                Type::Builtin(_) => {
                    // TODO: check type arguments on built-in types
                }
                Type::Enum(enum_info) => {
                    if enum_info.type_params.len() != type_hint.args.len() {
                        warnings.push(Warning {
                            message: format!(
                                "{} takes {} type arguments, but got {} arguments.",
                                &type_hint.sym.name,
                                enum_info.type_params.len(),
                                type_hint.args.len()
                            ),
                            position: type_hint.position.clone(),
                        });
                    }
                }
                Type::Struct(_) => {
                    // TODO: check type arguments on enums.
                }
            }
        }
        None => {
            warnings.push(Warning {
                message: format!("No such type: {}", &type_hint.sym),
                position: type_hint.position.clone(),
            });
        }
    }

    for type_arg in &type_hint.args {
        warnings.extend(check_type_hint(type_arg, env));
    }

    warnings
}

fn check_free_variables(fun_info: &FunInfo, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    let mut info = VarInfo::default();
    free_variable_fun(fun_info, &mut info, env);

    for (free_sym, position) in info.free {
        warnings.push(Warning {
            message: format!("Unbound symbol: {free_sym}"),
            position,
        });
    }

    warnings
}

fn check_free_variables_block(block: &Block, env: &Env) -> Vec<Warning> {
    let mut warnings = vec![];

    let mut info = VarInfo::default();
    free_variable_block(block, &mut info, env);

    for (free_sym, position) in info.free {
        warnings.push(Warning {
            message: format!("Unbound symbol: {free_sym}"),
            position,
        });
    }

    warnings
}

struct VarInfo {
    bound_scopes: Vec<HashSet<SymbolName>>,
    free: HashMap<SymbolName, Position>,
}

impl Default for VarInfo {
    fn default() -> Self {
        VarInfo {
            bound_scopes: vec![HashSet::new()],
            free: HashMap::new(),
        }
    }
}

impl VarInfo {
    fn is_bound(&self, name: &SymbolName) -> bool {
        for scope in &self.bound_scopes {
            if scope.contains(name) {
                return true;
            }
        }

        false
    }

    fn add_binding(&mut self, name: &SymbolName) {
        let scope = self
            .bound_scopes
            .last_mut()
            .expect("Should always be non-empty");
        scope.insert(name.clone());
    }

    fn push_scope(&mut self) {
        self.bound_scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.bound_scopes.pop();
    }
}

fn free_variable_exprs(exprs: &[Expression], info: &mut VarInfo, env: &Env) {
    for expr in exprs {
        free_variable_expr(expr, info, env);
    }
}

fn free_variable_block(block: &Block, info: &mut VarInfo, env: &Env) {
    let Block { exprs, .. } = block;
    info.push_scope();
    free_variable_exprs(exprs, info, env);
    info.pop_scope();
}

fn free_variable_expr(expr: &Expression, info: &mut VarInfo, env: &Env) {
    // TODO: Implement a visitor in the pattern of
    // https://www.reddit.com/r/rust/comments/11q7l8m/best_practices_for_ast_design_in_rust/
    match &expr.1 {
        Expression_::Match(scrutinee, cases) => {
            free_variable_expr(scrutinee, info, env);
            for (pattern, case_expr) in cases {
                // TODO: add a check that there's an enum with this
                // variant, and that we've covered all the variants.

                info.push_scope();
                if let Some(pattern_arg) = &pattern.argument {
                    info.add_binding(&pattern_arg.name);
                }

                free_variable_expr(case_expr, info, env);
                info.pop_scope();
            }
        }
        Expression_::If(cond, then_block, else_block) => {
            free_variable_expr(cond, info, env);
            free_variable_block(then_block, info, env);
            if let Some(else_block) = else_block {
                free_variable_block(else_block, info, env);
            }
        }
        Expression_::While(cond, body) => {
            free_variable_expr(cond, info, env);
            free_variable_block(body, info, env);
        }
        Expression_::Assign(symbol, expr) => {
            free_variable_symbol(symbol, info, env);
            free_variable_expr(expr, info, env);
        }
        Expression_::Let(symbol, expr) => {
            free_variable_expr(expr, info, env);
            info.add_binding(&symbol.name);
        }
        Expression_::Return(expr) => {
            if let Some(expr) = expr {
                free_variable_expr(expr, info, env)
            };
        }
        Expression_::IntLiteral(_) => {}
        Expression_::StringLiteral(_) => {}
        Expression_::ListLiteral(exprs) => {
            free_variable_exprs(exprs, info, env);
        }
        Expression_::StructLiteral(_, key_values) => {
            for (_, value) in key_values {
                free_variable_expr(value, info, env);
            }
        }
        Expression_::BinaryOperator(lhs, _op, rhs) => {
            free_variable_expr(lhs, info, env);
            free_variable_expr(rhs, info, env);
        }
        Expression_::Variable(symbol) => free_variable_symbol(symbol, info, env),
        Expression_::Call(recv, args) => {
            free_variable_expr(recv, info, env);
            for arg in args {
                free_variable_expr(arg, info, env);
            }
        }
        Expression_::MethodCall(recv, _meth_name, args) => {
            free_variable_expr(recv, info, env);
            for arg in args {
                free_variable_expr(arg, info, env);
            }
        }
        Expression_::FunLiteral(fun_info) => free_variable_fun(fun_info, info, env),
        Expression_::Block(block) => free_variable_block(block, info, env),
    }
}

fn free_variable_fun(fun_info: &FunInfo, info: &mut VarInfo, env: &Env) {
    info.push_scope();
    for param in &fun_info.params {
        info.add_binding(&param.symbol.name);
    }

    // Function literals can close over values, so it's just another
    // nested scope, like any other block.
    free_variable_block(&fun_info.body, info, env);

    info.pop_scope();
}

fn free_variable_symbol(symbol: &Symbol, info: &mut VarInfo, env: &Env) {
    if !info.is_bound(&symbol.name) && !env.file_scope.contains_key(&symbol.name) {
        // Variable is free.
        if !info.free.contains_key(&symbol.name) {
            // Only record the first occurrence as free.
            info.free
                .insert(symbol.name.clone(), symbol.position.clone());
        }
    }
}
