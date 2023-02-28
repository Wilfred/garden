use std::{collections::HashMap, fmt::Display};

use crate::parse::{lex, parse_expression, Expression, Expression_, Statement_, VariableName};
use crate::parse::{parse_toplevel, Statement};
use crate::prompt::prompt_symbol;

use owo_colors::OwoColorize;
use rustyline::Editor;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(VariableName, Vec<VariableName>, Vec<Statement>),
    BuiltinFunction(BuiltinFunctionKind),
    String(String),
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinFunctionKind {
    Print,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(name, _, _) => write!(f, "(function: {})", name.0),
            Value::BuiltinFunction(BuiltinFunctionKind::Print) => write!(f, "(function: print)"),
            Value::Void => write!(f, "void"),
            // TODO: escape inner double quotes.
            Value::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    pub file_scope: HashMap<VariableName, Value>,
    pub fun_scopes: Vec<(VariableName, HashMap<VariableName, Value>)>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope = HashMap::new();
        file_scope.insert(
            VariableName("print".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::Print),
        );

        Self {
            file_scope,
            fun_scopes: vec![(VariableName("toplevel".into()), HashMap::new())],
        }
    }
}

impl Env {
    pub fn get(&self, name: &VariableName) -> Option<Value> {
        if let Some((_, fun_scope)) = self.fun_scopes.last() {
            if let Some(value) = fun_scope.get(name) {
                return Some(value.clone());
            }
        }

        if let Some(value) = self.file_scope.get(name) {
            return Some(value.clone());
        }

        None
    }

    pub fn push_new_fun_scope(&mut self, description: &VariableName) {
        self.fun_scopes.push((description.clone(), HashMap::new()));
    }

    pub fn pop_fun_scope(&mut self) {
        self.fun_scopes.pop().unwrap();
    }

    pub fn set_with_file_scope(&mut self, name: &VariableName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }

    pub fn set_with_fun_scope(&mut self, name: &VariableName, value: Value) {
        let (_, fun_scope) = &mut self.fun_scopes.last_mut().unwrap();
        fun_scope.insert(name.clone(), value);
    }

    pub fn fun_scope_has_var(&self, name: &VariableName) -> bool {
        let (_, fun_scope) = self.fun_scopes.last().unwrap();
        fun_scope.contains_key(name)
    }
}

fn error_prompt(message: &str) -> Result<Statement, String> {
    println!("{}: {}", "Error".bright_red(), message);
    println!("What value would you like to use instead?");

    let mut rl: Editor<()> = Editor::new().unwrap();
    match rl.readline(&prompt_symbol(1)) {
        Ok(input) => {
            let input = input.trim().to_string();
            let tokens = lex(input.trim())?;
            let mut token_ptr = &tokens[..];

            let mut asts: Vec<Statement> = parse_toplevel(&mut token_ptr)?;
            if asts.len() != 1 {
                return Err(format!(
                    "Expected to read a single statement, got {} items",
                    asts.len()
                ));
            }

            Ok(asts.pop().unwrap())
        }
        Err(e) => Err(format!("Input failed: {}", e)),
    }
}

pub fn eval_stmts(stmts: &[Statement], env: &mut Env) -> Result<Value, String> {
    let mut stmts_to_eval = vec![];
    for stmt in stmts.iter().rev() {
        stmts_to_eval.push((false, stmt.clone()));
    }

    let mut stmts_to_eval_per_fun: Vec<Vec<(bool, Statement)>> = vec![stmts_to_eval];
    let mut evalled_values: Vec<Value> = vec![Value::Void];

    loop {
        if let Some(mut stmts_to_eval) = stmts_to_eval_per_fun.pop() {
            while let Some((done_children, Statement(offset, stmt_))) = stmts_to_eval.pop() {
                let stmt_copy = stmt_.clone();
                match stmt_ {
                    Statement_::Fun(name, params, body) => {
                        env.set_with_file_scope(
                            &name,
                            Value::Fun(name.clone(), params.clone(), body.clone()),
                        );
                    }
                    Statement_::If(condition, ref then_body, ref else_body) => {
                        if done_children {
                            let condition_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for if condition");
                            match condition_value {
                                Value::Boolean(b) => {
                                    if b {
                                        for stmt in then_body.iter().rev() {
                                            stmts_to_eval.push((false, stmt.clone()));
                                        }
                                    } else {
                                        for stmt in else_body.iter().rev() {
                                            stmts_to_eval.push((false, stmt.clone()));
                                        }
                                    }
                                }
                                v => {
                                    return Err(format!("Expected a boolean, but got: {}", v));
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval.push((
                                false,
                                Statement(condition.0, Statement_::Expr(*condition.clone())),
                            ));
                        }
                    }
                    Statement_::Let(variable, expr) => {
                        if done_children {
                            let expr_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            env.set_with_fun_scope(&variable, expr_value.clone());
                            evalled_values.push(expr_value);
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(expr.0, Statement_::Expr(*expr.clone()))));
                        }
                    }
                    Statement_::Assign(variable, expr) => {
                        if done_children {
                            if !env.fun_scope_has_var(&variable) {
                                return Err(format!(
                                    "{} is not currently bound. Try `let {} = something`.",
                                    variable.0, variable.0
                                ));
                            }

                            let expr_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for let value");
                            env.set_with_fun_scope(&variable, expr_value.clone());
                            evalled_values.push(expr_value);
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(expr.0, Statement_::Expr(*expr.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(_, Expression_::IntLiteral(i))) => {
                        evalled_values.push(Value::Integer(i));
                    }
                    Statement_::Expr(Expression(_, Expression_::BoolLiteral(b))) => {
                        evalled_values.push(Value::Boolean(b));
                    }
                    Statement_::Expr(Expression(_, Expression_::StringLiteral(s))) => {
                        evalled_values.push(Value::String(s));
                    }
                    Statement_::Expr(Expression(_, Expression_::Variable(name))) => {
                        if let Some(value) = env.get(&name) {
                            evalled_values.push(value);
                        } else {
                            let stmt = error_prompt(&format!("Undefined variable: {}", name.0))?;
                            stmts_to_eval.push((false, stmt));
                        }
                    }
                    Statement_::Expr(Expression(_, Expression_::BinaryOperator(lhs, _, rhs))) => {
                        if done_children {
                            let lhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for LHS of binary operator");
                            let rhs_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for RHS of binary operator");

                            let lhs_num = match lhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(format!(
                                        "Expected an integer, but got: {}",
                                        lhs_value
                                    ));
                                }
                            };
                            let rhs_num = match rhs_value {
                                Value::Integer(i) => i,
                                _ => {
                                    return Err(format!(
                                        "Expected an integer, but got: {}",
                                        rhs_value
                                    ));
                                }
                            };

                            evalled_values.push(Value::Integer(lhs_num + rhs_num));
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));
                            stmts_to_eval
                                .push((false, Statement(rhs.0, Statement_::Expr(*rhs.clone()))));
                            stmts_to_eval
                                .push((false, Statement(lhs.0, Statement_::Expr(*lhs.clone()))));
                        }
                    }
                    Statement_::Expr(Expression(_, Expression_::Call(receiver, ref args))) => {
                        if done_children {
                            let receiver_value = evalled_values
                                .pop()
                                .expect("Popped an empty value stack for call receiver");
                            let mut arg_values = vec![];
                            for _ in 0..args.len() {
                                arg_values.push(
                                    evalled_values.pop().expect(
                                        "Popped an empty value for stack for call arguments",
                                    ),
                                );
                            }

                            match receiver_value {
                                Value::Fun(name, params, body) => {
                                    if params.len() != arg_values.len() {
                                        return Err(format!(
                                            "Expected {} arguments to function {}, but got {}",
                                            params.len(),
                                            name.0,
                                            arg_values.len()
                                        ));
                                    }

                                    stmts_to_eval_per_fun.push(stmts_to_eval.clone());

                                    env.push_new_fun_scope(&name);
                                    for (param, value) in params.iter().zip(arg_values.iter()) {
                                        env.set_with_fun_scope(param, value.clone());
                                    }

                                    let mut fun_subexprs = vec![];
                                    for stmt in body.iter().rev() {
                                        fun_subexprs.push((false, stmt.clone()));
                                    }
                                    stmts_to_eval_per_fun.push(fun_subexprs);
                                }
                                v => {
                                    return Err(format!("Expected a function, but got: {}", v));
                                }
                            }
                        } else {
                            stmts_to_eval.push((true, Statement(offset, stmt_copy)));

                            stmts_to_eval.push((
                                false,
                                Statement(receiver.0, Statement_::Expr(*receiver.clone())),
                            ));
                            for arg in args {
                                stmts_to_eval
                                    .push((false, Statement(arg.0, Statement_::Expr(arg.clone()))));
                            }
                        }
                    }
                }
            }

            if let Some(subexprs_to_eval) = stmts_to_eval_per_fun.last() {
                if subexprs_to_eval.is_empty() {
                    // Reached end of this block. Pop to the parent.
                    if env.fun_scopes.len() > 1 {
                        // Don't pop the outer scope: that's for the top level environment.
                        env.pop_fun_scope();
                    }
                }
            }
        } else {
            break;
        }
    }

    Ok(evalled_values
        .pop()
        .expect("Should have a value from the last expression"))
}

fn read_replacement(msg: &str) -> Result<Expression, String> {
    println!("{}: {}", "Unexpected error".bright_red(), msg);
    println!("What value should be used instead?\n");

    let mut rl: Editor<()> = Editor::new().unwrap();
    let input = rl
        .readline(&prompt_symbol(1))
        .expect("error: unable to read user input");

    let tokens = lex(input.trim())?;
    let mut token_ptr = &tokens[..];
    parse_expression(&mut token_ptr)
}

#[cfg(test)]
mod tests {
    use crate::parse::{Expression_, BinaryOperatorKind};

    use super::*;

    #[test]
    fn test_eval_bool_literal() {
        let stmts = vec![Statement(
            0,
            Statement_::Expr(Expression(0, Expression_::BoolLiteral(true))),
        )];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let stmts = vec![Statement(
            0,
            Statement_::Let(
                VariableName("foo".into()),
                Box::new(Expression(0, Expression_::BoolLiteral(true))),
            ),
        )];
        eval_stmts(&stmts, &mut env).unwrap();

        let stmts = vec![Statement(
            0,
            Statement_::Expr(Expression(
                0,
                Expression_::Variable(VariableName("foo".into())),
            )),
        )];
        eval_stmts(&stmts, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_stmts() {
        let stmts = vec![
            Statement(
                0,
                Statement_::Expr(Expression(0, Expression_::BoolLiteral(true))),
            ),
            Statement(
                5,
                Statement_::Expr(Expression(0, Expression_::BoolLiteral(false))),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_add() {
        let stmts = vec![Statement(
            0,
            Statement_::Expr(Expression(
                0,
                Expression_::BinaryOperator(
                    Box::new(Expression(0, Expression_::IntLiteral(1))),
                    BinaryOperatorKind::Add,
                    Box::new(Expression(0, Expression_::IntLiteral(2))),
                ),
            )),
        )];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_let() {
        let stmts = vec![
            Statement(
                0,
                Statement_::Let(
                    VariableName("foo".into()),
                    Box::new(Expression(0, Expression_::BoolLiteral(true))),
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Variable(VariableName("foo".into())),
                )),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let stmts = vec![
            Statement(
                0,
                Statement_::Let(
                    VariableName("foo".into()),
                    Box::new(Expression(0, Expression_::BoolLiteral(true))),
                ),
            ),
            Statement(
                0,
                Statement_::Let(
                    VariableName("foo".into()),
                    Box::new(Expression(0, Expression_::BoolLiteral(false))),
                ),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_stmts(&[], &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_call() {
        // fun f() { true; }
        // f();
        let stmts = vec![
            Statement(
                0,
                Statement_::Fun(
                    VariableName("f".into()),
                    vec![],
                    vec![Statement(
                        0,
                        Statement_::Expr(Expression(0, Expression_::BoolLiteral(true))),
                    )],
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Call(
                        Box::new(Expression(
                            0,
                            Expression_::Variable(VariableName("f".into())),
                        )),
                        vec![],
                    ),
                )),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        // fun f(x) { x; }
        // f(123);
        let stmts = vec![
            Statement(
                0,
                Statement_::Fun(
                    VariableName("f".into()),
                    vec![VariableName("x".into())],
                    vec![Statement(
                        0,
                        Statement_::Expr(Expression(
                            0,
                            Expression_::Variable(VariableName("x".into())),
                        )),
                    )],
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Call(
                        Box::new(Expression(
                            0,
                            Expression_::Variable(VariableName("f".into())),
                        )),
                        vec![Expression(0, Expression_::IntLiteral(123))],
                    ),
                )),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        // fun f(x, y) { y; }
        // f(1, 2);
        let stmts = vec![
            Statement(
                0,
                Statement_::Fun(
                    VariableName("f".into()),
                    // TODO: check for duplicate param names.
                    vec![VariableName("x".into()), VariableName("y".into())],
                    vec![Statement(
                        0,
                        Statement_::Expr(Expression(
                            0,
                            Expression_::Variable(VariableName("y".into())),
                        )),
                    )],
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Call(
                        Box::new(Expression(
                            0,
                            Expression_::Variable(VariableName("f".into())),
                        )),
                        vec![
                            Expression(0, Expression_::IntLiteral(1)),
                            Expression(0, Expression_::IntLiteral(2)),
                        ],
                    ),
                )),
            ),
        ];

        let mut env = Env::default();
        let value = eval_stmts(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }
}
