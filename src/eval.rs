use std::{collections::HashMap, fmt::Display};

use crate::parse::{lex, parse_expression, Expression, Expression_, Statement_};
use crate::parse::{parse_toplevel, Statement};
use crate::prompt::prompt_symbol;

use owo_colors::OwoColorize;
use rustyline::Editor;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(String, Vec<String>, Vec<Statement>),
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
            Value::Fun(name, _, _) => write!(f, "(function: {})", name),
            Value::BuiltinFunction(BuiltinFunctionKind::Print) => write!(f, "(function: print)"),
            Value::Void => write!(f, "void"),
            // TODO: escape inner double quotes.
            Value::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    pub file_scope: HashMap<String, Value>,
    pub fun_scopes: Vec<(String, HashMap<String, Value>)>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope: HashMap<String, Value> = HashMap::new();
        file_scope.insert(
            "print".to_owned(),
            Value::BuiltinFunction(BuiltinFunctionKind::Print),
        );

        Self {
            file_scope,
            fun_scopes: vec![("toplevel".into(), HashMap::new())],
        }
    }
}

impl Env {
    pub fn get(&self, name: &str) -> Option<Value> {
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

    pub fn push_new_fun_scope(&mut self, description: String) {
        self.fun_scopes.push((description, HashMap::new()));
    }

    pub fn pop_fun_scope(&mut self) {
        self.fun_scopes.pop().unwrap();
    }

    pub fn set_with_file_scope(&mut self, name: &str, value: Value) {
        self.file_scope.insert(name.to_string(), value);
    }

    pub fn set_with_fun_scope(&mut self, name: &str, value: Value) {
        let (_, fun_scope) = &mut self.fun_scopes.last_mut().unwrap();
        fun_scope.insert(name.to_string(), value);
    }
}

#[derive(Debug)]
enum NextStep {
    NextStmt { idx: usize },
    EvalLet(String),
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
    let mut subexprs_to_eval_per_fun: Vec<Vec<(bool, Expression)>> = vec![vec![]];
    let mut subexprs_values: Vec<Value> = vec![Value::Void];
    let mut next_steps: Vec<NextStep> = vec![NextStep::NextStmt { idx: 0 }];
    let mut fun_bodies: Vec<Vec<Statement>> = vec![stmts.to_vec()];

    loop {
        let subexprs_to_eval = subexprs_to_eval_per_fun.last_mut().unwrap();
        while let Some((done_subexprs, Expression(offset, expr_))) = subexprs_to_eval.pop() {
            match expr_.clone() {
                Expression_::IntLiteral(i) => {
                    subexprs_values.push(Value::Integer(i));
                }
                Expression_::BoolLiteral(b) => {
                    subexprs_values.push(Value::Boolean(b));
                }
                Expression_::StringLiteral(s) => {
                    subexprs_values.push(Value::String(s));
                }
                Expression_::BinaryOperator(lhs, _, rhs) => {
                    if done_subexprs {
                        let rhs_value = subexprs_values
                            .pop()
                            .expect("Got an empty value stack when evaluating RHS of +");
                        let lhs_value = subexprs_values
                            .pop()
                            .expect("Got an empty value stack when evaluating LHS of +");

                        let lhs_num = match lhs_value {
                            Value::Integer(i) => i,
                            _ => {
                                // TODO: read replacement value
                                let _lhs_new = read_replacement(&format!(
                                    "Expected an integer, but got: {}",
                                    lhs_value
                                ))?;
                                return Err(format!("Expected an integer, but got: {}", lhs_value));
                            }
                        };

                        let rhs_num = match rhs_value {
                            Value::Integer(i) => i,
                            _ => {
                                let _rhs_new = read_replacement(&format!(
                                    "Expected an integer, but got: {}",
                                    rhs_value
                                ))?;
                                return Err(format!("Expected an integer, but got: {}", rhs_value));
                            }
                        };

                        subexprs_values.push(Value::Integer(lhs_num + rhs_num))
                    } else {
                        subexprs_to_eval.push((true, Expression(offset, expr_)));
                        subexprs_to_eval.push((false, *rhs.clone()));
                        subexprs_to_eval.push((false, *lhs.clone()));
                    }
                }
                Expression_::Variable(s) => match env.get(&s) {
                    Some(v) => subexprs_values.push(v),
                    None => {
                        let replacement_stmt = error_prompt(&format!("Unbound variable: {}", s))?;
                        match replacement_stmt {
                            Statement(_, Statement_::Expr(expr)) => {
                                subexprs_to_eval.push((false, expr));
                            }
                            _ => {
                                return Err(format!(
                                    "Expected an expression, but got {:?}",
                                    replacement_stmt
                                ));
                            }
                        }
                    }
                },
                Expression_::Call(receiver, args) => {
                    if done_subexprs {
                        let receiver = subexprs_values
                            .pop()
                            .expect("Popped an empty value stack for call receiver");

                        let mut arg_values = vec![];
                        for _ in 0..args.len() {
                            arg_values.push(
                                subexprs_values
                                    .pop()
                                    .expect("Popped an empty value for stack for call arguments"),
                            );
                        }

                        match receiver {
                            Value::Fun(name, params, body) => {
                                if args.len() != params.len() {
                                    // TODO: prompt user for extra arguments.
                                    return Err(format!(
                                        "Function {} requires {} arguments, but got: {}",
                                        name,
                                        params.len(),
                                        args.len()
                                    ));
                                }

                                env.push_new_fun_scope(name);
                                for (var_name, value) in params.iter().zip(arg_values) {
                                    env.set_with_fun_scope(&var_name, value);
                                }

                                subexprs_to_eval_per_fun.push(vec![]);
                                fun_bodies.push(body);
                                next_steps.push(NextStep::NextStmt { idx: 0 });

                                break;
                            }
                            Value::BuiltinFunction(k) => match k {
                                BuiltinFunctionKind::Print => {
                                    if args.len() != 1 {
                                        return Err(format!(
                                            "Function print requires 1 argument, but got: {}",
                                            args.len()
                                        ));
                                    }
                                    match &arg_values[0] {
                                        Value::String(s) => println!("{}", s),
                                        v => {
                                            return Err(format!(
                                                "Expected a string, but got: {}",
                                                v
                                            ));
                                        }
                                    }
                                }
                            },
                            v => {
                                return Err(format!("Expected a function, but got: {}", v));
                            }
                        }
                    } else {
                        subexprs_to_eval.push((true, Expression(offset, expr_)));
                        subexprs_to_eval.push((false, *receiver.clone()));
                        for arg in args {
                            subexprs_to_eval.push((false, arg.clone()));
                        }
                    }
                }
            }
        }

        if let Some(step) = next_steps.pop() {
            match step {
                NextStep::NextStmt { idx } => {
                    let subexprs_to_eval = subexprs_to_eval_per_fun.last_mut().unwrap();
                    let stmts = fun_bodies
                        .last()
                        .expect("Function stack should never be empty");
                    if let Some(Statement(_, stmt)) = stmts.get(idx) {
                        if idx > 0 {
                            // Discard value from previous statement.
                            subexprs_values.pop().expect("Popped an empty value stack");
                        }
                        match stmt {
                            Statement_::Fun(name, params, body) => {
                                let value = Value::Fun(name.clone(), params.clone(), body.clone());
                                env.set_with_file_scope(name, value);
                                subexprs_values.push(Value::Void);
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                            }
                            Statement_::Let(v, e) => {
                                subexprs_to_eval.push((false, e.clone()));
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                                next_steps.push(NextStep::EvalLet(v.clone()));
                            }
                            Statement_::Expr(e) => {
                                subexprs_to_eval.push((false, e.clone()));
                                next_steps.push(NextStep::NextStmt { idx: idx + 1 });
                            }
                        }
                    } else {
                        // Reached end of this block. Pop to the parent.
                        if env.fun_scopes.len() > 1 {
                            // Don't pop the outer scope: that's for the top level environment.
                            env.pop_fun_scope();
                            subexprs_to_eval_per_fun.pop();
                        }

                        fun_bodies.pop();
                    }
                }
                NextStep::EvalLet(variable) => {
                    let value = subexprs_values
                        .pop()
                        .expect("Got an empty value stack when evaluating let");

                    // TODO: does this make sense for scope for let outside a function?
                    env.set_with_fun_scope(&variable, value.clone());

                    subexprs_values.push(value);
                }
            }
        } else {
            break;
        }
    }

    Ok(subexprs_values
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
    use crate::parse::Expression_;

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
            Statement_::Let("foo".into(), Expression(0, Expression_::BoolLiteral(true))),
        )];
        eval_stmts(&stmts, &mut env).unwrap();

        let stmts = vec![Statement(
            0,
            Statement_::Expr(Expression(0, Expression_::Variable("foo".into()))),
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
                    "+".into(),
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
                Statement_::Let("foo".into(), Expression(0, Expression_::BoolLiteral(true))),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(0, Expression_::Variable("foo".into()))),
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
                Statement_::Let("foo".into(), Expression(0, Expression_::BoolLiteral(true))),
            ),
            Statement(
                0,
                Statement_::Let("bar".into(), Expression(0, Expression_::BoolLiteral(false))),
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
                    "f".into(),
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
                        Box::new(Expression(0, Expression_::Variable("f".into()))),
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
                    "f".into(),
                    vec!["x".into()],
                    vec![Statement(
                        0,
                        Statement_::Expr(Expression(0, Expression_::Variable("x".into()))),
                    )],
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Call(
                        Box::new(Expression(0, Expression_::Variable("f".into()))),
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
                    "f".into(),
                    // TODO: check for duplicate param names.
                    vec!["x".into(), "y".into()],
                    vec![Statement(
                        0,
                        Statement_::Expr(Expression(0, Expression_::Variable("y".into()))),
                    )],
                ),
            ),
            Statement(
                0,
                Statement_::Expr(Expression(
                    0,
                    Expression_::Call(
                        Box::new(Expression(0, Expression_::Variable("f".into()))),
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
