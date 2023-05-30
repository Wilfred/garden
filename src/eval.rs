use std::fmt::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{collections::HashMap, fmt::Display};

use crate::json_session::{Response, ResponseKind};
use crate::parse::BinaryOperatorKind;
use crate::parse::{
    Definition, Definition_, DefinitionsOrExpression, Expression, Expression_, VariableName,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(
        Option<String>,
        VariableName,
        Vec<VariableName>,
        Vec<Expression>,
    ),
    BuiltinFunction(BuiltinFunctionKind),
    String(String),
    List(Vec<Value>),
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinFunctionKind {
    Print,
    IntToString,
    Shell,
    ListAppend,
    StringLength,
    StringSubstring,
}

pub fn builtin_fun_doc(kind: &BuiltinFunctionKind) -> &str {
    match kind {
        BuiltinFunctionKind::Print => {
            "Write a string to stdout.

```
print(\"hello world\");
```"
        }
        BuiltinFunctionKind::IntToString => {
            "Convert an integer to its decimal representation as a string.

```
int_to_string(123); // \"123\"
```"
        }
        BuiltinFunctionKind::Shell =>{
            "Execute the given string as a shell command, and return stdout concatenated with stderr.

```
shell(\"ls\", [\"-l\", \"/\"]);
```"
        }
        BuiltinFunctionKind::ListAppend =>{
            "Return a new list with the value added to the end.

```
list_append([10], 11); // [10, 11]
```"
        }
        BuiltinFunctionKind::StringLength => {
            "Return the number of characters (codepoints) in the string.

```
string_length(\"abc\"); // 3
```"
        }
        BuiltinFunctionKind::StringSubstring => {
            "Return the substring of the string between the indexes specified.

```
string_substring(\"abcdef\", 1, 3); // \"bc\"
```"
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(_, name, _, _) => write!(f, "(function: {})", name.0),
            Value::BuiltinFunction(kind) => {
                let name = match kind {
                    BuiltinFunctionKind::Print => "print",
                    BuiltinFunctionKind::IntToString => "int_to_string",
                    BuiltinFunctionKind::Shell => "shell",
                    BuiltinFunctionKind::ListAppend => "list_append",
                    BuiltinFunctionKind::StringLength => "string_length",
                    BuiltinFunctionKind::StringSubstring => "string_substring",
                };
                write!(f, "(function: {})", name)
            }
            Value::Void => write!(f, "void"),
            Value::String(s) => {
                write!(f, "\"")?;

                // Escape inner double quotes and backslashes.
                for c in s.chars() {
                    match c {
                        '"' => write!(f, "\\\"")?,
                        '\\' => write!(f, "\\\\")?,
                        '\n' => write!(f, "\\n")?,
                        _ => write!(f, "{}", c)?,
                    }
                }

                write!(f, "\"")
            }
            Value::List(items) => {
                write!(f, "[")?;

                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", item)?;
                }

                write!(f, "]")
            }
        }
    }
}

#[derive(Debug)]
pub struct StackFrame {
    pub fun_name: VariableName,
    pub bindings: HashMap<VariableName, Value>,
    pub exprs_to_eval: Vec<(bool, Expression)>,
    pub evalled_values: Vec<Value>,
}

#[derive(Debug)]
pub struct Env {
    pub trace_exprs: bool,
    pub file_scope: HashMap<VariableName, Value>,
    pub stack: Vec<StackFrame>,
}

impl Default for Env {
    fn default() -> Self {
        let mut file_scope = HashMap::new();
        file_scope.insert(
            VariableName("print".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::Print),
        );
        file_scope.insert(
            VariableName("int_to_string".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::IntToString),
        );
        file_scope.insert(
            VariableName("list_append".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::ListAppend),
        );
        file_scope.insert(
            VariableName("shell".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::Shell),
        );
        file_scope.insert(
            VariableName("string_length".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::StringLength),
        );
        file_scope.insert(
            VariableName("string_substring".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::StringSubstring),
        );

        Self {
            trace_exprs: false,
            file_scope,
            stack: vec![StackFrame {
                fun_name: VariableName("toplevel".into()),
                bindings: HashMap::new(),
                exprs_to_eval: vec![],
                evalled_values: vec![Value::Void],
            }],
        }
    }
}

impl Env {
    pub fn pop_to_toplevel(&mut self) {
        self.stack.truncate(1);
        self.stack[0].evalled_values.truncate(1);
    }

    pub fn set_with_file_scope(&mut self, name: &VariableName, value: Value) {
        self.file_scope.insert(name.clone(), value);
    }
}

fn get_var(name: &VariableName, stack_frame: &StackFrame, env: &Env) -> Option<Value> {
    if let Some(value) = stack_frame.bindings.get(name) {
        return Some(value.clone());
    }

    if let Some(value) = env.file_scope.get(name) {
        return Some(value.clone());
    }

    None
}

#[derive(Debug)]
pub struct Session<'a> {
    pub history: String,
    pub interrupted: &'a Arc<AtomicBool>,
    pub has_attached_stdout: bool,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ErrorKind {
    /// An inappropriate value for the current expression, such as a
    /// type error or divide by zero.
    BadValue,
    /// A malformed expression, such as an arity error or assigning to
    /// an undefined variable.
    MalformedExpression,
}

#[derive(Debug)]
pub enum EvalError {
    Interrupted,
    ResumableError(String),
    Stop(Option<ErrorKind>),
}

// TODO: result is really Result<Value, ErrorWithSuspendedEnv>
pub fn eval_def_or_exprs(
    items: &DefinitionsOrExpression,
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    match items {
        DefinitionsOrExpression::Defs(defs) => {
            eval_defs(defs, env);
            Ok(Value::Void)
        }
        DefinitionsOrExpression::Expr(e) => {
            let exprs = vec![e.clone()];
            eval_exprs(&exprs, env, session)
        }
    }
}

pub fn eval_defs(definitions: &[Definition], env: &mut Env) {
    for definition in definitions {
        match &definition.1 {
            Definition_::Fun(doc_comment, name, params, body) => {
                env.set_with_file_scope(
                    name,
                    Value::Fun(
                        doc_comment.clone(),
                        name.clone(),
                        params.clone(),
                        body.clone(),
                    ),
                );
            }
        }
    }
}

// If value is a list of strings, return the strings as a vec. Return
// an error otherwise.
fn as_string_list(value: &Value) -> Result<Vec<String>, Value> {
    match value {
        Value::List(items) => {
            let mut res: Vec<String> = vec![];
            for item in items {
                match item {
                    Value::String(s) => {
                        res.push(s.clone());
                    }
                    _ => {
                        return Err(item.clone());
                    }
                }
            }

            Ok(res)
        }
        _ => Err(value.clone()),
    }
}

fn restore_stack_frame(
    env: &mut Env,
    mut stack_frame: StackFrame,
    expr_to_eval: (bool, Expression),
    evalled_values: &[Value],
    error_kind: Option<ErrorKind>,
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    let offset = expr_to_eval.1 .0.clone();
    stack_frame.exprs_to_eval.push(expr_to_eval);
    stack_frame
        .exprs_to_eval
        .push((false, Expression(offset, Expression_::Stop(error_kind))));

    env.stack.push(stack_frame);
}

struct ErrorInfo {
    message: String,
    restore_values: Vec<Value>,
}

fn eval_if(
    stack_frame: &mut StackFrame,
    then_body: &[Expression],
    else_body: &[Expression],
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    match &condition_value {
        Value::Boolean(b) => {
            if *b {
                for expr in then_body.iter().rev() {
                    stack_frame.exprs_to_eval.push((false, expr.clone()));
                }
            } else {
                for expr in else_body.iter().rev() {
                    stack_frame.exprs_to_eval.push((false, expr.clone()));
                }
            }
        }
        v => {
            return Err(ErrorInfo {
                message: format!("Expected a boolean when evaluating `if`, but got: {}", v),
                restore_values: vec![condition_value],
            });
        }
    }

    Ok(())
}

fn eval_while(
    stack_frame: &mut StackFrame,
    expr: Expression,
    body: &[Expression],
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    match &condition_value {
        Value::Boolean(b) => {
            if *b {
                // Start loop evaluation again.
                stack_frame.exprs_to_eval.push((false, expr));

                // Evaluate the body.
                for expr in body.iter().rev() {
                    stack_frame.exprs_to_eval.push((false, expr.clone()));
                }
            } else {
                stack_frame.evalled_values.push(Value::Void);
            }
        }
        v => {
            return Err(ErrorInfo {
                message: format!("Expected a boolean when evaluating `while`, but got: {}", v),
                restore_values: vec![condition_value],
            });
        }
    }

    Ok(())
}

fn eval_assign(stack_frame: &mut StackFrame, variable: &VariableName) -> Result<(), ErrorInfo> {
    if !stack_frame.bindings.contains_key(variable) {
        return Err(ErrorInfo {
            message: format!(
                "{} is not currently bound. Try `let {} = something`.",
                variable.0, variable.0
            ),
            restore_values: vec![],
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .insert(variable.clone(), expr_value.clone());
    stack_frame.evalled_values.push(expr_value);

    Ok(())
}

fn eval_let(stack_frame: &mut StackFrame, variable: &VariableName) -> Result<(), ErrorInfo> {
    if stack_frame.bindings.contains_key(variable) {
        return Err(ErrorInfo {
            message: format!(
                "{} is already bound. Try `{} = something` instead.",
                variable.0, variable.0
            ),
            restore_values: vec![],
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .insert(variable.clone(), expr_value.clone());
    stack_frame.evalled_values.push(expr_value);
    Ok(())
}

fn eval_boolean_binop(
    stack_frame: &mut StackFrame,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
    {
        let rhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for LHS of binary operator");

        let lhs_bool = match lhs_value {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected a bool, but got: {}", lhs_value),
                    restore_values: vec![lhs_value, rhs_value],
                });
            }
        };
        let rhs_bool = match rhs_value {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected a bool, but got: {}", rhs_value),
                    restore_values: vec![lhs_value, rhs_value],
                });
            }
        };

        match op {
            BinaryOperatorKind::And => {
                stack_frame
                    .evalled_values
                    .push(Value::Boolean(lhs_bool && rhs_bool));
            }
            BinaryOperatorKind::Or => {
                stack_frame
                    .evalled_values
                    .push(Value::Boolean(lhs_bool || rhs_bool));
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn eval_equality_binop(
    stack_frame: &mut StackFrame,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
    let rhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for RHS of binary operator");
    let lhs_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for LHS of binary operator");

    match op {
        BinaryOperatorKind::Equal => {
            stack_frame
                .evalled_values
                .push(Value::Boolean(lhs_value == rhs_value));
        }
        BinaryOperatorKind::NotEqual => {
            stack_frame
                .evalled_values
                .push(Value::Boolean(lhs_value != rhs_value));
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn eval_integer_binop(
    stack_frame: &mut StackFrame,
    op: BinaryOperatorKind,
) -> Result<(), ErrorInfo> {
    {
        let rhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for RHS of binary operator");
        let lhs_value = stack_frame
            .evalled_values
            .pop()
            .expect("Popped an empty value stack for LHS of binary operator");

        let lhs_num = match lhs_value {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected an integer, but got: {}", lhs_value),
                    restore_values: vec![lhs_value, rhs_value],
                });
            }
        };
        let rhs_num = match rhs_value {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected an integer, but got: {}", rhs_value),
                    restore_values: vec![lhs_value, rhs_value],
                });
            }
        };

        match op {
            BinaryOperatorKind::Add => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_add(rhs_num)));
            }
            BinaryOperatorKind::Subtract => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_sub(rhs_num)));
            }
            BinaryOperatorKind::Multiply => {
                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num.wrapping_mul(rhs_num)));
            }
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err(ErrorInfo {
                        message: format!("Tried to divide {} by zero.", rhs_value),
                        restore_values: vec![lhs_value, rhs_value],
                    });
                }

                stack_frame
                    .evalled_values
                    .push(Value::Integer(lhs_num / rhs_num));
            }
            BinaryOperatorKind::LessThan => {
                stack_frame
                    .evalled_values
                    .push(Value::Boolean(lhs_num < rhs_num));
            }
            BinaryOperatorKind::GreaterThan => {
                stack_frame
                    .evalled_values
                    .push(Value::Boolean(lhs_num > rhs_num));
            }
            _ => {
                unreachable!()
            }
        }
    }
    Ok(())
}

fn eval_call(
    stack_frame: &mut StackFrame,
    args: &[Expression],
    session: &Session,
) -> Result<Option<StackFrame>, ErrorInfo> {
    let mut arg_values = vec![];
    for _ in 0..args.len() {
        arg_values.push(
            stack_frame
                .evalled_values
                .pop()
                .expect("Popped an empty value for stack for call arguments"),
        );
    }
    let receiver_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for call receiver");

    match &receiver_value {
        Value::Fun(_, name, params, body) => {
            if params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    message: format!(
                        "Function {} expects {} arguments, but got {}",
                        name.0.clone(),
                        params.len(),
                        arg_values.len()
                    ),
                    restore_values: saved_values,
                });
            }

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in body.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                fun_bindings.insert(param.clone(), value.clone());
            }

            return Ok(Some(StackFrame {
                fun_name: name.clone(),
                bindings: fun_bindings,
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![Value::Void],
            }));
        }
        Value::BuiltinFunction(kind) => match kind {
            BuiltinFunctionKind::Print => {
                if args.len() != 1 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function print requires 1 argument, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                match &arg_values[0] {
                    Value::String(s) => {
                        if session.has_attached_stdout {
                            println!("{}", s);
                        } else {
                            let response = Response {
                                kind: ResponseKind::Printed,
                                value: Ok(format!("{}\n", s)),
                            };
                            let serialized = serde_json::to_string(&response).unwrap();
                            println!("{}", serialized);
                        }
                    }
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected a string, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                }
                stack_frame.evalled_values.push(Value::Void);
            }
            BuiltinFunctionKind::StringLength => {
                if args.len() != 1 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function string_length requires 1 argument, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                match &arg_values[0] {
                    Value::String(s) => {
                        stack_frame
                            .evalled_values
                            .push(Value::Integer(s.chars().count() as i64));
                    }
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected a string, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                }
            }
            BuiltinFunctionKind::Shell => {
                if args.len() != 2 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function shell requires 2 arguments, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                match &arg_values[0] {
                    Value::String(s) => {
                        match as_string_list(&arg_values[1]) {
                            Ok(items) => {
                                let mut command = std::process::Command::new(s);
                                for item in items {
                                    command.arg(item);
                                }

                                // TODO: define a result type in garden to report errors to the user.
                                let output = command.output().expect("failed to execute process");

                                let mut s = String::new();
                                // TODO: complain if output is not UTF-8.
                                s.write_str(&String::from_utf8_lossy(&output.stdout))
                                    .unwrap();
                                s.write_str(&String::from_utf8_lossy(&output.stderr))
                                    .unwrap();

                                stack_frame.evalled_values.push(Value::String(s));
                            }
                            Err(v) => {
                                let mut saved_values = vec![];
                                for value in arg_values.iter().rev() {
                                    saved_values.push(value.clone());
                                }
                                saved_values.push(receiver_value.clone());

                                return Err(ErrorInfo {
                                    message: format!("Expected a list, but got: {}", v),
                                    restore_values: saved_values,
                                });
                            }
                        }
                    }
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected a string, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                }
            }
            BuiltinFunctionKind::ListAppend => {
                if args.len() != 2 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function list_append requires 2 arguments, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                match &arg_values[0] {
                    Value::List(items) => {
                        let mut new_items = items.clone();
                        new_items.push(arg_values[1].clone());
                        stack_frame.evalled_values.push(Value::List(new_items));
                    }
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected a list, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                }
            }
            BuiltinFunctionKind::IntToString => {
                if args.len() != 1 {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format!(
                            "Function int_to_string requires 1 argument, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                match &arg_values[0] {
                    Value::Integer(i) => {
                        stack_frame
                            .evalled_values
                            .push(Value::String(format!("{}", i)));
                    }
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected an integer, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                }
            }
            BuiltinFunctionKind::StringSubstring => {
                if args.len() != 3 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function string_substring requires 3 arguments, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                    });
                }
                let s_arg = match &arg_values[0] {
                    Value::String(s) => s,
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected a string, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                };
                let from_arg = match &arg_values[1] {
                    Value::Integer(i) => i,
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected an integer, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                };
                let to_arg = match &arg_values[2] {
                    Value::Integer(i) => i,
                    v => {
                        let mut saved_values = vec![];
                        for value in arg_values.iter().rev() {
                            saved_values.push(value.clone());
                        }
                        saved_values.push(receiver_value.clone());

                        return Err(ErrorInfo {
                            message: format!("Expected an integer, but got: {}", v),
                            restore_values: saved_values,
                        });
                    }
                };

                if *from_arg < 0 {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format!("The first argument to string_substring must be greater than 0, but got: {}", from_arg),
                        restore_values: saved_values,
                    });
                }

                if from_arg > to_arg {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format!("The first argument to string_substring cannot be greater than the second, but got: {} and {}", from_arg, to_arg),
                        restore_values: saved_values,
                    });
                }

                stack_frame.evalled_values.push(Value::String(
                    s_arg
                        .chars()
                        .skip(*from_arg as usize)
                        .take((to_arg - from_arg) as usize)
                        .collect(),
                ));
            }
        },
        v => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            return Err(ErrorInfo {
                message: format!("Expected a function, but got: {}", v),
                restore_values: saved_values,
            });
        }
    }

    Ok(None)
}

pub fn eval_env(env: &mut Env, session: &mut Session) -> Result<Value, EvalError> {
    loop {
        if let Some(mut stack_frame) = env.stack.pop() {
            if let Some((done_children, Expression(offset, expr_))) =
                stack_frame.exprs_to_eval.pop()
            {
                if session.interrupted.load(Ordering::SeqCst) {
                    session.interrupted.store(false, Ordering::SeqCst);
                    restore_stack_frame(
                        env,
                        stack_frame,
                        (done_children, Expression(offset, expr_)),
                        &[],
                        None,
                    );
                    return Err(EvalError::Interrupted);
                }

                let expr_copy = expr_.clone();

                if env.trace_exprs {
                    println!("{:?} {}", expr_, done_children);
                }
                match expr_ {
                    Expression_::If(condition, ref then_body, ref else_body) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_if(&mut stack_frame, then_body, else_body)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *condition.clone()));
                        }
                    }
                    Expression_::While(condition, ref body) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_while(
                                &mut stack_frame,
                                Expression(offset.clone(), expr_copy.clone()),
                                body,
                            ) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *condition.clone()));
                        }
                    }
                    Expression_::Return(expr) => {
                        if done_children {
                            // No more expressions to evaluate in this function.
                            stack_frame.exprs_to_eval.clear();
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Assign(variable, expr) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_assign(&mut stack_frame, &variable)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Let(variable, expr) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_let(&mut stack_frame, &variable)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Stop(e) => {
                        stack_frame
                            .exprs_to_eval
                            .push((false, Expression(offset, Expression_::Stop(e))));
                        env.stack.push(stack_frame);
                        return Err(EvalError::Stop(e));
                    }
                    Expression_::IntLiteral(i) => {
                        stack_frame.evalled_values.push(Value::Integer(i));
                    }
                    Expression_::BoolLiteral(b) => {
                        stack_frame.evalled_values.push(Value::Boolean(b));
                    }
                    Expression_::StringLiteral(s) => {
                        stack_frame.evalled_values.push(Value::String(s));
                    }
                    Expression_::ListLiteral(items) => {
                        if done_children {
                            let mut list_values = Vec::with_capacity(items.len());
                            for _ in 0..items.len() {
                                list_values.push(stack_frame.evalled_values.pop().expect(
                                    "Value stack should have sufficient items for the list literal",
                                ));
                            }

                            stack_frame.evalled_values.push(Value::List(list_values));
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));

                            for item in items.iter() {
                                stack_frame.exprs_to_eval.push((false, item.clone()));
                            }
                        }
                    }
                    Expression_::Variable(name) => {
                        if let Some(value) = get_var(&name, &stack_frame, env) {
                            stack_frame.evalled_values.push(value);
                        } else {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(offset, expr_copy)),
                                &[],
                                Some(ErrorKind::MalformedExpression),
                            );
                            return Err(EvalError::ResumableError(format!(
                                "Undefined variable: {}.",
                                name.0
                            )));
                        }
                    }
                    Expression_::BinaryOperator(
                        lhs,
                        op @ (BinaryOperatorKind::Add
                        | BinaryOperatorKind::Subtract
                        | BinaryOperatorKind::Multiply
                        | BinaryOperatorKind::Divide
                        | BinaryOperatorKind::LessThan
                        | BinaryOperatorKind::GreaterThan),
                        rhs,
                    ) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_integer_binop(&mut stack_frame, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                            stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                        }
                    }
                    Expression_::BinaryOperator(
                        lhs,
                        op @ (BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual),
                        rhs,
                    ) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_equality_binop(&mut stack_frame, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                            stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                        }
                    }
                    Expression_::BinaryOperator(
                        lhs,
                        op @ (BinaryOperatorKind::And | BinaryOperatorKind::Or),
                        rhs,
                    ) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                            }) = eval_boolean_binop(&mut stack_frame, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(offset, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(message));
                            }
                        } else {
                            // TODO: do short-circuit evaluation of && and ||.
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                            stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                        }
                    }
                    Expression_::Call(receiver, ref args) => {
                        if done_children {
                            match eval_call(&mut stack_frame, args, session) {
                                Ok(Some(new_stack_frame)) => {
                                    env.stack.push(stack_frame);
                                    env.stack.push(new_stack_frame);
                                    continue;
                                }
                                Ok(None) => {}
                                Err(ErrorInfo {
                                    message,
                                    restore_values,
                                }) => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(offset, expr_copy)),
                                        &restore_values,
                                        // TODO: let ErrorInfo specify
                                        // kind, as not all errors on
                                        // calling functions are this
                                        // kind.
                                        Some(ErrorKind::MalformedExpression),
                                    );
                                    return Err(EvalError::ResumableError(message));
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(offset, expr_copy)));

                            for arg in args {
                                stack_frame.exprs_to_eval.push((false, arg.clone()));
                            }
                            // Push the receiver after arguments, so
                            // we evaluate it before arguments. This
                            // makes it easier to use :replace on bad
                            // functions.
                            stack_frame.exprs_to_eval.push((false, *receiver.clone()));
                        }
                    }
                }
            }

            if stack_frame.exprs_to_eval.is_empty() {
                // No more statements in this stack frame.
                if env.stack.is_empty() {
                    // Don't pop the outer scope: that's for the top level environment.
                    env.stack.push(stack_frame);
                    break;
                } else {
                    // The final evaluation result of the function
                    // call should be used in the previous stack
                    // frame.
                    let result = stack_frame
                        .evalled_values
                        .pop()
                        .expect("Should have a value");
                    env.stack.last_mut().unwrap().evalled_values.push(result);
                }
            } else {
                // Keep going on this stack frame.
                env.stack.push(stack_frame);
            }
        } else {
            unreachable!();
        }
    }

    Ok(env
        .stack
        .last_mut()
        .expect("toplevel stack frame should exist")
        .evalled_values
        .pop()
        .expect("Should have a value from the last expression"))
}

pub fn eval_exprs(
    exprs: &[Expression],
    env: &mut Env,
    session: &mut Session,
) -> Result<Value, EvalError> {
    let mut exprs_to_eval = vec![];
    for expr in exprs.iter().rev() {
        exprs_to_eval.push((false, expr.clone()));
    }

    let top_stack = env.stack.last_mut().unwrap();
    // TODO: do this setup outside of this function.
    top_stack.exprs_to_eval = exprs_to_eval;

    eval_env(env, session)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parse::{parse_def_or_expr_from_str, parse_exprs_from_str, Position};

    use super::*;

    fn eval_exprs(exprs: &[Expression], env: &mut Env) -> Result<Value, EvalError> {
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut session = Session {
            history: String::new(),
            interrupted: &interrupted,
            has_attached_stdout: false,
        };

        super::eval_exprs(exprs, env, &mut session)
    }

    #[test]
    fn test_eval_bool_literal() {
        let exprs = vec![Expression(
            Position {
                offset: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::BoolLiteral(true),
        )];

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_equality() {
        let exprs = parse_exprs_from_str("\"a\" == \"b\";").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_persist_env() {
        let mut env = Env::default();

        let exprs = vec![Expression(
            Position {
                offset: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Let(
                VariableName("foo".into()),
                Box::new(Expression(
                    Position {
                        offset: 0,
                        path: PathBuf::from("__test.gdn"),
                    },
                    Expression_::BoolLiteral(true),
                )),
            ),
        )];
        eval_exprs(&exprs, &mut env).unwrap();

        let exprs = vec![Expression(
            Position {
                offset: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Variable(VariableName("foo".into())),
        )];
        eval_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_stmts() {
        let stmts = parse_exprs_from_str("true; false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_add() {
        let stmts = parse_exprs_from_str("1 + 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_list_literal() {
        let stmts = parse_exprs_from_str("[1 + 2, 3 * 4];").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(vec![Value::Integer(3), Value::Integer(12)])
        );
    }

    #[test]
    fn test_eval_let() {
        let stmts = parse_exprs_from_str("let foo = true; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let stmts = parse_exprs_from_str("let foo = true; let foo = false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env);
        assert!(value.is_err());
    }

    #[test]
    fn test_eval_if() {
        let stmts = parse_exprs_from_str("let foo = if (true) { 1; } else { 2; }; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_list_append() {
        let stmts = parse_exprs_from_str("list_append([1, 2], 3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])
        );
    }

    #[test]
    fn test_eval_string_length() {
        let stmts = parse_exprs_from_str("string_length(\"abc\");").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_string_substring() {
        let stmts = parse_exprs_from_str("string_substring(\"abcdef\", 1, 3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::String("bc".into()));
    }

    #[test]
    fn test_eval_call() {
        let mut env = Env::default();

        let defs =
            match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "fun f() { true; }")
                .unwrap()
            {
                DefinitionsOrExpression::Defs(defs) => defs,
                DefinitionsOrExpression::Expr(_) => unreachable!(),
            };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_call_with_arg() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "fun f(x) {x; }")
            .unwrap()
        {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f(123);").unwrap();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(123));
    }

    #[test]
    fn test_eval_call_second_arg() {
        let mut env = Env::default();

        let defs =
            match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "fun f(x, y) { y; }")
                .unwrap()
            {
                DefinitionsOrExpression::Defs(defs) => defs,
                DefinitionsOrExpression::Expr(_) => unreachable!(),
            };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f(1, 2);").unwrap();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_while() {
        let stmts = parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_env_after_call() {
        let mut env = Env::default();

        let defs =
            match parse_def_or_expr_from_str(&PathBuf::from("__test.gdn"), "fun id(x) { x; }")
                .unwrap()
            {
                DefinitionsOrExpression::Defs(defs) => defs,
                DefinitionsOrExpression::Expr(_) => unreachable!(),
            };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("let i = 0; id(i); i;").unwrap();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(0));
    }

    #[test]
    fn test_eval_return() {
        let mut env = Env::default();

        let defs = match parse_def_or_expr_from_str(
            &PathBuf::from("__test.gdn"),
            "fun f() { return 1; 2; }",
        )
        .unwrap()
        {
            DefinitionsOrExpression::Defs(defs) => defs,
            DefinitionsOrExpression::Expr(_) => unreachable!(),
        };
        eval_defs(&defs, &mut env);

        let stmts = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&stmts, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(format!("{}", value), "\"foo \\\\ \\\" \\n bar\"");
    }
}
