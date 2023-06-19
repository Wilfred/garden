use std::fmt::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{collections::HashMap, fmt::Display};

use crate::json_session::{Response, ResponseKind};
use crate::parse::{BinaryOperatorKind, Position, Variable};
use crate::parse::{
    Definition, Definition_, DefinitionsOrExpression, Expression, Expression_, VariableName,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(Option<String>, Variable, Vec<Variable>, Vec<Expression>),
    BuiltinFunction(BuiltinFunctionKind),
    String(String),
    List(Vec<Value>),
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinFunctionKind {
    Print,
    DebugPrint,
    IntToString,
    Shell,
    ListAppend,
    StringLength,
    StringSubstring,
    WorkingDirectory,
}

pub fn builtin_fun_doc(kind: &BuiltinFunctionKind) -> &str {
    match kind {
        BuiltinFunctionKind::Print => {
            "Write a string to stdout.

```
print(\"hello world\");
```"
        }
        BuiltinFunctionKind::DebugPrint => {
            "Write an arbitrary value to stdout, along with debugging metadata.

```
dbg([1, 2]);
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
        BuiltinFunctionKind::WorkingDirectory => {
            "Return the path of the current working directory.

```
working_directory(); // \"/home/yourname/awesome_garden_project\"
```"
        }
    }
}

/// Convert "foo" to "\"foo\"", a representation that we can print as
/// a valid Garden string literal.
pub fn escape_string_literal(s: &str) -> String {
    let mut res = String::new();
    res.push('"');

    // Escape inner double quotes and backslashes.
    for c in s.chars() {
        match c {
            '"' => res.push_str("\\\""),
            '\n' => res.push_str("\\n"),
            '\\' => res.push_str("\\\\"),
            _ => res.push(c),
        }
    }

    res.push('"');
    res
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Fun(_, name, _, _) => write!(f, "(function: {})", name.1 .0),
            Value::BuiltinFunction(kind) => {
                let name = match kind {
                    BuiltinFunctionKind::DebugPrint => "dbg",
                    BuiltinFunctionKind::Print => "print",
                    BuiltinFunctionKind::IntToString => "int_to_string",
                    BuiltinFunctionKind::Shell => "shell",
                    BuiltinFunctionKind::ListAppend => "list_append",
                    BuiltinFunctionKind::StringLength => "string_length",
                    BuiltinFunctionKind::StringSubstring => "string_substring",
                    BuiltinFunctionKind::WorkingDirectory => "working_directory",
                };
                write!(f, "(function: {})", name)
            }
            Value::Void => write!(f, "void"),
            Value::String(s) => {
                write!(f, "{}", escape_string_literal(s))
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
pub struct Bindings(Vec<HashMap<VariableName, Value>>);

impl Bindings {
    fn new_with(outer_scope: HashMap<VariableName, Value>) -> Self {
        Self(vec![outer_scope])
    }

    fn get(&self, name: &VariableName) -> Option<Value> {
        // TODO: this allows shadowing. Is that desirable -- does it
        // make REPL workflows less convenient when it's harder to inspect?
        //
        // (Probably not, as long as users can inspect everything.)
        for block_bindings in self.0.iter().rev() {
            if let Some(value) = block_bindings.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    fn has(&self, name: &VariableName) -> bool {
        self.get(name).is_some()
    }

    fn add_new(&mut self, name: &VariableName, value: Value) {
        let block_bindings = self
            .0
            .last_mut()
            .expect("Vec of bindings should always be non-empty");
        block_bindings.insert(name.clone(), value);
    }

    fn set_existing(&mut self, name: &VariableName, value: Value) {
        for block_bindings in self.0.iter_mut().rev() {
            if block_bindings.contains_key(name) {
                block_bindings.insert(name.clone(), value);
                return;
            }
        }
        unreachable!()
    }

    pub fn all(&self) -> Vec<(&VariableName, &Value)> {
        let mut res = vec![];
        for block_bindings in self.0.iter().rev() {
            for (k, v) in block_bindings.iter() {
                res.push((k, v));
            }
        }

        res
    }
}

impl Default for Bindings {
    fn default() -> Self {
        Self(vec![HashMap::new()])
    }
}

#[derive(Debug)]
pub struct StackFrame {
    // TODO: use Variable in here so we have the function position.
    pub fun_name: VariableName,
    pub bindings: Bindings,
    pub exprs_to_eval: Vec<(bool, Expression)>,
    pub evalled_values: Vec<(Position, Value)>,
}

impl StackFrame {
    fn enter_block(&mut self) {
        self.bindings.0.push(HashMap::new());
    }

    fn exit_block(&mut self) {
        self.bindings.0.pop();
        assert!(!self.bindings.0.is_empty());
    }
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
            VariableName("dbg".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::DebugPrint),
        );
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
        file_scope.insert(
            VariableName("working_directory".to_owned()),
            Value::BuiltinFunction(BuiltinFunctionKind::WorkingDirectory),
        );

        Self {
            trace_exprs: false,
            file_scope,
            stack: vec![StackFrame {
                fun_name: VariableName("toplevel".into()),
                bindings: Bindings::default(),
                exprs_to_eval: vec![],
                evalled_values: vec![(
                    Position {
                        // TODO: do these values make sense?
                        offset: 0,
                        end_offset: 0,
                        path: PathBuf::from("__toplevel__"),
                    },
                    Value::Void,
                )],
            }],
        }
    }
}

impl Env {
    pub fn pop_to_toplevel(&mut self) {
        self.stack.truncate(1);
        self.stack[0].evalled_values.truncate(1);
        self.stack[0].bindings.0.truncate(1);
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
    ResumableError(Position, String),
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
                    &name.1,
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
    evalled_values: &[(Position, Value)],
    error_kind: Option<ErrorKind>,
) {
    for value in evalled_values {
        stack_frame.evalled_values.push(value.clone());
    }

    let position = expr_to_eval.1 .0.clone();
    stack_frame.exprs_to_eval.push(expr_to_eval);
    stack_frame
        .exprs_to_eval
        .push((false, Expression(position, Expression_::Stop(error_kind))));

    env.stack.push(stack_frame);
}

struct ErrorInfo {
    error_position: Position,
    message: String,
    restore_values: Vec<(Position, Value)>,
}

fn eval_if(
    stack_frame: &mut StackFrame,
    position: &Position,
    bool_position: &Position,
    then_body: &[Expression],
    else_body: &[Expression],
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    match &condition_value.1 {
        Value::Boolean(b) => {
            if *b {
                stack_frame.exprs_to_eval.push((
                    false,
                    Expression(position.clone(), Expression_::Block(then_body.into())),
                ));
            } else {
                stack_frame.exprs_to_eval.push((
                    false,
                    Expression(position.clone(), Expression_::Block(else_body.into())),
                ));
            }
        }
        v => {
            return Err(ErrorInfo {
                message: format!("Expected a boolean when evaluating `if`, but got: {}", v),
                restore_values: vec![condition_value],
                error_position: bool_position.clone(),
            });
        }
    }

    Ok(())
}

fn eval_while(
    stack_frame: &mut StackFrame,
    condition_pos: &Position,
    expr: Expression,
    body: &[Expression],
) -> Result<(), ErrorInfo> {
    let condition_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for if condition");

    match &condition_value.1 {
        Value::Boolean(b) => {
            if *b {
                // Start loop evaluation again.
                stack_frame.exprs_to_eval.push((false, expr.clone()));

                // Evaluate the body.
                stack_frame
                    .exprs_to_eval
                    .push((false, Expression(expr.0, Expression_::Block(body.into()))))
            } else {
                // TODO: It's weird using the position of the
                // condition when there's no else.
                stack_frame
                    .evalled_values
                    .push((condition_pos.clone(), Value::Void));
            }
        }
        v => {
            return Err(ErrorInfo {
                message: format!("Expected a boolean when evaluating `while`, but got: {}", v),
                restore_values: vec![condition_value],
                error_position: condition_pos.clone(),
            });
        }
    }

    Ok(())
}

fn eval_assign(stack_frame: &mut StackFrame, variable: &Variable) -> Result<(), ErrorInfo> {
    let var_name = &variable.1;
    if !stack_frame.bindings.has(&var_name) {
        return Err(ErrorInfo {
            message: format!(
                "{} is not currently bound. Try `let {} = something`.",
                var_name.0, var_name.0
            ),
            restore_values: vec![],
            error_position: variable.0.clone(),
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .set_existing(&var_name, expr_value.1.clone());
    stack_frame.evalled_values.push(expr_value);

    Ok(())
}

fn eval_let(stack_frame: &mut StackFrame, variable: &Variable) -> Result<(), ErrorInfo> {
    let var_name = &variable.1;
    if stack_frame.bindings.has(&var_name) {
        return Err(ErrorInfo {
            message: format!(
                "{} is already bound. Try `{} = something` instead.",
                var_name.0, var_name.0
            ),
            restore_values: vec![],
            error_position: variable.0.clone(),
        });
    }

    let expr_value = stack_frame
        .evalled_values
        .pop()
        .expect("Popped an empty value stack for let value");
    stack_frame
        .bindings
        .add_new(&var_name, expr_value.1.clone());
    stack_frame.evalled_values.push(expr_value);
    Ok(())
}

fn eval_boolean_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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

        let lhs_bool = match lhs_value.1 {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected a bool, but got: {}", lhs_value.1),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_value.0,
                });
            }
        };
        let rhs_bool = match rhs_value.1 {
            Value::Boolean(b) => b,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected a bool, but got: {}", rhs_value.1),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_value.0,
                });
            }
        };

        match op {
            BinaryOperatorKind::And => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_bool && rhs_bool)));
            }
            BinaryOperatorKind::Or => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_bool || rhs_bool)));
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn eval_equality_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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
                .push((position.clone(), Value::Boolean(lhs_value.1 == rhs_value.1)));
        }
        BinaryOperatorKind::NotEqual => {
            stack_frame
                .evalled_values
                .push((position.clone(), Value::Boolean(lhs_value.1 != rhs_value.1)));
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn eval_integer_binop(
    stack_frame: &mut StackFrame,
    position: &Position,
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

        let lhs_num = match lhs_value.1 {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected an integer, but got: {}", lhs_value.1),
                    restore_values: vec![lhs_value.clone(), rhs_value],
                    error_position: lhs_value.0,
                });
            }
        };
        let rhs_num = match rhs_value.1 {
            Value::Integer(i) => i,
            _ => {
                return Err(ErrorInfo {
                    message: format!("Expected an integer, but got: {}", rhs_value.1),
                    restore_values: vec![lhs_value, rhs_value.clone()],
                    error_position: rhs_value.0,
                });
            }
        };

        match op {
            BinaryOperatorKind::Add => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_add(rhs_num)),
                ));
            }
            BinaryOperatorKind::Subtract => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_sub(rhs_num)),
                ));
            }
            BinaryOperatorKind::Multiply => {
                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::Integer(lhs_num.wrapping_mul(rhs_num)),
                ));
            }
            BinaryOperatorKind::Divide => {
                if rhs_num == 0 {
                    return Err(ErrorInfo {
                        message: format!("Tried to divide {} by zero.", rhs_value.1),
                        restore_values: vec![lhs_value, rhs_value.clone()],
                        error_position: rhs_value.0,
                    });
                }

                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Integer(lhs_num / rhs_num)));
            }
            BinaryOperatorKind::LessThan => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num < rhs_num)));
            }
            BinaryOperatorKind::GreaterThan => {
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Boolean(lhs_num > rhs_num)));
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
    position: &Position,
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

    match &receiver_value.1 {
        Value::Fun(_, name, params, body) => {
            if params.len() != arg_values.len() {
                let mut saved_values = vec![receiver_value.clone()];
                for value in arg_values.iter().rev() {
                    saved_values.push(value.clone());
                }

                return Err(ErrorInfo {
                    message: format!(
                        "Function {} expects {} argument{}, but got {}",
                        name.1 .0.clone(),
                        params.len(),
                        if params.len() == 1 { "" } else { "s" },
                        arg_values.len()
                    ),
                    restore_values: saved_values,
                    error_position: receiver_value.0,
                });
            }

            let mut fun_subexprs: Vec<(bool, Expression)> = vec![];
            for expr in body.iter().rev() {
                fun_subexprs.push((false, expr.clone()));
            }

            let mut fun_bindings = HashMap::new();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                fun_bindings.insert(param.1.clone(), value.1.clone());
            }

            return Ok(Some(StackFrame {
                fun_name: name.1.clone(),
                bindings: Bindings::new_with(fun_bindings),
                exprs_to_eval: fun_subexprs,
                evalled_values: vec![(name.0.clone(), Value::Void)],
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
                        error_position: position.clone(),
                    });
                }
                match &arg_values[0].1 {
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
                            error_position: arg_values[0].0.clone(),
                        });
                    }
                }
                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Void));
            }
            BuiltinFunctionKind::DebugPrint => {
                if args.len() != 1 {
                    let mut saved_values = vec![receiver_value.clone()];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }

                    return Err(ErrorInfo {
                        message: format!(
                            "Function dbg requires 1 argument, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                        error_position: position.clone(),
                    });
                }

                // TODO: define a proper pretty-printer for values
                // rather than using Rust's Debug.
                let value = &arg_values[0].1;
                if session.has_attached_stdout {
                    println!("{:?}", value);
                } else {
                    let response = Response {
                        kind: ResponseKind::Printed,
                        value: Ok(format!("{:?}\n", value)),
                    };
                    let serialized = serde_json::to_string(&response).unwrap();
                    println!("{}", serialized);
                }

                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::Void));
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
                        error_position: position.clone(),
                    });
                }
                match &arg_values[0].1 {
                    Value::String(s) => {
                        stack_frame
                            .evalled_values
                            .push((position.clone(), Value::Integer(s.chars().count() as i64)));
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
                            error_position: arg_values[0].0.clone(),
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
                        error_position: position.clone(),
                    });
                }
                match &arg_values[0].1 {
                    Value::String(s) => {
                        match as_string_list(&arg_values[1].1) {
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

                                stack_frame
                                    .evalled_values
                                    .push((position.clone(), Value::String(s)));
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
                                    error_position: arg_values[0].0.clone(),
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
                            error_position: arg_values[0].0.clone(),
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
                        error_position: position.clone(),
                    });
                }
                match &arg_values[0].1 {
                    Value::List(items) => {
                        let mut new_items = items.clone();
                        new_items.push(arg_values[1].1.clone());
                        stack_frame
                            .evalled_values
                            .push((position.clone(), Value::List(new_items)));
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
                            error_position: arg_values[0].0.clone(),
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
                        error_position: position.clone(),
                    });
                }
                match &arg_values[0].1 {
                    Value::Integer(i) => {
                        stack_frame
                            .evalled_values
                            .push((position.clone(), Value::String(format!("{}", i))));
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
                            error_position: arg_values[0].0.clone(),
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
                        error_position: position.clone(),
                    });
                }
                let s_arg = match &arg_values[0].1 {
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
                            error_position: arg_values[0].0.clone(),
                        });
                    }
                };
                let from_arg = match &arg_values[1].1 {
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
                            error_position: arg_values[1].0.clone(),
                        });
                    }
                };
                let to_arg = match &arg_values[2].1 {
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
                            error_position: arg_values[2].0.clone(),
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
                        message: format!("The second argument to string_substring must be greater than 0, but got: {}", from_arg),
                        restore_values: saved_values,
                        error_position: arg_values[1].0.clone(),
                    });
                }

                if from_arg > to_arg {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format!("The second argument to string_substring cannot be greater than the third, but got: {} and {}", from_arg, to_arg),
                        restore_values: saved_values,
                        error_position: arg_values[1].0.clone(),
                    });
                }

                stack_frame.evalled_values.push((
                    position.clone(),
                    Value::String(
                        s_arg
                            .chars()
                            .skip(*from_arg as usize)
                            .take((to_arg - from_arg) as usize)
                            .collect(),
                    ),
                ));
            }
            BuiltinFunctionKind::WorkingDirectory => {
                if args.len() != 0 {
                    let mut saved_values = vec![];
                    for value in arg_values.iter().rev() {
                        saved_values.push(value.clone());
                    }
                    saved_values.push(receiver_value.clone());

                    return Err(ErrorInfo {
                        message: format!(
                            "Function working_directory requires 0 arguments, but got: {}",
                            args.len()
                        ),
                        restore_values: saved_values,
                        error_position: position.clone(),
                    });
                }

                // TODO: when we have a userland result type, use that.
                let path = std::env::current_dir().unwrap_or_default();

                stack_frame
                    .evalled_values
                    .push((position.clone(), Value::String(path.display().to_string())));
            }
        },
        v => {
            let mut saved_values = vec![];
            for value in arg_values.iter().rev() {
                saved_values.push(value.clone());
            }
            saved_values.push(receiver_value.clone());

            return Err(ErrorInfo {
                error_position: receiver_value.0,
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
            if let Some((done_children, Expression(expr_position, expr_))) =
                stack_frame.exprs_to_eval.pop()
            {
                if session.interrupted.load(Ordering::SeqCst) {
                    session.interrupted.store(false, Ordering::SeqCst);
                    restore_stack_frame(
                        env,
                        stack_frame,
                        (done_children, Expression(expr_position, expr_)),
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
                                error_position: position,
                            }) = eval_if(
                                &mut stack_frame,
                                &expr_position,
                                &condition.0,
                                then_body,
                                else_body,
                            ) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *condition.clone()));
                        }
                    }
                    Expression_::While(condition, ref body) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position: position,
                            }) = eval_while(
                                &mut stack_frame,
                                &condition.0,
                                Expression(expr_position.clone(), expr_copy.clone()),
                                body,
                            ) {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
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
                                .push((true, Expression(expr_position, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Assign(variable, expr) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position: position,
                            }) = eval_assign(&mut stack_frame, &variable)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Let(variable, expr) => {
                        if done_children {
                            if let Err(ErrorInfo {
                                message,
                                restore_values,
                                error_position: position,
                            }) = eval_let(&mut stack_frame, &variable)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::MalformedExpression),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *expr.clone()));
                        }
                    }
                    Expression_::Stop(e) => {
                        stack_frame
                            .exprs_to_eval
                            .push((false, Expression(expr_position, Expression_::Stop(e))));
                        env.stack.push(stack_frame);
                        return Err(EvalError::Stop(e));
                    }
                    Expression_::IntLiteral(i) => {
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::Integer(i)));
                    }
                    Expression_::BoolLiteral(b) => {
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::Boolean(b)));
                    }
                    Expression_::StringLiteral(s) => {
                        stack_frame
                            .evalled_values
                            .push((expr_position, Value::String(s)));
                    }
                    Expression_::ListLiteral(items) => {
                        if done_children {
                            let mut list_values: Vec<Value> = Vec::with_capacity(items.len());
                            for _ in 0..items.len() {
                                list_values.push(stack_frame.evalled_values.pop().expect(
                                    "Value stack should have sufficient items for the list literal",
                                ).1);
                            }

                            stack_frame
                                .evalled_values
                                .push((expr_position, Value::List(list_values)));
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));

                            for item in items.iter() {
                                stack_frame.exprs_to_eval.push((false, item.clone()));
                            }
                        }
                    }
                    Expression_::Variable(name) => {
                        if let Some(value) = get_var(&name.1, &stack_frame, env) {
                            stack_frame.evalled_values.push((expr_position, value));
                        } else {
                            restore_stack_frame(
                                env,
                                stack_frame,
                                (done_children, Expression(expr_position, expr_copy)),
                                &[],
                                Some(ErrorKind::MalformedExpression),
                            );
                            return Err(EvalError::ResumableError(
                                name.0.clone(),
                                format!("Undefined variable: {}.", name.1 .0),
                            ));
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
                                error_position: position,
                            }) = eval_integer_binop(&mut stack_frame, &expr_position, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
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
                                error_position: position,
                            }) = eval_equality_binop(&mut stack_frame, &expr_position, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
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
                                error_position: position,
                            }) = eval_boolean_binop(&mut stack_frame, &expr_position, op)
                            {
                                restore_stack_frame(
                                    env,
                                    stack_frame,
                                    (done_children, Expression(expr_position, expr_copy)),
                                    &restore_values,
                                    Some(ErrorKind::BadValue),
                                );
                                return Err(EvalError::ResumableError(position, message));
                            }
                        } else {
                            // TODO: do short-circuit evaluation of && and ||.
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));
                            stack_frame.exprs_to_eval.push((false, *rhs.clone()));
                            stack_frame.exprs_to_eval.push((false, *lhs.clone()));
                        }
                    }
                    Expression_::Call(receiver, ref args) => {
                        if done_children {
                            match eval_call(&mut stack_frame, &expr_position, args, session) {
                                Ok(Some(new_stack_frame)) => {
                                    env.stack.push(stack_frame);
                                    env.stack.push(new_stack_frame);
                                    continue;
                                }
                                Ok(None) => {}
                                Err(ErrorInfo {
                                    message,
                                    restore_values,
                                    error_position: position,
                                }) => {
                                    restore_stack_frame(
                                        env,
                                        stack_frame,
                                        (done_children, Expression(expr_position, expr_copy)),
                                        &restore_values,
                                        // TODO: let ErrorInfo specify
                                        // kind, as not all errors on
                                        // calling functions are this
                                        // kind.
                                        Some(ErrorKind::MalformedExpression),
                                    );
                                    return Err(EvalError::ResumableError(position, message));
                                }
                            }
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));

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
                    Expression_::Block(exprs) => {
                        if done_children {
                            stack_frame.exit_block();
                        } else {
                            stack_frame
                                .exprs_to_eval
                                .push((true, Expression(expr_position, expr_copy)));

                            stack_frame.enter_block();
                            for expr in exprs.iter().rev() {
                                stack_frame.exprs_to_eval.push((false, expr.clone()));
                            }
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
        .expect("Should have a value from the last expression")
        .1)
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
                end_offset: 4,
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
                end_offset: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Let(
                Variable(
                    Position {
                        offset: 0,
                        end_offset: 0,
                        path: PathBuf::from("__test.gdn"),
                    },
                    VariableName("foo".into()),
                ),
                Box::new(Expression(
                    Position {
                        offset: 0,
                        end_offset: 0,
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
                end_offset: 0,
                path: PathBuf::from("__test.gdn"),
            },
            Expression_::Variable(Variable(
                Position {
                    offset: 0,
                    end_offset: 0,
                    path: PathBuf::from("__test.gdn"),
                },
                VariableName("foo".into()),
            )),
        )];
        eval_exprs(&exprs, &mut env).unwrap();
    }

    #[test]
    fn test_eval_multiple_exprs() {
        let exprs = parse_exprs_from_str("true; false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_eval_add() {
        let exprs = parse_exprs_from_str("1 + 2;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_list_literal() {
        let exprs = parse_exprs_from_str("[1 + 2, 3 * 4];").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(
            value,
            Value::List(vec![Value::Integer(3), Value::Integer(12)])
        );
    }

    #[test]
    fn test_eval_block() {
        let exprs = parse_exprs_from_str("{ let x = 1; x + 1; };").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_block_scope_should_not_leak() {
        let exprs = parse_exprs_from_str("{ let x = 1; }; x;").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_let() {
        let exprs = parse_exprs_from_str("let foo = true; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_eval_let_twice() {
        let exprs = parse_exprs_from_str("let foo = true; let foo = false;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env);
        assert!(value.is_err());
    }

    #[test]
    fn test_eval_if() {
        let exprs = parse_exprs_from_str("let foo = if (true) { 1; } else { 2; }; foo;").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_eval_if_block_scope() {
        let exprs = parse_exprs_from_str("if (true) { let x = 1; } x;").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_err());
    }

    #[test]
    fn test_eval_empty() {
        let mut env = Env::default();
        let value = eval_exprs(&[], &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_list_append() {
        let exprs = parse_exprs_from_str("list_append([1, 2], 3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
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
        let exprs = parse_exprs_from_str("string_length(\"abc\");").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(3));
    }

    #[test]
    fn test_eval_string_substring() {
        let exprs = parse_exprs_from_str("string_substring(\"abcdef\", 1, 3);").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
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

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
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

        let exprs = parse_exprs_from_str("f(123);").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
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

        let exprs = parse_exprs_from_str("f(1, 2);").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(2));
    }

    #[test]
    fn test_eval_while() {
        let exprs = parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1;}").unwrap();

        let mut env = Env::default();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Void);
    }

    #[test]
    fn test_eval_while_block_scope_does_not_leak() {
        let exprs =
            parse_exprs_from_str("let i = 0; while (i < 5) { i = i + 1; let x = 1; }").unwrap();

        let mut env = Env::default();
        assert!(eval_exprs(&exprs, &mut env).is_ok());
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

        let exprs = parse_exprs_from_str("let i = 0; id(i); i;").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
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

        let exprs = parse_exprs_from_str("f();").unwrap();
        let value = eval_exprs(&exprs, &mut env).unwrap();
        assert_eq!(value, Value::Integer(1));
    }

    #[test]
    fn test_display_value_for_string_with_doublequote() {
        let value = Value::String("foo \\ \" \n bar".into());
        assert_eq!(format!("{}", value), "\"foo \\\\ \\\" \\n bar\"");
    }
}
