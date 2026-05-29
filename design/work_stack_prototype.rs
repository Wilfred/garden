//! Prototype: comparing the `ExpressionState` encoding against a `Work`-stack
//! encoding for a stackless tree-walking interpreter.
//!
//! This is a standalone illustration, NOT wired into the cargo build. Compile
//! and run it directly:
//!
//!     rustc design/work_stack_prototype.rs -o /tmp/wsp && /tmp/wsp
//!
//! It implements the same tiny language twice:
//!
//!   * `machine_a` mirrors Garden's current design (src/eval.rs): an explicit
//!     stack of `(ExpressionState, Rc<Expr>)`, where each AST node is pushed
//!     back onto the work stack with an advanced state tag and re-dispatched
//!     through the big `match` on the node kind. Loops need a third phase, so
//!     `PartiallyEvaluated` carries a `BlockState` sub-state, exactly like
//!     `eval_while_body` / the `While` arm in `eval_expr`.
//!
//!   * `machine_b` is the proposed encoding: an explicit stack of `Work`
//!     items. `Work::Eval(e)` matches a node ONCE and pushes concrete,
//!     self-describing continuations (`IfDecide`, `WhileCheck`, `BinOp`, ...).
//!     There is no re-dispatch of finished nodes and no `BlockState`.
//!
//! Both machines keep their entire state on the heap (a `Vec` of frames, each
//! with its own work stack and value stack), so both:
//!
//!   * never recurse in the host (Rust) stack on user-level recursion, and
//!   * could be paused/resumed between any two `step`s (the property Garden
//!     relies on for `stop_at_expr_id`, tick limits and `:resume`).
//!
//! The `main` function runs identical programs through both and asserts they
//! agree, including a deeply-recursive, NON-tail program (`sum_to`) that a
//! naive recursive tree-walker would overflow on.

use std::collections::HashMap;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Shared AST and values
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
enum Value {
    Int(i64),
    Bool(bool),
    Unit,
}

impl Value {
    fn as_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            other => panic!("expected Bool, got {:?}", other),
        }
    }
    fn as_int(self) -> i64 {
        match self {
            Value::Int(i) => i,
            other => panic!("expected Int, got {:?}", other),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Eq,
    Lt,
}

impl Op {
    fn apply(self, l: Value, r: Value) -> Value {
        match self {
            Op::Add => Value::Int(l.as_int() + r.as_int()),
            Op::Sub => Value::Int(l.as_int() - r.as_int()),
            Op::Eq => Value::Bool(l.as_int() == r.as_int()),
            Op::Lt => Value::Bool(l.as_int() < r.as_int()),
        }
    }
}

type Block = Vec<Rc<Expr>>;

#[derive(Debug)]
enum ExprKind {
    Int(i64),
    Var(String),
    Bin(Op, Rc<Expr>, Rc<Expr>),
    Assign(String, Rc<Expr>),
    If(Rc<Expr>, Rc<Block>, Option<Rc<Block>>),
    While(Rc<Expr>, Rc<Block>),
    Call(String, Vec<Rc<Expr>>),
}

/// `value_used` mirrors Garden's `Expression::value_is_used`: when false, the
/// node is in statement position and must NOT leave a value on the value
/// stack. This is how both machines keep the value stack balanced across a
/// block without an explicit per-statement "drop" instruction.
#[derive(Debug)]
struct Expr {
    kind: ExprKind,
    value_used: bool,
}

struct Func {
    params: Vec<String>,
    body: Rc<Block>,
}

// --- builders (set `value_used` up front, like the parser does) ------------

fn int(i: i64, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::Int(i), value_used: used })
}
fn var(name: &str, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::Var(name.to_owned()), value_used: used })
}
fn bin(op: Op, l: Rc<Expr>, r: Rc<Expr>, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::Bin(op, l, r), value_used: used })
}
fn assign(name: &str, rhs: Rc<Expr>, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::Assign(name.to_owned(), rhs), value_used: used })
}
fn if_(cond: Rc<Expr>, then_b: Block, else_b: Option<Block>, used: bool) -> Rc<Expr> {
    Rc::new(Expr {
        kind: ExprKind::If(cond, Rc::new(then_b), else_b.map(Rc::new)),
        value_used: used,
    })
}
fn while_(cond: Rc<Expr>, body: Block, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::While(cond, Rc::new(body)), value_used: used })
}
fn call(name: &str, args: Vec<Rc<Expr>>, used: bool) -> Rc<Expr> {
    Rc::new(Expr { kind: ExprKind::Call(name.to_owned(), args), value_used: used })
}

/// Push a block's statements onto a work stack so the first statement runs
/// first (LIFO => push in reverse). Each statement carries its own
/// `value_used`, so non-final statements leave nothing on the value stack.
/// Generic over the work-item type via the `wrap` closure.
fn schedule_block<W>(work: &mut Vec<W>, body: &Block, wrap: impl Fn(Rc<Expr>) -> W) {
    for stmt in body.iter().rev() {
        work.push(wrap(stmt.clone()));
    }
}

// ===========================================================================
// machine_a — the current `ExpressionState` + `BlockState` encoding
// ===========================================================================
mod machine_a {
    use super::*;

    /// Mirrors src/eval.rs:150 `ExpressionState`.
    #[derive(Debug, Clone, Copy)]
    enum ExpressionState {
        NotEvaluated,
        PartiallyEvaluated(BlockState),
        EvaluatedSubexpressions,
    }

    /// Mirrors src/eval.rs:138 `BlockState`. Exists only because a 3-state tag
    /// cannot express the >2 phases that loops need. `NotBlock` is never
    /// constructed here (it feeds the `unreachable!()` arm below), just as in
    /// the real code — a sign the encoding is being stretched.
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)]
    enum BlockState {
        WillRunBlock,
        DoneRunBlock,
        NotBlock,
    }

    type WorkItem = (ExpressionState, Rc<Expr>);

    struct Frame {
        bindings: HashMap<String, Value>,
        work: Vec<WorkItem>,
        values: Vec<Value>,
        caller_uses_value: bool,
    }

    pub fn run(funcs: &HashMap<String, Func>, main_body: &Block) -> Value {
        let mut base = Frame {
            bindings: HashMap::new(),
            work: vec![],
            values: vec![],
            caller_uses_value: true,
        };
        schedule_block(&mut base.work, main_body, |e| (ExpressionState::NotEvaluated, e));
        let mut stack = vec![base];

        loop {
            let Some((state, expr)) = stack.last_mut().unwrap().work.pop() else {
                // Frame finished: pop it and hand its value back to the caller.
                let mut done = stack.pop().unwrap();
                let ret = done.values.pop().unwrap_or(Value::Unit);
                match stack.last_mut() {
                    None => return ret,
                    Some(caller) => {
                        if done.caller_uses_value {
                            caller.values.push(ret);
                        }
                    }
                }
                continue;
            };

            if let Some(new_frame) = step(&mut stack, funcs, state, expr) {
                stack.push(new_frame);
            }
        }
    }

    /// One evaluation step. Returns a new frame when a call is made (the
    /// trampoline pushes it), exactly like `eval_expr` returning
    /// `Ok(Some(StackFrame))`.
    fn step(
        stack: &mut [Frame],
        funcs: &HashMap<String, Func>,
        state: ExpressionState,
        expr: Rc<Expr>,
    ) -> Option<Frame> {
        let frame = stack.last_mut().unwrap();
        let used = expr.value_used;
        match &expr.kind {
            ExprKind::Int(i) => {
                if used {
                    frame.values.push(Value::Int(*i));
                }
            }
            ExprKind::Var(name) => {
                if used {
                    let v = *frame.bindings.get(name).expect("unbound variable");
                    frame.values.push(v);
                }
            }
            ExprKind::Bin(op, l, r) => match state {
                ExpressionState::NotEvaluated => {
                    frame.work.push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
                    frame.work.push((ExpressionState::NotEvaluated, r.clone()));
                    frame.work.push((ExpressionState::NotEvaluated, l.clone()));
                }
                _ => {
                    let r = frame.values.pop().unwrap();
                    let l = frame.values.pop().unwrap();
                    let out = op.apply(l, r);
                    if used {
                        frame.values.push(out);
                    }
                }
            },
            ExprKind::Assign(name, rhs) => match state {
                ExpressionState::NotEvaluated => {
                    frame.work.push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
                    frame.work.push((ExpressionState::NotEvaluated, rhs.clone()));
                }
                _ => {
                    let v = frame.values.pop().unwrap();
                    frame.bindings.insert(name.clone(), v);
                    if used {
                        frame.values.push(Value::Unit);
                    }
                }
            },
            // --- the representative family: `if` ----------------------------
            ExprKind::If(cond, then_b, else_b) => match state {
                ExpressionState::NotEvaluated => {
                    frame.work.push((
                        ExpressionState::PartiallyEvaluated(BlockState::WillRunBlock),
                        expr.clone(),
                    ));
                    frame.work.push((ExpressionState::NotEvaluated, cond.clone()));
                }
                ExpressionState::PartiallyEvaluated(_) => {
                    // schedule the cleanup phase (always runs)
                    frame
                        .work
                        .push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
                    let b = frame.values.pop().unwrap().as_bool();
                    if b {
                        schedule_block(&mut frame.work, then_b, |e| {
                            (ExpressionState::NotEvaluated, e)
                        });
                    } else if let Some(else_b) = else_b {
                        schedule_block(&mut frame.work, else_b, |e| {
                            (ExpressionState::NotEvaluated, e)
                        });
                    }
                }
                ExpressionState::EvaluatedSubexpressions => {
                    // `if` with no `else` evaluates to Unit; matches eval.rs:6452.
                    if used && else_b.is_none() {
                        frame.values.push(Value::Unit);
                    }
                }
            },
            // --- the representative family: `while` (note BlockState) -------
            ExprKind::While(cond, body) => match state {
                ExpressionState::NotEvaluated => {
                    frame.work.push((
                        ExpressionState::PartiallyEvaluated(BlockState::WillRunBlock),
                        expr.clone(),
                    ));
                    frame.work.push((ExpressionState::NotEvaluated, cond.clone()));
                }
                ExpressionState::PartiallyEvaluated(BlockState::WillRunBlock) => {
                    let b = frame.values.pop().unwrap().as_bool();
                    if b {
                        frame.work.push((
                            ExpressionState::PartiallyEvaluated(BlockState::DoneRunBlock),
                            expr.clone(),
                        ));
                        schedule_block(&mut frame.work, body, |e| {
                            (ExpressionState::NotEvaluated, e)
                        });
                    } else {
                        frame
                            .work
                            .push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
                        if used {
                            frame.values.push(Value::Unit);
                        }
                    }
                }
                ExpressionState::PartiallyEvaluated(BlockState::DoneRunBlock) => {
                    // re-check the condition for the next iteration
                    frame.work.push((
                        ExpressionState::PartiallyEvaluated(BlockState::WillRunBlock),
                        expr.clone(),
                    ));
                    frame.work.push((ExpressionState::NotEvaluated, cond.clone()));
                }
                ExpressionState::PartiallyEvaluated(BlockState::NotBlock) => unreachable!(),
                ExpressionState::EvaluatedSubexpressions => { /* loop done */ }
            },
            // --- function application: returns a new frame, no Rust recursion
            ExprKind::Call(name, args) => match state {
                ExpressionState::NotEvaluated => {
                    frame
                        .work
                        .push((ExpressionState::EvaluatedSubexpressions, expr.clone()));
                    for arg in args.iter().rev() {
                        frame.work.push((ExpressionState::NotEvaluated, arg.clone()));
                    }
                }
                _ => {
                    return Some(make_call_frame(frame, funcs, name, args.len(), used));
                }
            },
        }
        None
    }

    fn make_call_frame(
        frame: &mut Frame,
        funcs: &HashMap<String, Func>,
        name: &str,
        argc: usize,
        used: bool,
    ) -> Frame {
        let func = funcs.get(name).expect("unknown function");
        let mut args: Vec<Value> = (0..argc).map(|_| frame.values.pop().unwrap()).collect();
        args.reverse();
        let bindings = func.params.iter().cloned().zip(args).collect();
        let mut new_frame = Frame {
            bindings,
            work: vec![],
            values: vec![],
            caller_uses_value: used,
        };
        schedule_block(&mut new_frame.work, &func.body, |e| (ExpressionState::NotEvaluated, e));
        new_frame
    }
}

// ===========================================================================
// machine_b — the proposed `Work`-stack encoding
// ===========================================================================
mod machine_b {
    use super::*;

    /// Self-describing continuations. Each names exactly the remaining work,
    /// so there is no re-dispatch through a node `match` and no `BlockState`.
    enum Work {
        Eval(Rc<Expr>),
        BinOp(Op, bool),           // operands on the value stack
        AssignTo(String, bool),    // rhs on the value stack
        IfDecide {
            then_b: Rc<Block>,
            else_b: Option<Rc<Block>>,
            used: bool,
        },
        // A while loop is two tiny continuations that re-push each other.
        WhileCheck {
            cond: Rc<Expr>,
            body: Rc<Block>,
            used: bool,
        },
        Apply {
            name: String,
            argc: usize,
            used: bool,
        },
    }

    struct Frame {
        bindings: HashMap<String, Value>,
        work: Vec<Work>,
        values: Vec<Value>,
        caller_uses_value: bool,
    }

    pub fn run(funcs: &HashMap<String, Func>, main_body: &Block) -> Value {
        let mut base = Frame {
            bindings: HashMap::new(),
            work: vec![],
            values: vec![],
            caller_uses_value: true,
        };
        schedule_block(&mut base.work, main_body, Work::Eval);
        let mut stack = vec![base];

        loop {
            let Some(work) = stack.last_mut().unwrap().work.pop() else {
                let mut done = stack.pop().unwrap();
                let ret = done.values.pop().unwrap_or(Value::Unit);
                match stack.last_mut() {
                    None => return ret,
                    Some(caller) => {
                        if done.caller_uses_value {
                            caller.values.push(ret);
                        }
                    }
                }
                continue;
            };

            if let Some(new_frame) = step(&mut stack, funcs, work) {
                stack.push(new_frame);
            }
        }
    }

    fn step(stack: &mut [Frame], funcs: &HashMap<String, Func>, work: Work) -> Option<Frame> {
        let frame = stack.last_mut().unwrap();
        match work {
            Work::Eval(expr) => {
                let used = expr.value_used;
                match &expr.kind {
                    ExprKind::Int(i) => {
                        if used {
                            frame.values.push(Value::Int(*i));
                        }
                    }
                    ExprKind::Var(name) => {
                        if used {
                            let v = *frame.bindings.get(name).expect("unbound variable");
                            frame.values.push(v);
                        }
                    }
                    ExprKind::Bin(op, l, r) => {
                        frame.work.push(Work::BinOp(*op, used));
                        frame.work.push(Work::Eval(r.clone()));
                        frame.work.push(Work::Eval(l.clone()));
                    }
                    ExprKind::Assign(name, rhs) => {
                        frame.work.push(Work::AssignTo(name.clone(), used));
                        frame.work.push(Work::Eval(rhs.clone()));
                    }
                    ExprKind::If(cond, then_b, else_b) => {
                        frame.work.push(Work::IfDecide {
                            then_b: then_b.clone(),
                            else_b: else_b.clone(),
                            used,
                        });
                        frame.work.push(Work::Eval(cond.clone()));
                    }
                    ExprKind::While(cond, body) => {
                        frame.work.push(Work::WhileCheck {
                            cond: cond.clone(),
                            body: body.clone(),
                            used,
                        });
                        frame.work.push(Work::Eval(cond.clone()));
                    }
                    ExprKind::Call(name, args) => {
                        frame.work.push(Work::Apply {
                            name: name.clone(),
                            argc: args.len(),
                            used,
                        });
                        for arg in args.iter().rev() {
                            frame.work.push(Work::Eval(arg.clone()));
                        }
                    }
                }
            }
            Work::BinOp(op, used) => {
                let r = frame.values.pop().unwrap();
                let l = frame.values.pop().unwrap();
                let out = op.apply(l, r);
                if used {
                    frame.values.push(out);
                }
            }
            Work::AssignTo(name, used) => {
                let v = frame.values.pop().unwrap();
                frame.bindings.insert(name, v);
                if used {
                    frame.values.push(Value::Unit);
                }
            }
            Work::IfDecide { then_b, else_b, used } => {
                let b = frame.values.pop().unwrap().as_bool();
                if b {
                    schedule_block(&mut frame.work, &then_b, Work::Eval);
                } else if let Some(else_b) = else_b {
                    schedule_block(&mut frame.work, &else_b, Work::Eval);
                } else if used {
                    frame.values.push(Value::Unit);
                }
            }
            Work::WhileCheck { cond, body, used } => {
                let b = frame.values.pop().unwrap().as_bool();
                if b {
                    // Run the body, then re-check (push the re-check first so
                    // it runs after the body). No DoneRunBlock phase needed.
                    frame.work.push(Work::WhileCheck {
                        cond: cond.clone(),
                        body: body.clone(),
                        used,
                    });
                    frame.work.push(Work::Eval(cond));
                    schedule_block(&mut frame.work, &body, Work::Eval);
                } else if used {
                    frame.values.push(Value::Unit);
                }
            }
            Work::Apply { name, argc, used } => {
                let func = funcs.get(&name).expect("unknown function");
                let mut args: Vec<Value> =
                    (0..argc).map(|_| frame.values.pop().unwrap()).collect();
                args.reverse();
                let bindings = func.params.iter().cloned().zip(args).collect();
                let mut new_frame = Frame {
                    bindings,
                    work: vec![],
                    values: vec![],
                    caller_uses_value: used,
                };
                schedule_block(&mut new_frame.work, &func.body, Work::Eval);
                return Some(new_frame);
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// Programs and the comparison harness
// ---------------------------------------------------------------------------

/// Program 1 (uses `while`):
///
///     i = 0
///     total = 0
///     while i < n { total = total + i; i = i + 1 }
///     total
fn while_program(n: i64) -> (HashMap<String, Func>, Block) {
    let body = vec![
        assign("total", bin(Op::Add, var("total", true), var("i", true), true), false),
        assign("i", bin(Op::Add, var("i", true), int(1, true), true), false),
    ];
    let main = vec![
        assign("i", int(0, true), false),
        assign("total", int(0, true), false),
        assign("n", int(n, true), false),
        while_(bin(Op::Lt, var("i", true), var("n", true), true), body, false),
        var("total", true),
    ];
    (HashMap::new(), main)
}

/// Program 2 (uses `if` + deep NON-tail recursion):
///
///     fun sum_to(n) { if n == 0 { 0 } else { n + sum_to(n - 1) } }
///     sum_to(N)
///
/// The recursive call sits inside `n + _`, so a naive recursive interpreter
/// keeps a host frame alive per level and overflows for large N. Both explicit
/// machines keep that pending `+` on the heap instead.
fn sum_to_program(n: i64) -> (HashMap<String, Func>, Block) {
    let cond = bin(Op::Eq, var("n", true), int(0, true), true);
    let then_b = vec![int(0, true)];
    let else_b = vec![bin(
        Op::Add,
        var("n", true),
        call("sum_to", vec![bin(Op::Sub, var("n", true), int(1, true), true)], true),
        true,
    )];
    let body = vec![if_(cond, then_b, Some(else_b), true)];

    let mut funcs = HashMap::new();
    funcs.insert(
        "sum_to".to_owned(),
        Func { params: vec!["n".to_owned()], body: Rc::new(body) },
    );
    let main = vec![call("sum_to", vec![int(n, true)], true)];
    (funcs, main)
}

fn check(label: &str, funcs: &HashMap<String, Func>, main: &Block, expected: Value) {
    let a = machine_a::run(funcs, main);
    let b = machine_b::run(funcs, main);
    assert_eq!(a, b, "{label}: machine_a {a:?} != machine_b {b:?}");
    assert_eq!(a, expected, "{label}: got {a:?}, expected {expected:?}");
    println!("{label:<28} machine_a = machine_b = {a:?}");
}

fn main() {
    // `while` loop.
    let (f, m) = while_program(100);
    check("while: sum 0..100", &f, &m, Value::Int(4950));

    // `if` + shallow recursion.
    let (f, m) = sum_to_program(10);
    check("if/recursion: sum_to(10)", &f, &m, Value::Int(55));

    // Deep NON-tail recursion: a recursive tree-walker would overflow here.
    let n = 200_000;
    let (f, m) = sum_to_program(n);
    check("if/recursion: sum_to(200k)", &f, &m, Value::Int(n * (n + 1) / 2));

    println!("\nAll programs agree across both encodings, including deep recursion.");
}
