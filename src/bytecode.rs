//! Bytecode representation of Garden programs.
//!
//! Function bodies, test bodies and toplevel expressions are compiled
//! to a flat sequence of instructions before evaluation. Control flow
//! (loops, conditionals, `break`) becomes explicit jumps, so the
//! evaluator in `eval.rs` is a program-counter loop rather than a
//! tree walker.
//!
//! Every instruction keeps a reference to the source expression it
//! was compiled from, so runtime errors have positions, the evaluator
//! can stop when a requested expression finishes ("evaluate up to
//! cursor"), and sessions can skip the erroring expression.

use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parser::ast::{BinaryOperatorKind, Block, Expression, Expression_, SyntaxId};
use crate::values::{Value, Value_};

/// A placeholder jump target, patched before `compile` returns.
const PLACEHOLDER: u32 = u32::MAX;

#[derive(Debug, Clone)]
pub(crate) enum Op {
    /// Push a precomputed value, such as a literal.
    PushValue(Value),
    /// Look up the variable in `expr` and push its value (if used).
    LoadVar,
    /// Pop N values from the value stack.
    PopN(u32),
    /// Pop N bindings blocks.
    PopBlocks(u32),
    /// Push a bindings block, seeded with `bindings_next_block`.
    EnterBlock,
    /// Pop a bindings block.
    ExitBlock,
    Jump(u32),
    /// Pop the condition value and jump if it's False. The
    /// instruction's `expr` is the condition expression.
    JumpIfFalse(u32),
    /// Set up one `for` loop iteration. The stack has the loop index
    /// below the iteree value. Binds the loop variables for this
    /// iteration, or jumps to `end` when the iteree is exhausted.
    ForInNext {
        end: u32,
    },
    /// Pop the scrutinee and jump to the matching case, binding the
    /// pattern variables. `arms` is parallel to the cases in `expr`.
    MatchDispatch {
        arms: Vec<u32>,
    },
    /// Pop two values and apply the binary operator in `expr`.
    IntBinop,
    FloatBinop,
    EqualityBinop,
    BoolBinop,
    StringConcat,
    /// Duplicate the top two values, so a failing `assert` can print
    /// the operands of its comparison.
    Dup2,
    /// Pop arguments and receiver, then call. Pushes a new stack
    /// frame for userland functions.
    Call,
    /// Pop arguments and receiver, then call the method. Pushes a new
    /// stack frame for userland methods.
    MethodCall,
    /// Pop the bound value and bind the `let` destination in `expr`.
    FinishLet,
    /// Pop the value and assign the variable in `expr`.
    FinishAssign,
    /// Pop the value and apply `+=`/`-=` to the variable in `expr`.
    FinishAssignUpdate,
    /// Pop the element values and push the list.
    FinishList,
    /// Pop the key-value pairs and push the dict.
    FinishDict,
    /// Pop the item values and push the tuple.
    FinishTuple,
    /// Pop the field values and push the struct.
    FinishStruct,
    /// Pop the receiver and push the field value.
    FinishDotAccess,
    /// Pop the namespace value and push the item value.
    FinishNamespaceAccess,
    /// Push a closure capturing the current bindings.
    PushClosure,
    /// Pop the asserted value (and the duplicated comparison operands
    /// if the asserted expression is a binary operation) and raise an
    /// error if it's False.
    Assert,
    /// Finish the current stack frame. The frame's result is the last
    /// value pushed.
    Return,
    /// No-op. Used as a join point after conditionals and loops, so
    /// there is an instruction that marks the whole expression as
    /// evaluated.
    Finish,
    /// Evaluating syntactically invalid code raises an exception.
    Invalid,
}

#[derive(Debug, Clone)]
pub(crate) struct Instr {
    pub(crate) op: Op,
    /// The source expression this instruction was compiled from.
    pub(crate) expr: Rc<Expression>,
    /// True if this instruction finishes evaluating `expr`. Used to
    /// stop after a requested expression ("evaluate up to cursor").
    pub(crate) completes: bool,
    /// The index just after the last instruction compiled from
    /// `expr`, so a session can skip the rest of this expression.
    pub(crate) skip_to: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct Code {
    pub(crate) instrs: Vec<Instr>,
}

/// A partially executed [Code] sequence: bytecode along with the
/// current program counter.
#[derive(Debug, Clone)]
pub(crate) struct ChunkState {
    pub(crate) code: Rc<Code>,
    pub(crate) pc: usize,
}

impl ChunkState {
    pub(crate) fn new(code: Rc<Code>) -> Self {
        Self { code, pc: 0 }
    }
}

/// Compile a function, method or test body. The compiled code does
/// not push a bindings block: the stack frame's initial block holds
/// the parameters.
pub(crate) fn compile_body(block: &Block) -> Code {
    compile_exprs(&block.exprs)
}

/// Compile a sequence of expressions, such as toplevel expressions.
pub(crate) fn compile_exprs(exprs: &[Rc<Expression>]) -> Code {
    let mut compiler = Compiler {
        instrs: vec![],
        loops: vec![],
        block_depth: 0,
    };
    for expr in exprs {
        compiler.compile_expr(expr);
    }

    Code {
        instrs: compiler.instrs,
    }
}

/// Fetch the compiled code for `body` from the cache, compiling it if
/// necessary.
///
/// The cache is keyed by the syntax ID of the first body expression:
/// re-parsing a definition produces fresh IDs, so redefining a
/// function never reuses stale code.
pub(crate) fn compile_body_cached(
    cache: &mut FxHashMap<SyntaxId, Rc<Code>>,
    body: &Block,
) -> Rc<Code> {
    let Some(first_expr) = body.exprs.first() else {
        // An empty body compiles to no instructions, so caching is
        // pointless (and there's no expression ID to key on).
        return Rc::new(compile_body(body));
    };

    if let Some(code) = cache.get(&first_expr.id) {
        return Rc::clone(code);
    }

    let code = Rc::new(compile_body(body));
    cache.insert(first_expr.id, Rc::clone(&code));
    code
}

struct LoopCtx {
    /// The jump target of `continue`: the loop condition, or the
    /// instruction that sets up the next `for` iteration.
    head: u32,
    /// `for` loops keep the loop index and iteree on the value stack,
    /// which `break` must pop.
    is_for_in: bool,
    /// The bindings block depth just outside the loop body, so
    /// `break` and `continue` can pop the blocks they're inside.
    block_depth: usize,
    /// Jump instructions to patch with the loop's end.
    break_jumps: Vec<usize>,
}

struct Compiler {
    instrs: Vec<Instr>,
    loops: Vec<LoopCtx>,
    /// The number of `EnterBlock` instructions we're currently
    /// inside.
    block_depth: usize,
}

impl Compiler {
    fn emit(&mut self, op: Op, expr: &Rc<Expression>) -> usize {
        self.instrs.push(Instr {
            op,
            expr: Rc::clone(expr),
            completes: false,
            skip_to: PLACEHOLDER,
        });
        self.instrs.len() - 1
    }

    /// Mark the last emitted instruction as completing its
    /// expression.
    fn mark_completes(&mut self) {
        if let Some(instr) = self.instrs.last_mut() {
            instr.completes = true;
        }
    }

    fn next_idx(&self) -> u32 {
        self.instrs.len() as u32
    }

    fn patch_jump(&mut self, idx: usize) {
        let target = self.next_idx();
        match &mut self.instrs[idx].op {
            Op::Jump(t) | Op::JumpIfFalse(t) | Op::ForInNext { end: t } => *t = target,
            op => unreachable!("Tried to patch a non-jump instruction: {:?}", op),
        }
    }

    /// Compile a block that runs in its own bindings block, such as
    /// an `if` branch or a loop body. `owner` is the expression the
    /// block belongs to.
    ///
    /// If the block is empty and its value is used, it evaluates to
    /// Unit. Otherwise the block's value is whatever its last
    /// expression pushes.
    fn compile_inner_block(&mut self, block: &Block, value_is_used: bool, owner: &Rc<Expression>) {
        self.emit(Op::EnterBlock, owner);
        self.block_depth += 1;

        if block.exprs.is_empty() && value_is_used {
            self.emit(Op::PushValue(Value::unit()), owner);
        }
        for expr in &block.exprs {
            self.compile_expr(expr);
        }

        self.emit(Op::ExitBlock, owner);
        self.block_depth -= 1;
    }

    fn compile_expr(&mut self, expr: &Rc<Expression>) {
        let start = self.instrs.len();
        let used = expr.value_is_used;

        match &expr.expr_ {
            Expression_::IntLiteral(i) => {
                if used {
                    self.emit(Op::PushValue(Value::new(Value_::Int(*i))), expr);
                    self.mark_completes();
                }
            }
            Expression_::FloatLiteral(f) => {
                if used {
                    self.emit(
                        Op::PushValue(Value::new(Value_::Float(f.into_inner()))),
                        expr,
                    );
                    self.mark_completes();
                }
            }
            Expression_::StringLiteral(s) => {
                if used {
                    self.emit(Op::PushValue(Value::new(Value_::String(s.clone()))), expr);
                    self.mark_completes();
                }
            }
            Expression_::Variable(_) => {
                self.emit(Op::LoadVar, expr);
                self.mark_completes();
            }
            Expression_::FunLiteral(_) => {
                if used {
                    self.emit(Op::PushClosure, expr);
                    self.mark_completes();
                }
            }
            Expression_::Invalid => {
                self.emit(Op::Invalid, expr);
                self.mark_completes();
            }
            Expression_::Parentheses(paren) => {
                self.compile_expr(&paren.expr);
            }
            Expression_::ListLiteral(items) => {
                // Items are evaluated right-to-left, so popping N
                // values produces the list in source order.
                for item in items.iter().rev() {
                    self.compile_expr(&item.expr);
                }
                self.emit(Op::FinishList, expr);
                self.mark_completes();
            }
            Expression_::TupleLiteral(items) => {
                for item in items.iter().rev() {
                    self.compile_expr(item);
                }
                self.emit(Op::FinishTuple, expr);
                self.mark_completes();
            }
            Expression_::DictLiteral(pairs) => {
                // Pairs are evaluated right-to-left; within a pair,
                // the key is evaluated before the value.
                for kv in pairs.iter().rev() {
                    self.compile_expr(&kv.key);
                    self.compile_expr(&kv.value);
                }
                self.emit(Op::FinishDict, expr);
                self.mark_completes();
            }
            Expression_::StructLiteral(_, field_exprs) => {
                for (_, field_expr) in field_exprs.iter().rev() {
                    self.compile_expr(field_expr);
                }
                self.emit(Op::FinishStruct, expr);
                self.mark_completes();
            }
            Expression_::BinaryOperator(lhs, op_sym, rhs) => {
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                self.emit(binop_op(op_sym.kind), expr);
                self.mark_completes();
            }
            Expression_::Let(_, _, init) => {
                self.compile_expr(init);
                self.emit(Op::FinishLet, expr);
                self.mark_completes();
            }
            Expression_::Assign(_, init) => {
                self.compile_expr(init);
                self.emit(Op::FinishAssign, expr);
                self.mark_completes();
            }
            Expression_::AssignUpdate(_, _, init) => {
                self.compile_expr(init);
                self.emit(Op::FinishAssignUpdate, expr);
                self.mark_completes();
            }
            Expression_::Return(inner) => {
                match inner {
                    Some(inner) => self.compile_expr(inner),
                    None => {
                        // `return` is the same as `return Unit`.
                        self.emit(Op::PushValue(Value::unit()), expr);
                    }
                }
                self.emit(Op::Return, expr);
                self.mark_completes();
            }
            Expression_::DotAccess(recv, _) => {
                self.compile_expr(recv);
                self.emit(Op::FinishDotAccess, expr);
                self.mark_completes();
            }
            Expression_::NamespaceAccess(recv, _) => {
                self.compile_expr(recv);
                self.emit(Op::FinishNamespaceAccess, expr);
                self.mark_completes();
            }
            Expression_::Call(recv, paren_args) => {
                // The receiver is evaluated first, then arguments
                // right-to-left.
                self.compile_expr(recv);
                for arg in paren_args.arguments.iter().rev() {
                    self.compile_expr(&arg.expr);
                }
                self.emit(Op::Call, expr);
                self.mark_completes();
            }
            Expression_::MethodCall(recv, _, paren_args) => {
                self.compile_expr(recv);
                for arg in paren_args.arguments.iter().rev() {
                    self.compile_expr(&arg.expr);
                }
                self.emit(Op::MethodCall, expr);
                self.mark_completes();
            }
            Expression_::Assert(inner) => {
                match binop_for_assert(inner) {
                    Some((lhs, op_kind, rhs)) => {
                        // Evaluate the operands separately and
                        // duplicate them, so an assertion failure can
                        // show their values.
                        self.compile_expr(&lhs);
                        self.compile_expr(&rhs);
                        self.emit(Op::Dup2, expr);
                        self.emit(binop_op(op_kind), inner);
                        self.mark_completes();
                        self.emit(Op::Assert, expr);
                        self.mark_completes();
                    }
                    None => {
                        self.compile_expr(inner);
                        self.emit(Op::Assert, expr);
                        self.mark_completes();
                    }
                }
            }
            Expression_::If(cond, then_body, else_body) => {
                self.compile_expr(cond);
                let jif = self.emit(Op::JumpIfFalse(PLACEHOLDER), cond);

                match else_body {
                    Some(else_body) => {
                        self.compile_inner_block(then_body, used, expr);
                        let jend = self.emit(Op::Jump(PLACEHOLDER), expr);
                        self.patch_jump(jif);
                        self.compile_inner_block(else_body, used, expr);
                        self.patch_jump(jend);
                        self.emit(Op::Finish, expr);
                        self.mark_completes();
                    }
                    None => {
                        // Without an else, the whole `if` evaluates
                        // to Unit, so the branch's value is unused.
                        self.compile_inner_block(then_body, false, expr);
                        self.patch_jump(jif);
                        if used {
                            self.emit(Op::PushValue(Value::unit()), expr);
                        } else {
                            self.emit(Op::Finish, expr);
                        }
                        self.mark_completes();
                    }
                }
            }
            Expression_::While(cond, body) => {
                let head = self.next_idx();
                self.compile_expr(cond);
                let jif = self.emit(Op::JumpIfFalse(PLACEHOLDER), cond);

                let outer_depth = self.block_depth;
                self.emit(Op::EnterBlock, expr);
                self.block_depth += 1;
                self.loops.push(LoopCtx {
                    head,
                    is_for_in: false,
                    block_depth: outer_depth,
                    break_jumps: vec![],
                });

                for body_expr in &body.exprs {
                    self.compile_expr(body_expr);
                }

                self.emit(Op::ExitBlock, expr);
                self.block_depth -= 1;
                self.emit(Op::Jump(head), expr);

                let ctx = self.loops.pop().expect("Loop context should be present");

                self.patch_jump(jif);
                if used {
                    self.emit(Op::PushValue(Value::unit()), expr);
                }
                // `break` pushes its own Unit, so it jumps after the
                // Unit push above.
                for break_jump in ctx.break_jumps {
                    self.patch_jump(break_jump);
                }
                self.emit(Op::Finish, expr);
                self.mark_completes();
            }
            Expression_::ForIn(_, iteree, body) => {
                // The loop index and the iteree value live on the
                // value stack between iterations.
                self.emit(Op::PushValue(Value::new(Value_::Int(0))), expr);
                self.compile_expr(iteree);

                let head = self.next_idx();
                let next = self.emit(Op::ForInNext { end: PLACEHOLDER }, expr);

                let outer_depth = self.block_depth;
                self.emit(Op::EnterBlock, expr);
                self.block_depth += 1;
                self.loops.push(LoopCtx {
                    head,
                    is_for_in: true,
                    block_depth: outer_depth,
                    break_jumps: vec![],
                });

                for body_expr in &body.exprs {
                    self.compile_expr(body_expr);
                }

                self.emit(Op::ExitBlock, expr);
                self.block_depth -= 1;
                self.emit(Op::Jump(head), expr);

                let ctx = self.loops.pop().expect("Loop context should be present");

                self.patch_jump(next);
                if used {
                    self.emit(Op::PushValue(Value::unit()), expr);
                }
                for break_jump in ctx.break_jumps {
                    self.patch_jump(break_jump);
                }
                self.emit(Op::Finish, expr);
                self.mark_completes();
            }
            Expression_::Break => {
                match self.loops.last() {
                    Some(ctx) => {
                        let is_for_in = ctx.is_for_in;
                        let pops = self.block_depth - ctx.block_depth;
                        if pops > 0 {
                            self.emit(Op::PopBlocks(pops as u32), expr);
                        }
                        if is_for_in {
                            // Discard the loop index and iteree.
                            self.emit(Op::PopN(2), expr);
                        }
                        if used {
                            self.emit(Op::PushValue(Value::unit()), expr);
                        }
                        let jump = self.emit(Op::Jump(PLACEHOLDER), expr);
                        self.loops
                            .last_mut()
                            .expect("Loop context should be present")
                            .break_jumps
                            .push(jump);
                    }
                    None => {
                        // `break` outside a loop finishes the current
                        // stack frame.
                        if used {
                            self.emit(Op::PushValue(Value::unit()), expr);
                        }
                        self.emit(Op::Return, expr);
                    }
                }
                self.mark_completes();
            }
            Expression_::Continue => {
                match self.loops.last() {
                    Some(ctx) => {
                        let head = ctx.head;
                        let pops = self.block_depth - ctx.block_depth;
                        if pops > 0 {
                            self.emit(Op::PopBlocks(pops as u32), expr);
                        }
                        self.emit(Op::Jump(head), expr);
                    }
                    None => {
                        self.emit(Op::Return, expr);
                    }
                }
                self.mark_completes();
            }
            Expression_::Match(scrutinee, cases) => {
                self.compile_expr(scrutinee);
                let dispatch = self.emit(Op::MatchDispatch { arms: vec![] }, expr);

                let mut arm_starts: Vec<u32> = Vec::with_capacity(cases.len());
                let mut end_jumps: Vec<usize> = Vec::with_capacity(cases.len());
                for (_, case_block) in cases {
                    arm_starts.push(self.next_idx());
                    self.compile_inner_block(case_block, used, expr);
                    end_jumps.push(self.emit(Op::Jump(PLACEHOLDER), expr));
                }

                for end_jump in end_jumps {
                    self.patch_jump(end_jump);
                }
                match &mut self.instrs[dispatch].op {
                    Op::MatchDispatch { arms } => *arms = arm_starts,
                    _ => unreachable!(),
                }
                self.emit(Op::Finish, expr);
                self.mark_completes();
            }
            Expression_::Try(try_body, _, _) => {
                // The catch block is not yet evaluated at runtime.
                self.compile_inner_block(try_body, used, expr);
                self.mark_completes();
            }
        }

        // Any instruction of this expression (that isn't part of a
        // subexpression, which set their targets already) skips to
        // just after the expression.
        let end = self.next_idx();
        for instr in &mut self.instrs[start..] {
            if instr.skip_to == PLACEHOLDER {
                instr.skip_to = end;
            }
        }
    }
}

fn binop_op(kind: BinaryOperatorKind) -> Op {
    match kind {
        BinaryOperatorKind::Add
        | BinaryOperatorKind::Subtract
        | BinaryOperatorKind::Multiply
        | BinaryOperatorKind::Divide
        | BinaryOperatorKind::Modulo
        | BinaryOperatorKind::Exponent
        | BinaryOperatorKind::BitwiseAnd
        | BinaryOperatorKind::BitwiseOr
        | BinaryOperatorKind::LessThan
        | BinaryOperatorKind::LessThanOrEqual
        | BinaryOperatorKind::GreaterThan
        | BinaryOperatorKind::GreaterThanOrEqual => Op::IntBinop,
        BinaryOperatorKind::AddFloat
        | BinaryOperatorKind::SubtractFloat
        | BinaryOperatorKind::MultiplyFloat
        | BinaryOperatorKind::DivideFloat => Op::FloatBinop,
        BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => Op::EqualityBinop,
        BinaryOperatorKind::And | BinaryOperatorKind::Or => Op::BoolBinop,
        BinaryOperatorKind::StringConcat => Op::StringConcat,
    }
}

pub(crate) fn binop_for_assert(
    expr: &Rc<Expression>,
) -> Option<(Rc<Expression>, BinaryOperatorKind, Rc<Expression>)> {
    match &expr.expr_ {
        Expression_::BinaryOperator(lhs, op_sym, rhs) => {
            Some((Rc::clone(lhs), op_sym.kind, Rc::clone(rhs)))
        }
        _ => None,
    }
}
