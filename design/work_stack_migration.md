# Migration sketch: `ExpressionState` → `Work` stack in `src/eval.rs`

Status: proposal / scoping. Pairs with the runnable prototype in
`work_stack_prototype.rs`.

This describes an **incremental** path to replace the
`(ExpressionState, Rc<Expression>)` work-stack encoding with a `Work`-item
encoding, keeping `cargo test reftest` green at every step. It is grounded in
the current code (line anchors are approximate and will drift).

## Invariants the migration must preserve

The encoding is load-bearing for more than "don't overflow". Each must survive:

1. **No host-stack growth on user recursion.** Calls return a new `StackFrame`
   to the trampoline (`eval.rs` ~7140); never recurse in Rust. *Unchanged* —
   `Work::Apply` returns a frame the same way.

2. **Pause/resume + eval-up-to.** `stop_at_expr_id` + `done_subexpressions()`
   let the loop stop right after a chosen expression completes and return its
   value (`eval.rs` ~7100; entry points in `cli_session.rs:147`,
   `json_session.rs:618`, and `eval.rs:7736`). This is the hardest constraint —
   see "Carrying SyntaxId" below.

3. **`break` / `continue` unwinding.** `eval_break` / `eval_continue`
   (`eval.rs:7214`, `7264`) pop `exprs_to_eval` until they find the enclosing
   `While` / `ForIn` *AST node*. The `Work` model has no such node on the stack,
   so loop continuations must be recognizable boundaries.

4. **`return` unwinding.** `Expression_::Return` (`eval.rs:6543`) drains the
   current frame's work and lets the trampoline pop the frame.

5. **Error recovery / resume.** `RestoreValues` + `restore_stack_frame`
   (`eval.rs:1581`) push popped values and the in-flight work item back so
   `:resume` can continue. Needs a `Work`-aware variant.

6. **Bindings-block discipline.** `if`/`match`/loops push a bindings block and
   pop it in their `EvaluatedSubexpressions` arm. Becomes an explicit
   `Work::PopBlock` (or a field on the completion item).

7. **Trace / profile output.** The loop prints `expr_state` + `expr.expr_`
   (`eval.rs` ~7075). Cosmetic; reprint the `Work` variant.

## The cross-cutting requirement: carrying `SyntaxId`

Today the *completing visit* of an expression is "the same `outer_expr` popped
with `done_subexpressions() == true`", so the loop knows both **which**
expression finished and **that** it finished. In the `Work` model the finishing
step is a distinct continuation (`BinOp`, `IfDone`, `Apply`, ...), which by
itself carries no expression identity.

Therefore **every completion `Work` variant must carry the originating
`SyntaxId` and the `value_is_used` flag.** The loop's stop-at check becomes:

```rust
// after running a Work item that completed expression `id`:
if env.stop_at_expr_id == Some(id) { return last_value; }
```

This replaces the 18 `expr_state.done_subexpressions()` sites. It is the main
source of new fields on `Work` variants, and the main correctness risk — get
the `SyntaxId` plumbing right per family and the rest is mechanical.

## Target `Work` enum (covers all of `Expression_`)

```rust
enum Work {
    /// One downward pass over a node: matched once, pushes continuations.
    Eval(Rc<Expression>),

    // completions (carry SyntaxId + value_is_used)
    BinOpDone(BinaryOperatorSymbol, SyntaxId, bool),
    AssignDone(Symbol, SyntaxId),
    AssignUpdateDone(Symbol, AssignUpdateKind, SyntaxId),
    LetDone(LetDestination, SyntaxId),
    ReturnDone(SyntaxId),                 // unwinds the frame
    AssertDone(SyntaxId, Position),
    DotAccessDone(Symbol, SyntaxId, bool),
    NamespaceAccessDone(Symbol, SyntaxId, bool, Position),
    TupleDone(usize, SyntaxId, bool),
    ListDone(usize, SyntaxId, bool),
    DictDone(usize, SyntaxId, bool),
    StructDone(TypeSymbol, Vec<Symbol>, SyntaxId, bool),

    // control flow with blocks
    IfDecide  { then_b: Block, else_b: Option<Block>, id: SyntaxId, used: bool },
    MatchDecide { cases: Rc<Vec<(Pattern, Block)>>, id: SyntaxId, used: bool },
    PopBlock,                             // balance a pushed bindings block

    // loops (recognizable unwinding boundaries — replaces BlockState)
    WhileNext { cond: Rc<Expression>, body: Block, id: SyntaxId, used: bool },
    ForInNext { dst: LetDestination, body: Block, id: SyntaxId, used: bool },

    // application: returns a new StackFrame to the trampoline
    Apply       { id: SyntaxId, arg_count: usize, used: bool, /* receiver on value stack */ },
    MethodApply { name: Symbol, id: SyntaxId, arg_count: usize, used: bool },

    // try/catch boundary marker for exception unwinding
    CatchBoundary { catch_sym: Symbol, catch_body: Block, id: SyntaxId, used: bool },

    /// Bridge during migration: delegate to the old evaluator for not-yet-
    /// ported node kinds. Deleted in the final phase.
    Legacy(ExpressionState, Rc<Expression>),
}
```

`Break`, `Continue`, `Variable`, `FunLiteral`, and the literals need no
completion variant — they finish inside their `Eval` arm.

## Why a `Legacy` bridge makes this incremental

Each node kind is **self-contained in its value-stack effect**: however it is
evaluated, it leaves the value stack in the same shape. So a migrated `if` and a
`Legacy` `while` can coexist on one stack. That lets us port one family at a
time behind a single switch:

- `exprs_to_eval` becomes `Vec<Work>`.
- The loop dispatches `Work::Legacy(s, e)` to today's `eval_expr`, and `Eval(e)`
  to the new `eval_work` dispatch.
- `eval_work`'s `Eval` arm starts by delegating *every* node kind to
  `push Work::Legacy(NotEvaluated, e)`. Then we move node kinds out of `Legacy`
  one family at a time.

`break`/`continue`/`return`/`restore` must understand *both* representations
until the relevant family is ported (they already scan AST nodes; they grow a
parallel check for the new `Work` boundary markers).

## Phased plan (each phase ends green on `cargo test reftest`)

**Phase 0 — plumbing, no behavior change.**
- Add `enum Work` with only `Legacy` + `Eval`. Retype `exprs_to_eval`.
- Update the loop, `restore_stack_frame`, trace/profile printing, and the three
  entry points to push `Work::Eval(expr)` instead of `(NotEvaluated, expr)`.
- `Eval` always delegates to `Legacy(NotEvaluated, ...)`. Pure refactor.

**Phase 1 — leaves.** `IntLiteral`, `FloatLiteral`, `StringLiteral`,
`Variable`, `FunLiteral`, `Invalid`. Single-shot; the stop-at check fires
immediately in `Eval`. Lowest risk; validates the `SyntaxId` plumbing.

**Phase 2 — fixed-arity compounds.** `BinaryOperator`, `Assign`,
`AssignUpdate`, `Let`, `Assert`, `DotAccess`, `NamespaceAccess`, `Parentheses`,
plus the literal aggregates (`Tuple`/`List`/`Dict`/`Struct`). Push children
then a `*Done` completion. No unwinding interaction.

**Phase 3 — `return`.** `ReturnDone` drains the frame's work to the frame
boundary (the trampoline then pops the frame). Update the frame-pop path to be
agnostic to which encoding produced the value.

**Phase 4 — `if` / `match`.** `IfDecide` / `MatchDecide` + `PopBlock`. Removes
the first batch of `done_subexpressions()` sites and a chunk of `BlockState`
usage at the `if`/`match` arms.

**Phase 5 — loops + `break`/`continue` (do together).** `While`/`ForIn` become
`WhileNext`/`ForInNext`; teach `eval_break`/`eval_continue` to stop at those
markers (and drop the loop-bookkeeping values for `ForIn`, mirroring the
current value-stack discipline). **This phase deletes `BlockState`** and its
`unreachable!()`.

**Phase 6 — calls.** `Call`/`MethodCall` → `Apply`/`MethodApply` returning a
new `StackFrame`. Mechanically close to today's `eval_call` tail.

**Phase 7 — `try`/`catch`.** `CatchBoundary` marker; route exception unwinding
to it.

**Phase 8 — delete the bridge.** Remove `Work::Legacy`, `ExpressionState`,
`BlockState`, `done_subexpressions`, and the old `eval_expr` state `match`. The
loop now only knows `Work`.

## Scope / risk

- `eval.rs` is ~7.7k lines with ~120 `ExpressionState`/`BlockState` references;
  this is a multi-session refactor, not a single PR. Phases 0–2 and 4–5 are the
  bulk; each is independently shippable and reft­est-gated.
- Highest-risk areas: `SyntaxId` plumbing on completions (Phase 1 proves it),
  loop/`break`/`continue` value-stack discipline (Phase 5), and exception
  unwinding (Phase 7).
- Net result is *clearer*, not necessarily shorter: `BlockState` and the
  re-dispatch of finished nodes are gone, and "what remains to be done" is named
  on the stack. The essential machine (heap-resident frames, trampolined calls)
  is unchanged, so all six invariants above are preserved by construction.

## Suggested first commit

Phase 0 only — it is self-contained, behavior-preserving, and makes the
`Eval`/`Legacy` split visible so later phases are small diffs.
