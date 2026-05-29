# Design prototype: `Work`-stack vs `ExpressionState`

`work_stack_prototype.rs` is a standalone, runnable comparison of two encodings
for Garden's stackless tree-walking interpreter. It is **not** part of the cargo
build.

Run it with:

```
rustc --edition 2021 design/work_stack_prototype.rs -o /tmp/wsp && /tmp/wsp
```

It implements the same tiny language (`int`, `var`, binary ops, `assign`, `if`,
`while`, function calls) twice:

- **`machine_a`** mirrors the current design in `src/eval.rs`: an explicit stack
  of `(ExpressionState, Rc<Expr>)`. Each AST node is pushed back onto the work
  stack with an advanced state tag and re-dispatched through the per-node
  `match`. Loops need a third phase, so `PartiallyEvaluated` carries a
  `BlockState` sub-state (`WillRunBlock` / `DoneRunBlock` / `NotBlock`), exactly
  like the `While` arm of `eval_expr` and `eval_while_body`.

- **`machine_b`** is the proposed encoding: an explicit stack of `Work` items.
  `Work::Eval(e)` matches a node **once** and pushes concrete, self-describing
  continuations (`IfDecide`, `WhileCheck`, `BinOp`, `AssignTo`, `Apply`). There
  is no re-dispatch of finished nodes and no `BlockState`.

## What to compare

Look at the two `step` functions, specifically the `if` and `while` handling:

- In `machine_a`, `while` spans four `match` arms and threads `BlockState`
  through re-pushes of the same AST node. The `NotBlock` variant exists only to
  feed an `unreachable!()`.
- In `machine_b`, `while` is two tiny continuations (`Eval(While)` sets it up;
  `WhileCheck` re-pushes itself for the next iteration). `if` is `Eval` +
  `IfDecide`.

## What both preserve

Both keep their entire state on the heap (a `Vec` of frames, each with its own
work stack and value stack), so both:

- never recurse in the host (Rust) stack on user-level recursion — the harness
  runs `sum_to(200_000)`, a **non-tail** recursion that a naive recursive
  tree-walker would overflow on; and
- could be paused/resumed between any two steps (the property Garden relies on
  for `stop_at_expr_id`, tick limits, and `:resume`).

The harness asserts both machines produce identical results on every program.

This is illustrative only; it does not model bindings-block scoping, error
recovery / value restoration, or the `stop_at_expr_id` machinery — those are
orthogonal to the encoding question and would carry over unchanged.
