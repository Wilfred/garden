# Instructions for repeating the Garden language review

Use this file as the task instructions for a fresh agent. Evaluate Garden as a
language someone could ship and maintain software with. Discover concrete
correctness problems and usability obstacles through small programs and targeted
experiments. Completing impressive applications is secondary to learning about
the language. Review the implementation where experiments suggest a problem;
do not turn the review into an implementation-fixing project unless asked.

## Start and preserve the baseline

1. Read the applicable `AGENTS.md` instructions and inspect the working tree.
   Leave unrelated changes alone. Do not change `CHANGELOG.md`.
2. Create a new review branch with a descriptive name. Commit and push useful
   checkpoints regularly, including minimized failures and provisional findings.
   Do not wait until the final report to publish progress. Honor any explicit
   user restrictions on branches, commits or network access.
3. Record the source commit, Garden/Rust versions, OS, build profile and relevant
   environment constraints. Build the current checkout with `cargo build`.
4. Inspect the prelude, built-in modules, manual sources and CLI help as a new
   user would. Garden executes top-level expressions; `main` is not special.
5. Choose a destination for this run's artifacts. Preserve the previous review
   and evidence before rerunning scripts that overwrite their output. Existing
   audits under [this directory](README.md) are reusable starting points, not
   expected conclusions or an exhaustive test plan.

For a repeat review, first form independent expectations from the current
language, then compare with [the earlier findings](review.md). Mark old findings
as reproduced, resolved, changed or unverified. Preserve their IDs when tracking
continuity; assign new IDs only to distinct findings. Do not assume old bugs or
old evidence still describe the new checkout.

## Work through varied small programs

Write Garden code for a JSON parser, hangman, HTTP header parser, Brainfuck
interpreter and simple fractal renderer. Add one or two bounded exercises that
stress different capabilities, such as CSV/config parsing, word frequencies,
a graph traversal or a small command-line file utility. Reuse a previous exercise
where appropriate, but also try fresh implementations and different approaches.

Act as programmers with different backgrounds: a console-game beginner, a
Python data programmer, a JavaScript application programmer, a Rust/C systems
programmer, a protocol/library author and a scientific programmer. Try natural
first attempts before adapting to Garden. Include ordinary mistakes, not only
carefully idiomatic code or artificial stress inputs.

For every project:

- State its intended behavior and boundaries. Record unsupported cases honestly;
  a JSON subset must not be described as a conforming JSON parser.
- Save failed first attempts as separate minimal probes, then adapt the working
  program. Keep failures out of the passing exercise suite.
- Record what worked well and what caused friction: syntax, standard-library
  discovery, diagnostics, warnings, type checking, iteration and debugging.
- Check, test and run it separately. Test meaningful normal and malformed inputs.
  Use deterministic tests around random/interactive code, plus console smoke
  checks for invalid input, duplicate input, game over and EOF where relevant.
- Format normal `.gdn` programs with Garden. Preserve deliberately malformed
  syntax and exact stress fixtures as inputs rather than trying to fix them.

Use the repository's commands, checking current CLI help for changes:

```sh
cargo build
./target/debug/garden check path/to/program.gdn
./target/debug/garden test path/to/program.gdn
./target/debug/garden run path/to/program.gdn
./target/debug/garden format --check path/to/program.gdn
```

Program flags follow `--`, for example `garden run game.gdn -- --play`.

## Audit warnings, documentation and migration mistakes

Evaluate warning usefulness, not just warning count. Look for missed bugs,
false positives, duplicate diagnostics, misleading locations and style advice
that obscures correctness errors. Determine whether a nonzero check exit means
an error or only warnings; inspect diagnostic severity, using `--json` if useful.

Exercise quickfixes with `check --fix --stdout` before applying anything to a
copy. Check and run the result, compare observable behavior, and check again for
remaining warnings. Test interacting/overlapping fixes, Unicode offsets, comments,
explicit annotations and side-effecting initializers. Verify that a successful
fix command actually leaves the source valid; do not infer this from exit 0.

Read the documentation before calling an unfamiliar behavior a bug. Measure
public API documentation and example coverage, but distinguish presence from
accuracy and execution. Try the example checker on both real pages and controls
that should fail. Check fence conventions, embedded tests, skipped examples,
edge-case contracts and agreement between source, reflection and generated docs.
Documentation suggestions should target the prelude/doc source that generates
the manual; do not add unrelated hand-written website pages.

Try mistakes familiar from other languages: foreign declaration keywords,
semicolons, `mut`, truthiness, assignment in conditions, string `+`, indexing,
range syntax, interpolation, nested patterns, `!`, missing calls and assuming
`main` runs automatically. Assess whether the diagnostic identifies the actual
mistake, shows the offending code and suggests a working Garden alternative.
A syntax difference is not itself an implementation defect.

## Review robustness first

User source and editor/client input must not cause a Rust panic, abort or hang.
Separate parser failures from interpreter failures; use `reftest-ast` to isolate
parsing, then compare `check` and `run` on the same saved input.

Cover incomplete declarations/tuples/patterns, every prefix of several plausible
programs, delimiters, Unicode identifiers/whitespace/punctuation, invalid escapes,
integer boundaries, nested syntax, malformed calls, built-in type collisions,
and `break`/`continue`/`return` inside nested expressions. Test exception handlers
and state cleanup after failures. Inspect relevant assertions, unchecked indexing,
`unwrap`/`expect` calls and parser progress loops to guide further experiments.
Do not count an internal assertion as a user-reachable bug without a reproducer.

Run stress cases in separate processes with short timeouts, bounded input and
output, and disabled core dumps where available. Clean up timed-out processes
and their children. Avoid host exhaustion, destructive filesystem experiments
or uncontrolled network services. A timeout is an observation to investigate,
not proof that every slow program loops forever. Minimize each failure and
record whether it occurs on invalid input or code that passes checking.

## Review the type checker next

Compare expected static constraints, actual diagnostics and execution. Focus on
programs that pass checking despite incompatible explicit types, and valid
programs rejected by the checker. Test:

- Generic structs/enums, inferred and annotated values, nested containers,
  repeated type variables, constructor payloads and field access.
- Method receiver constraints together with argument constraints; immutable
  updates must preserve the element types of retained values.
- Function parameter/result variance, including function-valued fields and
  enum payloads. Immutability alone does not establish covariance.
- Closures, contextual inference, empty containers, assignments, branch results,
  early returns, bottom types, scope boundaries and forward references.
- Imports and nominal type identity across modules, transitive body checking,
  cyclic imports and private exports.

A runtime exception alone does not establish a type-checker bug. A checked
program may intentionally throw or assert. Conversely, executing code already
rejected by checking tests the unchecked-runtime policy, not a new checker false
negative. Distinguish errors from warnings and compare with successful controls.

## Review missing bug-catching linters after type checking

Find type-correct mistakes that produce wrong results without useful warnings.
Prioritize discarded Results/immutable updates/comparisons, forgotten calls,
shadowed accumulators, dead stores, contradictory boolean conditions, duplicate
or subsumed branches, suspicious constant ranges and provable invalid operations.
Consider computed Float equality and precision-sensitive conversions separately
from ordinary representation behavior.

For every proposed lint, include a small example, its actual diagnostic output,
why the mistake is plausible and a nearby valid case that should remain quiet.
Identify false-positive tradeoffs and whether it belongs in default warnings,
an optional lint or documentation. Do not propose a linter already implemented,
or use a lint as a substitute for correcting unsound type checking.

## Finish with an open-ended blind-spot review

Look beyond the exercise programs: module isolation, stateful REPL/live edits,
resuming/cancellation, tooling protocols and EOF, resource/output limits,
Unicode normalization/graphemes, filesystem/process failure boundaries,
release/debug differences, platform behavior, packaging and compatibility.
Review existing tests and CI to see what they actually cover.

Test a few high-value boundaries with controls. Clearly label areas that remain
untested; do not report speculative risks as confirmed defects. Where practical,
use deterministic generated inputs or differential/property-style experiments
rather than only manually selected examples. Passing the existing suite does
not establish the absence of crashes or soundness failures.

## Preserve evidence and write the report

For each candidate, retain a minimal input and record the command, build profile,
stdin/arguments, exit status, stdout, stderr and timeout. For non-text protocol
input, save exact bytes or hex. Record seeds and bounds for generated cases.
Separate raw observations from explanations inferred by reading implementation.
Ensure evidence matches the committed fixture after formatting or editing it.

Seek at least **20 distinct improvement areas** on a fresh exploratory review,
continuing through all the passes above rather than stopping when the count is
reached. Do not inflate the count with equivalent inputs, repeated stack traces
or speculative gaps. If an improved version yields fewer supported findings,
report that honestly instead of inventing problems. A repeat run should also
report resolved findings, even if it discovers no new defect in an area.

Produce Markdown deliverables:

- `review.md`: the overall assessment, prioritized findings, per-project
  experiences, positive controls, documentation assessment, remaining blind
  spots and validation limits.
- `README.md`: commands, artifact layout, exercise scope and the distinction
  between a passing exercise verifier and observational failure-capturing audits.
- Minimal Garden programs, targeted replay scripts and machine-readable evidence.

For each finding, give a stable ID, classification, observed versus expected
behavior, practical impact, reproduction/evidence links and a bounded suggested
improvement. Group related failure shapes and explain overlap. A documented
choice can merit reconsideration without being called a broken implementation.

Order findings by production impact rather than discovery order or ID:

| Priority | Meaning |
| --- | --- |
| P0 | Release blockers: user-reachable host failures, broken type/module guarantees, incorrect accepted execution |
| P1 | High-value correctness, failure handling, validation and essential workflow/API work |
| P2 | Diagnostic clarity, contract/documentation gaps and everyday usability |
| P3 | Optional conveniences, stylistic preferences and hints with substantial false-positive tradeoffs |

Priorities are judgments supported by impact, reachability and evidence; not
every absent API or unfamiliar semantic rule blocks a production release.
Preserve useful design strengths alongside criticism. Link evidence directly
and explain partial implementations instead of overstating project completeness.

## Verify and publish

Run meaningful exercise tests and required repository checks. For this repository,
use `cargo fmt --check`, `cargo clippy` and `cargo test reftest` as applicable.
Investigate failures; distinguish source regressions from environmental effects.
Do not regenerate golden files merely to make a review run pass. Record any
environment adjustment and any checks not completed.

Validate Markdown links, finding IDs/counts and fixture/evidence consistency.
Review the diff for unrelated changes and accidental generated output. Keep
independent changes in concise, matter-of-fact commits. Push the final commits
and verify the remote branch head matches the local head; if publication fails,
report precisely which commits remain local. End with links to the report and
reproduction guide, the highest-priority conclusions, validation results and
material limitations. Do not claim the language has been fixed by this analysis.
