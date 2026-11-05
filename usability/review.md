# Garden usability and production readiness review

Garden supports small programs with readable tests, useful diagnostics and clear
immutable state transitions. It is not ready for production use under the
criterion that user programs and editor input must never panic or hang the Rust
host. The most urgent findings concern interpreter/parser robustness, generic
type soundness, module isolation, and incomplete exception handling.

This review contains **79 improvement candidates**, ordered by expected benefit
for shipping a reliable language. They include confirmed bugs, warning and
documentation gaps, and deliberately lower-priority design/API requests. They
are not 79 independent root causes. Related reproductions are grouped, and
existing finding IDs are retained so earlier commits remain traceable; IDs are
not priority numbers. No language implementation changes were made.

| Priority | Release significance | Candidates |
| --- | --- | ---: |
| P0 | Release blockers: host failures, broken type/module guarantees, nonworking accepted syntax | 19 |
| P1 | High-value correctness, failure reporting, validation and essential API work | 24 |
| P2 | Diagnostics, documented semantics, usable APIs and documentation clarity | 32 |
| P3 | Optional convenience and migration hints with false-positive tradeoffs | 4 |

The first release gate should be: no host panic/hang on bounded input, sound
type arguments and variance, module-local type identity, and reliable checking
of called imported code. High-signal warnings should come before style warnings.
Documented eager booleans and equal precedence are design decisions to evaluate,
not misclassified implementation bugs.

## Prioritized findings

### P0 — Fix before claiming production readiness

#### F57 — Continue inside an argument corrupts the loop stack

`for x in [1] { println(string_repr((x, continue))) }` passes check but run panics because the next loop index is a function value. Pending argument values survive continue. Unwind intermediate values together with expressions/bindings. The saved continue_argument.gdn is a small runtime-stack regression case.

Evidence: [continue_argument.gdn](robustness/continue_argument.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F61 — Generic struct type arguments are erased during inference

Inferred generic struct literals lose all type arguments. Box{ value: "wrong" } is accepted as `Box<Int>`, including in an explicitly annotated binding. Generic field arithmetic also passes check before failing at runtime. infer_struct_literal solves field variables but returns args: []; subtyping zips argument lists without checking equal length, accepting the empty list against any instantiation. Preserve solved args and check type-argument arity. See generic_struct_bad, generic_struct_annotation_bad, generic_struct_flow and wrong_struct_field_read in type_checker/.

Evidence: [generic_struct_annotation_bad.gdn](type_checker/generic_struct_annotation_bad.gdn) · [wrong_struct_field_read.gdn](type_checker/wrong_struct_field_read.gdn) · [captured check/run output](evidence/type_checker.json).

#### F62 — Method inference drops constraints from the receiver

Method inference overwrites a type variable solved from the receiver with the argument's type, then validates arguments without rechecking the receiver. Thus [1].append("wrong") and Dict["n" => 1].set("s", "wrong") pass check as homogeneous String containers; reading the original Int fails at runtime. User generic methods behave the same even with an annotated `Box<Int>` receiver. Keep receiver constraints during unification, validate conflicts and preserve existing collection element metadata. See append_then_read, dict_then_read, list_metadata_retag and generic_receiver_bound.

Evidence: [append_then_read.gdn](type_checker/append_then_read.gdn) · [dict_then_read.gdn](type_checker/dict_then_read.gdn) · [generic_receiver_bound.gdn](type_checker/generic_receiver_bound.gdn) · [captured check/run output](evidence/type_checker.json).

#### F63 — Callback-bearing types receive unsound covariance

Immutable structs/enums are assumed covariant in every parameter, including parameters used as callback arguments. `Handler<List<NoValue>>` is accepted as `Handler<List<Int>>` although its stored function only accepts an empty list. A checked program invokes it with [1] and fails an empty-list assertion. Immutability alone does not justify covariance through a function's argument position. Infer variance from parameter occurrences or use invariance initially. See both function_field_variance and function_enum_variance.

Evidence: [function_field_variance.gdn](type_checker/function_field_variance.gdn) · [function_enum_variance.gdn](type_checker/function_enum_variance.gdn) · [captured check/run output](evidence/type_checker.json).

#### F75 — Same-named types interfere across module namespaces

Importing two namespaces that independently declare Record with different fields causes one definition to overwrite the other's type behavior. Both entry programs check cleanly; construction in the first module fails against the other module's field type. Reversing import order reverses the affected module. One module alone works. Key nominal type identity by defining module, not just the textual name.

Evidence: [module_collision.gdn](broad/module_collision.gdn) · [reverse_module_collision.gdn](broad/reverse_module_collision.gdn) · [captured check/run output](evidence/broad.json).

#### F53 — Non-ASCII source text panics in the lexer

An unsupported non-ASCII source character, such as é in café, a smart quote, or a BOM, is sliced as &s[0..1]. Rust panics on an invalid UTF-8 boundary instead of producing a Garden diagnostic. Unicode whitespace takes a different one-byte advance path and also panics. See unicode_identifier/whitespace/bom/smart_quote.gdn.

Evidence: [unicode_identifier.gdn](robustness/unicode_identifier.gdn) · [unicode_whitespace.gdn](robustness/unicode_whitespace.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F54 — Incomplete tuples and patterns panic in the parser

Incomplete `(1,` and `match x { Some((a` trigger forward-progress assertions, in different tuple/pattern parsing loops. These are normal partial editor states; return an incomplete-input diagnostic rather than asserting progress.

Evidence: [unfinished_tuple.gdn](robustness/unfinished_tuple.gdn) · [unfinished_pattern.gdn](robustness/unfinished_pattern.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F55 — Incomplete enums hang the parser

`enum E { A,` never finishes parsing within the two-second bound. This can freeze editing, checking and execution before any Garden code runs. Recover or stop at EOF in enum variant parsing.

Evidence: [unfinished_enum.gdn](robustness/unfinished_enum.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F56 — Small deeply nested files abort the host process

2,048 parentheses or 1,024 nested lists abort with Rust stack overflow. The files are only a few kilobytes. Apply explicit nesting limits or iterative parsing/traversal; report a Garden error.

Evidence: [deep_parentheses.gdn](robustness/deep_parentheses.gdn) · [deep_lists.gdn](robustness/deep_lists.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F59 — Built-in type collisions reach zero-argument panic paths

Redefining List or Float as a struct passes check; calling the retained built-in len/floor method panics while indexing the empty argument-position vector in its receiver-type error. Reserve built-in type names or isolate namespace-local types, and use receiver_pos for receiver errors in zero-argument methods. See spoof_*.

Evidence: [spoof_list.gdn](robustness/spoof_list.gdn) · [spoof_float.gdn](robustness/spoof_float.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F30 — Overlapping quickfixes panic in preview mode

`check --fix --stdout` panics on `b || b || b` (84). Check emits three duplicate-expression warnings, including two for the same occurrence. Applying overlapping deletions uses invalid byte offsets and exits 101. Deduplicate diagnostics and edits, and reject/merge conflicting changes before applying a batch. This failure happens even in preview mode.

Evidence: [84_repeated_bool.gdn](probes/84_repeated_bool.gdn) · [fixes.json](evidence/fixes.json).

#### F76 — Tooling transports panic on malformed frames

An LSP Content-Length of usize::MAX panics with capacity overflow before reading a body. The JSON-session transport also panics on malformed lengths, truncated bodies and non-UTF-8 payloads. Bound frame lengths before allocating, validate decoding and return transport errors rather than expect/unwrap. Inputs are captured as exact hex bytes in evidence/broad.json; these tests open no network listeners.

Evidence: [broad.json](evidence/broad.json) · [audit_broad.py](audit_broad.py).

#### F03 — Compound integer overflow panics

Int += overflow panics in the debug Rust interpreter (exit 101), while + wraps. Probes 14/15. Make compound and ordinary arithmetic consistent and keep Garden failures within Garden's diagnostic machinery.

Evidence: [14_overflow_update.gdn](probes/14_overflow_update.gdn) · [15_overflow_add.gdn](probes/15_overflow_add.gdn) · [subtract_overflow.gdn](robustness/subtract_overflow.gdn) · [captured check/run output](evidence/probes.json).

#### F20 — Minimum integer division panics

minimum Int divided by -1 panics in Rust (30), unlike division by zero's Garden exception. This is a division overflow case distinct from the compound-assignment inconsistency in finding 03.

Evidence: [30_division_overflow.gdn](probes/30_division_overflow.gdn) · [captured check/run output](evidence/probes.json).

#### F01 — Empty separators hang split

`"abc".split("")` never terminates. Probe 02 times out. Reject an empty separator or define a terminating character-split behavior.

Evidence: [02_split_empty.gdn](probes/02_split_empty.gdn) · [captured check/run output](evidence/probes.json).

#### F02 — Empty patterns hang replace

`"abc".replace("", "-")` never terminates. Probe 03. This is a separate operation with its own progress loop, not just split's diagnostic. Define insertion semantics or reject the argument.

Evidence: [03_replace_empty.gdn](probes/03_replace_empty.gdn) · [captured check/run output](evidence/probes.json).

#### F60 — Try and catch parse but do not catch failures

Parsed `try { throw("expected") } catch(e) { ... }` never runs its catch body. Assertions are not caught either. The evaluator has a Try node but propagates errors immediately instead of unwinding to a handler. Implement catch behavior or reject the incomplete feature.

Evidence: [catch_exception.gdn](robustness/catch_exception.gdn) · [catch_assertion.gdn](robustness/catch_assertion.gdn) · [captured parser/runtime output](evidence/robustness.json).

#### F74 — Checking an entry file misses imported body errors

Checking an entry file accepts a called imported function declared Int whose body returns String. Checking the imported module itself reports the error. Check the reachable module graph, with caching/cycle handling, or make the single-file boundary explicit and provide a project-wide check command. See broad/imported_type_error.

Evidence: [imported_type_error.gdn](broad/imported_type_error.gdn) · [broken.gdn](broad/modules/broken.gdn) · [captured check/run output](evidence/broad.json).

#### F58 — Break in a loop iterator panics during execution

`for x in break { ... }` is rejected by check but run panics on an empty binding stack. Even unchecked invalid programs need a Garden error rather than a host assertion. See for_break_iterator.gdn.

Evidence: [for_break_iterator.gdn](robustness/for_break_iterator.gdn) · [captured parser/runtime output](evidence/robustness.json).

### P1 — Prioritize correctness and reliable workflows

#### F64 — Runtime type enforcement differs between equivalent constructs

Ordinary function arguments and annotated bindings enforce concrete types, but method arguments, enum payloads, reassignment and generic return types can bypass equivalent checks. Methods accepting an Int can receive an unused String argument; Text(42) constructs a declared String payload; identity<T> can return a String for T=Int at runtime. Their static errors are correctly caught, so this is not another checker false negative. Choose a coherent checked/unchecked execution policy; runtime checks or a mandatory preflight should enforce the promised boundary. These cases matter for REPL/live replacement too.

Evidence: [wrong_unused_method_argument.gdn](type_checker/wrong_unused_method_argument.gdn) · [constructor_payload_bad.gdn](type_checker/constructor_payload_bad.gdn) · [generic_return_bad.gdn](type_checker/generic_return_bad.gdn) · [captured check/run output](evidence/type_checker.json).

#### F77 — JSON sessions keep running after stdin EOF

The JSON session does not terminate on stdin EOF; the two-second audit times out after its greeting. Its read loop treats a zero-byte EOF read as an empty line and continues indefinitely. LSP's equivalent EOF control exits normally. Handle EOF and join/stop workers.

Evidence: [broad.json](evidence/broad.json) · [audit_broad.py](audit_broad.py).

#### F46 — Extension returns a name with no dot as an extension

extension() returns Some("README") for a filename with no dot (92), contradicting the contract requiring a dot. Dotfiles and trailing dots also need an explicit policy/examples.

Evidence: [92_path_extension.gdn](probes/92_path_extension.gdn) · [captured check/run output](evidence/probes.json).

#### F47 — Path parent loses the root and contradicts its docs

parent() on /file returns Some(Path{ p: "" }), losing the root (93). The doc prose says parent always returns a Path and root is returned as-is, but its signature/example/runtime return None for root. Correct the absolute single-component result and contradictory prose.

Evidence: [93_root_parent.gdn](probes/93_root_parent.gdn) · [__prelude.gdn](../src/__prelude.gdn) · [captured check/run output](evidence/probes.json).

#### F48 — Path joining can turn a relative path absolute

joining "" with "file" produces absolute /file; joining /tmp with /etc/config yields /tmp//etc/config (94). The one-line doc "Append component" leaves empty-base and absolute-component behavior unspecified. Preserve relative paths and define absolute-argument handling.

Evidence: [94_path_join.gdn](probes/94_path_join.gdn) · [captured check/run output](evidence/probes.json).

#### F21 — Remainder overflow reports the wrong divisor

minimum Int % -1 says "dividing ... by zero" although the divisor is -1 (31). It should return the representable remainder 0 or accurately describe the unsupported case.

Evidence: [31_remainder_overflow.gdn](probes/31_remainder_overflow.gdn) · [captured check/run output](evidence/probes.json).

#### F06 — Discarded Results hide failed operations

An ignored fs::write_file Err gets no warning; a program prints "saved" after an unsuccessful write (13). Consider a discarded-Result check.

Evidence: [13_unused_result.gdn](probes/13_unused_result.gdn) · [captured check/run output](evidence/probes.json).

#### F23 — Discarded immutable updates silently do nothing

discarded immutable append/set return values silently do nothing (37/52); no warning even when the original variable is read afterward. This is distinct from ignoring a Result error: a mutation- looking operation succeeds, but the update is lost. Consider warning for unused returned collection values or making names more explicit.

Evidence: [37_ignored_immutable.gdn](probes/37_ignored_immutable.gdn) · [52_dict_ignored_set.gdn](probes/52_dict_ignored_set.gdn) · [captured check/run output](evidence/probes.json).

#### F04 — Captured assignments silently leave outer state unchanged

assigning a captured variable inside a map callback changes a captured copy, leaving the outer value 0 without warning (11). Explain capture-by-value in tooling or warn on misleading captured writes.

Evidence: [11_mutable_capture.gdn](probes/11_mutable_capture.gdn) · [51_true_counter.gdn](probes/51_true_counter.gdn) · [captured check/run output](evidence/probes.json).

#### F65 — Discarded comparisons can hide assignment mistakes

`n == 2` as a statement passes check and leaves n unchanged (forgot_assignment). Warn when a comparison result is discarded; this is a common assignment/comparison typo and has no useful side effect in this simple example. Do not automatically replace == with =.

Evidence: [forgot_assignment.gdn](linters/forgot_assignment.gdn) · [captured check/run output](evidence/linters.json).

#### F66 — Bare function references can hide forgotten calls

A bare `save` function reference is silently discarded (forgot_call), so no save occurs and the program prints "finished". Warn on unused callable values where the likely intent was `save()`.

Evidence: [forgot_call.gdn](linters/forgot_call.gdn) · [captured check/run output](evidence/linters.json).

#### F67 — Shadowed accumulator updates need targeted warnings

`let total = total + n` inside a loop shadows rather than updates the outer accumulator, which remains 0 (shadow_update). Prefer a targeted shadowing warning when the initializer reads the same outer name and that outer value is used after the loop; ordinary useful shadowing should remain quiet.

Evidence: [shadow_update.gdn](linters/shadow_update.gdn) · [captured check/run output](evidence/linters.json).

#### F68 — Simple boolean contradictions get no warning

`(n != 1) || (n != 2)` is always True, while `(n == 1) && (n == 2)` is always False. Both pass check, as does `allowed || True`. Catch simple same-variable contradictions and constant operands, preserving side effects because Garden booleans are eager.

Evidence: [always_true_or.gdn](linters/always_true_or.gdn) · [impossible_and.gdn](linters/impossible_and.gdn) · [captured check/run output](evidence/linters.json).

#### F69 — Duplicate and subsumed conditional branches get no warning

duplicate `else if n == 1` and an n >= 20 branch after n >= 10 get no warning (duplicate_branch/subsumed_branch). Extend the useful unreachable-match guidance to repeated/subsumed if conditions; start with pure comparisons, not an unrestricted theorem prover.

Evidence: [duplicate_branch.gdn](linters/duplicate_branch.gdn) · [subsumed_branch.gdn](linters/subsumed_branch.gdn) · [captured check/run output](evidence/linters.json).

#### F71 — Constant invalid operations pass static checks

Literal division by zero, negative integer exponents, invalid byte constants and or_throw on a known empty first() all pass static checking. Check simple constant preconditions in normal code and point to the exact argument (constant_*, bytes_constant_invalid, unsafe_first). Avoid warning merely because a dynamic operation can fail, or suppress in deliberately expected-failure tests if those become supported.

Evidence: [constant_zero_division.gdn](linters/constant_zero_division.gdn) · [bytes_constant_invalid.gdn](linters/bytes_constant_invalid.gdn) · [unsafe_first.gdn](linters/unsafe_first.gdn) · [captured check/run output](evidence/linters.json).

#### F34 — Overwritten initial values get no warning

A local's initial value overwritten before any read gets no warning (80). The variable name is used, so an unused-variable check does not catch the likely mistaken initialization or forgotten branch.

Evidence: [80_dead_store.gdn](probes/80_dead_store.gdn) · [captured check/run output](evidence/probes.json).

#### F32 — Fix mode reports success with unresolved syntax errors

--fix exits 0 without diagnostics for syntax errors that it cannot fix, including Python def and semicolons (59/66). Output is unchanged and still fails check/run. Automation cannot treat successful fixing as successful checking. Report remaining diagnostics or document/provide an option for a failing exit status when issues remain.

Evidence: [66_semicolons.gdn](probes/66_semicolons.gdn) · [fixes.json](evidence/fixes.json).

#### F44 — Documentation examples with title-only fences are skipped

run-code-blocks skips title-only fences used by the keyword pages. fun and let report zero checked blocks. A title-only fence containing assert(False) passes; `garden title:Example` fails as expected. Align the checker with the website's code-fence convention, and report skipped Garden-looking examples. evidence/doc_examples.json captures both.

Evidence: [doc_examples.json](evidence/doc_examples.json) · [run_code_blocks.rs](../src/run_code_blocks.rs).

#### F45 — Documentation checks do not execute embedded tests

A fenced `test incorrect { assert(False) }` is counted as a checked block and passes without executing the test. The fun page's embedded test therefore has no effect even if its fence is recognized. Execute contained tests or distinguish loaded examples from tested ones.

Evidence: [doc_examples.json](evidence/doc_examples.json).

#### F05 — Trim documents more whitespace than it removes

String.trim only removes ASCII space, despite documenting whitespace. Tabs and newlines survive (01). HTTP needs a local trim_ows. Clarify the contract or offer whitespace and explicit-character variants.

Evidence: [01_trim_tabs.gdn](probes/01_trim_tabs.gdn) · [captured check/run output](evidence/probes.json).

#### F08 — String lacks checked Float parsing

String has as_int but no as_float (06). JSON decimal/exponent values cannot use the obvious symmetric conversion. Add checked parsing.

Evidence: [06_float_parse.gdn](probes/06_float_parse.gdn) · [captured check/run output](evidence/probes.json).

#### F13 — Float ordering is unavailable

Float has no ordering operators, including <. (05/23). Arithmetic has useful dedicated operators but a renderer cannot directly express radius_squared < 4.0. Add ordering with a clear NaN policy.

Evidence: [05_float_compare.gdn](probes/05_float_compare.gdn) · [23_float_order.gdn](probes/23_float_order.gdn) · [captured check/run output](evidence/probes.json).

#### F09 — No codepoint to character conversion

no integer-to-character conversion (07; prelude API inspected). Escaped JSON Unicode and Brainfuck character output need a workaround.

Evidence: [07_char_decode.gdn](probes/07_char_decode.gdn) · [captured check/run output](evidence/probes.json).

#### F28 — Text and bytes have no encoding bridge

String has no bytes conversion (56), and the API inventory has no UTF-8 encode/decode pair. __fs has byte file I/O, but protocol and interpreter code cannot bridge it to text without a manual encoding implementation or external process. This is separate from a single-codepoint constructor (09).

Evidence: [56_string_bytes.gdn](probes/56_string_bytes.gdn) · [captured check/run output](evidence/probes.json).

### P2 — Improve everyday usability and clarify contracts

#### F07 — Equal operator precedence surprises ordinary expressions

all binary operators have equal precedence. During the header parser, i != lines.len() - 1 grouped as (i != lines.len()) - 1. Even when the checker rejects it, the message doesn't explain grouping. Probe 29 captures arithmetic and mixed comparison. Standard precedence or a warning on mixed operators would reduce surprises.

Evidence: [29_precedence.gdn](probes/29_precedence.gdn) · [captured check/run output](evidence/probes.json).

#### F10 — Field assignment has an unhelpful error

field assignment s.n = 1 gives "Expected an expression after this" at n (09). Immutable state is viable, but this deserves an explicit message suggesting construction of a replacement struct.

Evidence: [09_field_update.gdn](probes/09_field_update.gdn) · [captured check/run output](evidence/probes.json).

#### F11 — Lists lack an element replacement API

indexed list assignment is unavailable (08), and List has no replacement method. Tape algorithms must rebuild a list or use a dict. A persistent list set(index, value) would suit the current value semantics.

Evidence: [08_list_update.gdn](probes/08_list_update.gdn) · [55_list_set.gdn](probes/55_list_set.gdn) · [captured check/run output](evidence/probes.json).

#### F12 — Nested enum patterns produce cascading errors

nested enum patterns Ok(Some(n)) produce seven parser errors (25). Nested matches work, but a precise unsupported-pattern message would help; supporting nested patterns would make Result/Option-heavy parsers smaller.

Evidence: [25_nested_pattern.gdn](probes/25_nested_pattern.gdn) · [captured check/run output](evidence/probes.json).

#### F14 — Scientific numeric literals are not recognized clearly

scientific literals such as 1e-3 are not recognized; errors ask for commas/parentheses instead of identifying the numeric syntax (20). Common scientific and JSON examples need rewriting.

Evidence: [20_scientific_literal.gdn](probes/20_scientific_literal.gdn) · [captured check/run output](evidence/probes.json).

#### F16 — Dictionary keys are restricted to strings

Dict keys must be strings (32). Brainfuck bracket addresses require string_repr conversions on every lookup. Integer or generic keys would also help graph algorithms and frequency counters.

Evidence: [32_dict_integer_key.gdn](probes/32_dict_integer_key.gdn) · [captured check/run output](evidence/probes.json).

#### F17 — Sorting cannot use strings or a comparator

no generic/comparator sort and no string ordering (21/33; prelude exposes only sort_nums). Sorting names or scored records requires implementing both a comparison strategy and sorting. A sort_by API is a smaller useful step than a general ordering/typeclass framework.

Evidence: [21_string_order.gdn](probes/21_string_order.gdn) · [33_custom_sort.gdn](probes/33_custom_sort.gdn) · [captured check/run output](evidence/probes.json).

#### F18 — Callback parameter inference loses receiver context

map(fun(x) { x + 1 }) checks x as Any and rejects it, despite the receiver `List<Int>` and successful execution (39). The same expression works when x: Int is written explicitly. Use callback context in lambda parameter inference, or suggest the missing annotation.

Evidence: [39_inference.gdn](probes/39_inference.gdn) · [captured check/run output](evidence/probes.json).

#### F19 — Boolean guards eagerly evaluate both operands

&& and || eagerly evaluate both sides (47/49). The guarded first().or_throw() on an empty list throws (50). Follow-up review confirmed both operator pages explicitly document this. This is a request for familiar short-circuit behavior or guard-oriented tooling guidance, not a claim that the documented semantics are broken.

Evidence: [50_boolean_guard.gdn](probes/50_boolean_guard.gdn) · [operator:ampamp.md](../website/operator:ampamp.md) · [operator:pipepipe.md](../website/operator:pipepipe.md) · [captured check/run output](evidence/probes.json).

#### F22 — Console EOF is encoded as an error string

read_line represents normal EOF with Err("End of input.") (35). Console apps cannot distinguish EOF from I/O failure by type; `Result<Option<String>, ...>` would make that distinction explicit.

Evidence: [35_eof.gdn](probes/35_eof.gdn) · [captured check/run output](evidence/probes.json).

#### F24 — Unsupported tuple access causes extra parser errors

unsupported tuple access gets a helpful destructuring suggestion followed by an unrelated missing-comma error (24). Preserve the good first message and reduce cascades after recognized unsupported constructs. Nested patterns (12) are another example of this issue.

Evidence: [24_tuple_access.gdn](probes/24_tuple_access.gdn) · [captured check/run output](evidence/probes.json).

#### F25 — Dictionary size requires materializing items

Dict lacks len/is_empty (22). Getting its size requires creating the full items list and taking len. Add a direct size query.

Evidence: [22_dict_length.gdn](probes/22_dict_length.gdn) · [captured check/run output](evidence/probes.json).

#### F26 — Debug string representations contain literal tabs

string_repr leaves tabs literal (58), unlike newline/CR escaping. Tabs make parser diagnostics and expected values hard to read and copy reliably. Escape tabs as \t in representations.

Evidence: [58_tab_repr.gdn](probes/58_tab_repr.gdn) · [captured check/run output](evidence/probes.json).

#### F27 — Basic floating point math APIs are missing

no Float.sqrt (54) or transcendental functions in the prelude. Escape-time rendering works, but smooth coloring, distances, rotations need common math functions.

Evidence: [54_float_sqrt.gdn](probes/54_float_sqrt.gdn) · [captured check/run output](evidence/probes.json).

#### F29 — Duplicate field runtime errors name the wrong problem

A duplicate struct field is correctly detected by check, but run says Point does not have a field named x, even though it does (44). Retain the precise duplicate-field diagnosis at runtime.

Evidence: [44_duplicate_field.gdn](probes/44_duplicate_field.gdn) · [captured check/run output](evidence/probes.json).

#### F31 — Quickfixes leave predictable follow-up warnings

An unused-literal binding becomes a bare unused literal after one --fix invocation (48/87), requiring another check/fix round. Keeping an effectful initializer is correct (90), but known pure literals could be removed in one step or remaining warnings could be reported.

Evidence: [48_unused_local.gdn](probes/48_unused_local.gdn) · [fixes.json](evidence/fixes.json).

#### F33 — Same-return warnings flag useful side-effecting branches

The same-literal-return warning triggers for deliberate status-code returns after different logging side effects (82). It is true that both return 0, but different branches still matter. Consider making this a configurable style hint rather than an unavoidable failing check.

Evidence: [82_same_literal_sideeffects.gdn](probes/82_same_literal_sideeffects.gdn) · [captured check/run output](evidence/probes.json).

#### F35 — Tautological length comparisons get no warning

List.len() >= 0 is accepted silently and always True (81). Catch impossible/tautological length comparisons, not just empty-list spellings. This is useful for users confusing >= with >.

Evidence: [81_len_nonnegative.gdn](probes/81_len_nonnegative.gdn) · [captured check/run output](evidence/probes.json).

#### F36 — Foreign function declarations need targeted guidance

def/function/fn declarations are treated as expressions or variable references, yielding misplaced punctuation or unbound-symbol errors (59/64/65). Recognize common declaration prefixes and suggest `fun greet(name: String) { ... }` rather than cascading through the body.

Evidence: [59_python_def.gdn](probes/59_python_def.gdn) · [64_js_function.gdn](probes/64_js_function.gdn) · [65_rust_fn.gdn](probes/65_rust_fn.gdn) · [captured check/run output](evidence/probes.json).

#### F37 — Foreign conditional keywords need targeted guidance

`elif` is an unknown variable (63), and Python `and` reports a missing brace at True (109). Suggest `else if` and `&&`, with a reminder of eager evaluation where appropriate. A known foreign keyword deserves a more actionable message than a generic unbound-symbol diagnostic.

Evidence: [63_python_elif.gdn](probes/63_python_elif.gdn) · [109_python_and.gdn](probes/109_python_and.gdn) · [captured check/run output](evidence/probes.json).

#### F38 — Rust mut is misread as a type hint

`let mut x = 1` reports a missing colon before a type hint (77). Say Garden bindings can be reassigned without `mut`, rather than implying the author should write a type annotation after `mut`.

Evidence: [77_mut.gdn](probes/77_mut.gdn) · [captured check/run output](evidence/probes.json).

#### F39 — Rust ranges receive tuple destructuring advice

`for i in 0..3` suggests tuple destructuring (70). The problem is the Rust-style range. Suggest `range(0, 3)` and leave tuple-access guidance for actual tuple expressions.

Evidence: [70_rust_range.gdn](probes/70_rust_range.gdn) · [captured check/run output](evidence/probes.json).

#### F40 — Logical negation errors omit the working alternative

`if !False` only says unrecognized `!` (72). Point to `not(False)`, just as String + already points to ^. Semicolons (66) could similarly suggest removal instead of only rejecting the character.

Evidence: [72_bang_not.gdn](probes/72_bang_not.gdn) · [captured check/run output](evidence/probes.json).

#### F41 — List indexing errors omit the get API

`items[0]` reports missing comma/parenthesis (69). This is distinct from the earlier list-update gap: ordinary reads have a working API, `items.get(0)`, and the message could explain its Option result.

Evidence: [69_index_read.gdn](probes/69_index_read.gdn) · [captured check/run output](evidence/probes.json).

#### F43 — Empty containers get stuck at NoValue

binding [] then reassigning an append result fails check with `List<NoValue>` versus `List<String>`, while execution works (98). Suggest annotating `let names: List<String> = []`; ideally infer from use. This is a separate inference limitation from lambda parameters (18).

Evidence: [98_empty_option_inference.gdn](probes/98_empty_option_inference.gdn) · [empty_dict_inference.gdn](type_checker/empty_dict_inference.gdn) · [captured check/run output](evidence/probes.json).

#### F49 — Division and modulo need a joint signed-arithmetic contract

division truncates toward zero but modulo is Euclidean: -4 / 3 is -1 and -4 % 3 is 2, so (a / b) * b + (a % b) != a (96). Both rules are individually documented; their interaction is not explained. Cross-link the pages with this counterexample or provide div_euclid.

Evidence: [96_divmod_sign.gdn](probes/96_divmod_sign.gdn) · [operator:slash.md](../website/operator:slash.md) · [operator:%.md](../website/operator:%.md) · [captured check/run output](evidence/probes.json).

#### F51 — Result lacks success-value mapping

Result has no map (105); routine success-value adaptation needs repeated match arms, separate from early-return propagation (15). Option/Result combinators would support concise parser composition.

Evidence: [105_result_map.gdn](probes/105_result_map.gdn) · [captured check/run output](evidence/probes.json).

#### F52 — Whitespace tokenization needs a standard API

no split_whitespace (102). Replacing tabs/newlines and filtering empty strings works, but every text-processing script must choose and implement its own whitespace policy. A documented tokenizer would pair well with clarifying trim's existing ASCII-space behavior (05).

Evidence: [102_string_split_whitespace.gdn](probes/102_string_split_whitespace.gdn) · [captured check/run output](evidence/probes.json).

#### F70 — Computed Float equality needs actionable guidance

`(0.1 +. 0.2) == 0.3` silently produces False (float_equality). Float's doc already warns against computed exact equality. An opt-in lint for equality involving arithmetic would turn that advice into actionable feedback; retain exact comparison for intentional sentinel/representation checks. An approximate-comparison helper is also needed to make the recommendation easy to follow.

Evidence: [float_equality.gdn](linters/float_equality.gdn) · [captured check/run output](evidence/linters.json).

#### F73 — Integer to Float docs omit precision loss

9007199254740993.as_float().floor() loses one (float_precision_loss). That is normal binary64 rounding, not an arithmetic bug. Int.as_float's doc does not explain the loss of exactness outside the safe integer range. Add a boundary example or checked exact conversion for callers who need round trips.

Evidence: [float_precision_loss.gdn](linters/float_precision_loss.gdn) · [captured check/run output](evidence/linters.json).

#### F78 — Reflection omits supported bitwise operators

& and | execute as integer bitwise operators but are absent from reflect::operators(), which claims to list all operators and drives the manual's operator links. Keep token recognition, reflection and documentation inventories consistent. See reflect_inventory.

Evidence: [reflect_inventory.gdn](broad/reflect_inventory.gdn) · [captured check/run output](evidence/broad.json).

#### F79 — Try and catch have no keyword documentation

try/catch are returned as keywords and parsed, but have no keyword pages. Other keywords have pages or redirect stubs. Explain support status, error type and catchable failures when the feature is implemented; until then reject it or explicitly mark it experimental.

Evidence: [broad.json](evidence/broad.json) · [build_site.gdn](../website/build_site.gdn).

### P3 — Consider optional conveniences after correctness

#### F15 — Result propagation requires repeated matches

there is no Result propagation shorthand (10). The JSON parser repeats match/Err return plumbing at every recursive step. This is a design request, not an incorrect rejection of valid Garden syntax.

Evidence: [10_try_result.gdn](probes/10_try_result.gdn) · [captured check/run output](evidence/probes.json).

#### F42 — Interpolation lookalikes silently print literal placeholders

"Hello ${name}" runs and prints the placeholder literally (68), with no warning despite an in-scope name. A conservative optional interpolation-lookalike hint could suggest ^ concatenation.

Evidence: [68_string_interpolation.gdn](probes/68_string_interpolation.gdn) · [captured check/run output](evidence/probes.json).

#### F50 — Common collection combinators are absent

no fold, reverse or zip (101/103/104), forcing loops and indexing boilerplate in ordinary aggregate/pairwise tasks. This is an API request, not a demand for iterator syntax or a claim of impossibility.

Evidence: [101_list_reduce.gdn](probes/101_list_reduce.gdn) · [103_list_reverse.gdn](probes/103_list_reverse.gdn) · [104_list_zip.gdn](probes/104_list_zip.gdn) · [captured check/run output](evidence/probes.json).

#### F72 — Reversed literal ranges silently do no work

range(5, 0) executes zero iterations without any diagnostic (descending_range). A hint on statically reversed endpoints can help Python/Rust users who expect descending iteration. Normal dynamic empty ranges should remain valid; a step-aware range API is a separate possible extension, not a precondition for this lint.

Evidence: [descending_range.gdn](linters/descending_range.gdn) · [captured check/run output](evidence/linters.json).

## Exercise experiences

Sources: [HTTP](http_headers.gdn), [hangman](hangman.gdn), [Brainfuck](brainfuck.gdn), [JSON](json_parser.gdn), [Mandelbrot](fractal.gdn), [CSV](csv.gdn).

### HTTP fields — protocol programmer

Working: ordered duplicate-preserving header list, colon splitting, ASCII name
validation, case normalization, explicit tab/space trimming, malformed-line
errors. This is a field-section exercise, not an HTTP message parser: no request
line, body framing, or byte-level control-character validation.

Good: split_once returns a destructurable Option tuple; chars and substring
agree on character offsets; enums and structural equality make tests readable.
The type checker caught a missing pair of parentheses in an expression combining
comparison and subtraction. That prevented wrong execution, but the diagnostic
only describes Int/Bool mismatches, not the unexpected grouping.

### Hangman — console-game beginner

Working: deterministic walkthrough plus --play interactive mode with a random
word; six-miss loss, repeated-letter validation, case normalization and win
handling. State transitions return `Result<Game, String>`, tested independently
from the console. Scope is deliberately ASCII English words.

Good: plain top-level expressions, tiny test blocks, structural equality and
returned immutable state make this approachable. Explicit Result matches make
all console failure paths visible. Rebuilding a Game for each guess is verbose
but easy to reason about. No special entry-point ceremony was needed.

### Brainfuck — systems programmer

Working: bracket prepass, nested loops, bounded tape, wrapping 8-bit cells,
byte input/output lists, EOF-as-zero and an instruction budget. The demo emits
[65]. Tests cover nested/skipped loops, wraparound, EOF, invalid brackets,
pointer bounds and nontermination. This is an interpreter API, not a terminal
character-output frontend.

Good: Euclidean modulo makes cell wrapping natural; Option bounds checks and
Result allow controlled errors instead of interpreter exceptions. Poor fit:
list replacement needs slice/append/concat, integer jump addresses need string
keys in Dict, and bytes cannot be directly decoded into a String.

### JSON — library author

Working subset: recursive arrays/objects, null/bools, exact numeric lexemes
including decimal/exponent grammar, decoded common string escapes, Unicode
literal text, trailing-input validation and a depth limit. Repeated object keys
are preserved. Tests cover nested data and malformed inputs.

Limits: explicitly rejects JSON \u, \b and \f escapes; it only rejects raw tab,
CR and LF among control characters, so this is not a conforming JSON parser.
Numeric lexemes avoid overflow/precision loss, but application code still lacks
a standard decimal conversion. No performance claims: repeated substring calls
rescan character offsets, so this style is unsuitable for large inputs.

Good: recursive enum variants and tuple destructuring make a compact syntax
tree possible. Forward references work. Bad: propagating Result requires
repeated Err(e) => return Err(e) arms; no nested enum patterns or ? shorthand.
Without ordinal/character conversion, standards-compliant string decoding
becomes a much larger project than the recursive grammar.

### Mandelbrot — scientific programmer

Working: ASCII 64 by 25 Mandelbrot escape-time render, tests for known points,
image dimensions and symmetry. Floating point arithmetic uses +., -., *. and
/.. To test squared magnitude < 4, the implementation compares floor(magnitude)
with integer 4. This is equivalent for the nonnegative bounded values used in
this renderer, but a workaround users should not need. render assumes width
and height greater than 1 and a positive iteration limit.

Good: explicit Int.as_float and Float.floor conversions, simple loops and
text output suffice to get a recognizable image. The arithmetic error for +
is unusually helpful. In contrast, float comparisons are impossible with
ordinary operators; <. is not syntax either. There is no sqrt/log/trigonometry
API in the prelude for smooth coloring or common other fractals.

### CSV — data-processing programmer

Working: explicit four-state parser for comma-separated rows, quoted commas,
quoted multiline fields, doubled quotes, CRLF or LF separators, empty fields,
and precise failures for malformed quoting. It preserves spaces and permits
ragged rows; schema/column-count validation is outside this exercise.

Good: enum state, straightforward loops, exhaustive matches and equality-based
tests were sufficient without framework code. No new language issue was needed
to implement this after learning the preceding constraints. Compared with the
recursive JSON parser, a local state machine is less affected by Result
propagation boilerplate. Character-by-character substring scans still impose
the same likely scaling limitation; this review has not benchmarked it.

## What already works well

- Typo and lowercase-boolean errors suggest the correct name and provide working fixes. String `+` points directly to `^`; Float `+` points to `+.`.
- Concrete argument diagnostics point to both the call and parameter declaration. Equality assertions show actual and expected containers.
- Exhaustive enum matches identify missing variants. Duplicate variants and arms after `_` get useful diagnostics.
- The unused-binding fix preserves an effectful initializer, and ordinary tuple-binding/final-return fixes preserve results. Smaller grouped boolean fixes work; the overlapping case is F30.
- An unmodified-local `while` condition gets an invariant-loop warning. `main` gets a particularly helpful explanation of top-level execution.
- Concrete callbacks, branch results and scopes are checked well in the control cases. Recursive enums, forward references, annotated empty containers, nested generic constructors and bounded cyclic imports work.
- Unicode string APIs consistently use codepoints. Private module access is rejected, and invalid byte/text file operations fail with Garden errors.

These successes are controls, not claims of general soundness. Checked programs
that fail because of F57/F61/F62/F63/F74/F75 are specifically distinguished from
intentional throws, failed assertions on ordinary input, and unchecked programs
that already failed static checking.

## Documentation coverage and clarity

The [reflection inventory](doc_inventory.gdn) finds doc comments for all **53
methods** and all **47 public functions** in the six built-in namespaces.
**41 methods** and **29 functions** contain fenced examples. Fourteen of fifteen
reflected types have doc comments; Tuple has a separate
[manual source](../website/type:Tuple.md), so this is not missing website content.
These counts measure doc/fence presence, not execution or assertion coverage.

The weakest areas are edge-case contracts and verification: skipped fences and
unexecuted tests (F44/F45), contradictory path prose (F47), omitted conversion
precision (F73), incomplete operator inventories (F78), and try/catch support
status (F60/F79). Both eager boolean pages explicitly explain eagerness; the
initial uncertainty about documentation was corrected. Division and modulo
are also individually documented; F49 requests an explanation of their joint
signed-arithmetic behavior rather than claiming neither rule is documented.

The `fun` page's unannotated arithmetic example is marked `nocheck`. It runs,
but checking reports `Any` where `Int` is needed. Explain this checker/runtime
boundary explicitly and connect it to contextual inference (F18/F43).

## Broad review and remaining blind spots

The module and transport probes found important issues outside the original
single-file exercises. Unicode controls also show that visually identical
precomposed/decomposed strings compare unequal and family emoji have multiple
codepoints. That follows the documented representation; normalization and
grapheme APIs remain an application-level need rather than a demonstrated bug.

The following areas need further work and are **not counted as confirmed defects**:

- **Stateful live tooling:** replacing definitions while closures/values survive, changing type layouts, resuming after errors, stale diagnostics, cancellation and memory retention.
- **Sustained protocols:** recovery after failed requests, framing limits, backpressure and cancellation latency across long sessions. The audit tested isolated stdin frames without network listeners.
- **Whole-pipeline budgets:** parser depth/CPU/output limits before evaluator sandbox budgets, plus memory quotas and reliable process shutdown.
- **Release/platform parity:** debug versus release arithmetic; Windows path/process behavior and macOS. CI runs Linux tests while release binaries target all three systems.
- **Scale:** long Unicode strings, persistent updates, recursion, many modules, large diagnostic output and cancellation. This review establishes hangs, not throughput benchmarks.
- **Filesystem failures:** disk full, interrupted writes, permission changes, symlinks, non-UTF-8 paths and concurrent writers. No claim is made that every case is broken.
- **Distribution and compatibility:** clean-machine/offline installation, reproducible packaging, dependency updates, standard-library evolution and language-version policy.
- **Generated testing:** bounded Unicode/token mutation, incremental editing and well-typed-program generation. The 198-case truncated-source corpus found failures despite passing parser reftests; compare checking/execution and debug/release behavior.

Linters need a false-positive policy. Early return from a loop may be a bug in a
sum but correct in a search; ignored `Err` arms can be intentional. Neither is
recommended as an unconditional default warning. The suggested fixes should
preserve side effects, including under Garden's eager boolean semantics.

## Evidence, scope and validation

- Six runnable exercises and one documentation inventory pass **12 Garden unit tests**, static checking, demos and formatting. Hangman also passes interactive invalid/duplicate-input, game-over and EOF smoke checks.
- **110 initial/migration probes**, **15 quickfix previews**, and documentation-check controls preserve original source and command output.
- **32 parser/runtime cases** plus **198 deterministic truncated inputs** isolate robustness failures; the corpus yields six panic observations and two hangs, grouped by failure shape.
- **58 type-checker cases**, **21 linter cases**, and **9 broad module/Unicode programs plus 7 transport inputs** provide targeted comparisons and controls.
- All **23 Rust reftest groups pass with a searchable-directory-only PATH**. The original environment's inaccessible/missing PATH entries made the missing-command snapshot return `Permission denied` instead of `No such file or directory`. No expected output was changed. See [test evidence](evidence/rust_reftests.txt).
- `cargo fmt --check` and `cargo clippy` pass; Rustfmt emits the existing stable-toolchain warning for `imports_granularity`.

Environment: Linux, Rust 1.95.0, Garden 0.26.0 debug build. Language source remains
at baseline `7beda2a86cec0e9ede3a480ced14748c7b4ddd50`; subsequent commits add only
review artifacts. Deliberately malformed/stress fixtures are retained as written.
The audit does not establish complete robustness, soundness, performance or
cross-platform compatibility. See [README](README.md) for reproduction commands.
