# Garden language usability exercises and review

Read the [prioritized review](review.md) for **79 improvement candidates**, the
six project experiences, positive controls and remaining blind spots. Findings
are ordered by production impact; their stable IDs link back to earlier commits.
Confirmed defects, documented design choices and API/linter proposals are
explicitly distinguished. No language implementation changes are included.

## Run the working exercises

From the repository root:

```sh
cargo build
python3 usability/verify.py
```

The verifier checks, tests, runs and checks formatting of six exercises plus the
documentation inventory. All **12 unit tests** must pass. It also smoke-tests
hangman's invalid and duplicate guesses, game-over handling and EOF. The word
choice is random; assertions cover behavior shared by all configured words.
Results are written to [evidence/projects.json](evidence/projects.json).

| Program | Scope |
| --- | --- |
| [JSON](json_parser.gdn) | Recursive JSON subset; exact numeric lexemes; explicitly unsupported `\u`, `\b`, `\f` escapes and incomplete control-character rejection |
| [Hangman](hangman.gdn) | ASCII English words; deterministic demo and interactive console mode |
| [HTTP fields](http_headers.gdn) | Duplicate-preserving HTTP/1 field lines; no message framing or complete byte/control validation |
| [Brainfuck](brainfuck.gdn) | Bounded tape, wrapping byte cells, byte-list input/output, instruction limit |
| [Mandelbrot](fractal.gdn) | ASCII escape-time renderer; positive iteration limit and dimensions greater than one |
| [CSV](csv.gdn) | Quoted commas/newlines, doubled quotes, LF/CRLF records and malformed-input errors |
| [Documentation inventory](doc_inventory.gdn) | Reflected doc-comment and fenced-example counts; Tuple has separate manual content |

Run any program directly, or play hangman:

```sh
./target/debug/garden check usability/json_parser.gdn
./target/debug/garden test usability/json_parser.gdn
./target/debug/garden run usability/fractal.gdn
./target/debug/garden run usability/hangman.gdn -- --play
```

Garden uses top-level expressions; none of these programs requires a special
`main` entry point. `-- --play` passes the flag to the program rather than Garden.

## Replay the observational audits

Build first, then run the audits independently:

```sh
python3 usability/probe.py
python3 usability/audit_fixes.py
python3 usability/audit_docs.py
python3 usability/audit_robustness.py
python3 usability/audit_type_checker.py
python3 usability/audit_linters.py
python3 usability/audit_broad.py
```

These scripts capture failures as evidence. **Their successful exit does not
mean Garden passed every case.** Intentionally invalid programs, host panics,
aborts and timeouts are expected observations in this snapshot. Inspect the
per-command exits and diagnostics in the JSON records. `verify.py` is the
separate passing exercise suite.

| Audit | Source/input | Captured output |
| --- | --- | --- |
| Language and migration attempts | [probes](probes/), including 110 programs | [probes.json](evidence/probes.json) |
| Quickfix safety | 15 selected probes; temporary copies of preview output | [fixes.json](evidence/fixes.json) |
| Documentation checking | Website pages and three source fixtures stored in the audit | [doc_examples.json](evidence/doc_examples.json) |
| Parser/interpreter robustness | [32 saved cases](robustness/) plus 198 deterministic truncations | [robustness.json](evidence/robustness.json) |
| Type checking | [58 cases](type_checker/) | [type_checker.json](evidence/type_checker.json) |
| Linters | [21 cases](linters/) with existing-warning controls | [linters.json](evidence/linters.json) |
| Modules, Unicode and transports | [module programs](broad/) and seven exact stdin byte sequences | [broad.json](evidence/broad.json) |

The scripts use Python's standard library and run Garden in separate local
processes. They overwrite only their evidence files and use temporary files for
quickfix/doc/corpus experiments. Quickfix input probes are not edited. The
robustness/type/linter/broad runners disable core dumps and use bounded process
timeouts; robustness AST stdout is discarded to avoid huge tree output. The
transport audit uses stdin, not network listeners. Probe 43 receives CRLF input;
other probes receive EOF. [The two-byte invalid-UTF-8 fixture](fixtures/invalid_utf8.bin)
is used by the file-reading probe.

Malformed migration inputs and parser stress fixtures intentionally retain their
original syntax. They are not expected to pass formatting/checking, and should
not be treated as normal example programs.

## Rust checks and environment

```sh
cargo fmt --check
cargo clippy
cargo test reftest
```

All 23 reftest groups passed with `PATH` restricted to existing searchable
directories. Under the original environment, `shell_invalid_command.gdn` returned
`Permission denied` where its golden output expected `No such file or directory`.
No golden output was updated. [The successful run](evidence/rust_reftests.txt)
records this environment adjustment. Rustfmt emits an existing stable-toolchain
warning for `imports_granularity`.

The review ran on Linux with Rust 1.95.0 and Garden 0.26.0 in debug mode.
Release-build and cross-platform equivalence remain explicitly unverified.
Existing unrelated workspace files and `CHANGELOG.md` were left alone.
