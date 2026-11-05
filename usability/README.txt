Garden usability exercises

These are experiments in language usability, not proposals to change Garden's
implementation. Run from the repository root with a fresh `cargo build`.
All exercise files use top-level demo expressions; no special main function.

  garden check usability/http_headers.gdn
  garden test usability/http_headers.gdn
  garden run usability/http_headers.gdn
  python3 usability/probe.py

Here and below, `garden` means ./target/debug/garden. The probe runner captures
check and run output in evidence/probes.json, with a three-second timeout for
nontermination. Probes deliberately contain invalid code; do not expect them
all to check. They are separate from the exercise programs.

See review.txt for project experiences and numbered findings. The evidence
contains diagnostics verbatim so reports can be re-evaluated after changes.
Existing untracked workspace files were left alone. No language implementation
changes are included in this review.

Exercises and verification

  python3 usability/verify.py

This checks, tests, runs and checks formatting of all six exercises; it also
smoke-tests hangman's interactive input validation, duplicate guesses, game
over and EOF. Evidence is saved in evidence/projects.json. Run interactive
hangman yourself with:

  ./target/debug/garden run usability/hangman.gdn -- --play

Other demos are json_parser.gdn, brainfuck.gdn, fractal.gdn and csv.gdn.
The JSON parser is intentionally incomplete: see review.txt for exact limits.
The Brainfuck API takes/returns bytes as Int lists. Fractal output is ASCII.
Both Python runners use only the standard library and write evidence locally.
The exercise verifier must pass; the deliberately failing probes are an
observational record, not a passing regression test suite. Probe 43 supplies
CRLF input; other probes receive EOF on stdin. Format valid probes with Garden;
invalid syntax probes retain their original attempts.

Follow-up audits

followup_review.txt covers warnings, quickfixes, migration mistakes and docs.
The suite also includes doc_inventory.gdn, which measures reflected API doc
and example coverage. Tuple's separate website page is outside reflection.

  python3 usability/audit_fixes.py

This previews quickfixes on selected probes, checks and runs the output, and
records evidence/fixes.json. It deliberately observes failures as well as
successful fixes and does not edit the input probes. The probe fixtures include
two invalid UTF-8 bytes for the read-file error case.
