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
