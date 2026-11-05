"""Capture existing and missing diagnostics for plausible logic mistakes."""
from pathlib import Path
import json
import resource
import sys

sys.dont_write_bytecode = True
from probe import ROOT, GARDEN, capture

if __name__ == '__main__':
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    evidence = {}
    for path in sorted((ROOT / 'linters').glob('*.gdn')):
        relative = str(path.relative_to(ROOT.parent))
        results = {command: capture([str(GARDEN), command, relative], timeout=1) for command in ['check', 'run']}
        evidence[path.stem] = {'source': path.read_text(), **results}
        print(path.stem, ', '.join(f"{cmd}={r['exit']}" for cmd, r in results.items()), flush=True)
    (ROOT / 'evidence/linters.json').write_text(json.dumps(evidence, indent=2) + '\n')
