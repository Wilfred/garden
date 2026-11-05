"""Compare static checking with execution for focused type-system cases."""
from pathlib import Path
import json
import resource
import sys

sys.dont_write_bytecode = True
from probe import ROOT, GARDEN, capture

if __name__ == '__main__':
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    evidence = {}
    for path in sorted((ROOT / 'type_checker').glob('*.gdn')):
        relative = str(path.relative_to(ROOT.parent))
        results = {command: capture([str(GARDEN), command, relative]) for command in ['check', 'run']}
        evidence[path.stem] = {'source': path.read_text(), **results}
        print(path.stem, ', '.join(f"{cmd}={r['exit']}" for cmd, r in results.items()), flush=True)
    (ROOT / 'evidence/type_checker.json').write_text(json.dumps(evidence, indent=2) + '\n')
