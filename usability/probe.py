"""Capture small Garden usability probes, including failures and hangs."""
from pathlib import Path
import json
import subprocess

ROOT = Path(__file__).resolve().parent
GARDEN = ROOT.parent / 'target/debug/garden'


def capture(command, timeout=3, stdin=None):
    try:
        p = subprocess.run(command, input=stdin, text=True, capture_output=True, timeout=timeout, cwd=ROOT.parent)
        return dict(exit=p.returncode, stdout=p.stdout, stderr=p.stderr)
    except subprocess.TimeoutExpired as e:
        def decode(s):
            return s.decode() if isinstance(s, bytes) else (s or '')
        return dict(exit='timeout', stdout=decode(e.stdout), stderr=decode(e.stderr))


if __name__ == '__main__':
    results = {}
    for path in sorted((ROOT / 'probes').glob('*.gdn')):
        relative = str(path.relative_to(ROOT.parent))
        results[path.stem] = {
            'source': path.read_text(),
            'check': capture([str(GARDEN), 'check', relative]),
            'run': capture([str(GARDEN), 'run', relative]),
        }
    (ROOT / 'evidence/probes.json').write_text(json.dumps(results, indent=2) + '\n')
    for name, r in results.items():
        print(name, 'check:', r['check']['exit'], 'run:', r['run']['exit'])
