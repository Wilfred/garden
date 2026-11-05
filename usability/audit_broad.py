"""Cross-module, Unicode, and local stdin-protocol smoke experiments."""
from pathlib import Path
import json
import resource
import subprocess
import sys

sys.dont_write_bytecode = True
from probe import ROOT, GARDEN, capture

if __name__ == '__main__':
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    evidence = {}
    paths = sorted((ROOT / 'broad').glob('*.gdn')) + [ROOT / 'broad/modules/broken.gdn']
    for path in paths:
        relative = str(path.relative_to(ROOT.parent))
        evidence[relative] = {'source': path.read_text(), **{
            command: capture([str(GARDEN), command, relative], timeout=2) for command in ['check', 'run']}}
        print(relative, {c: evidence[relative][c]['exit'] for c in ['check', 'run']}, flush=True)
    inventory = evidence['usability/broad/reflect_inventory.gdn']['run']['stdout'].splitlines()
    keywords = json.loads(next(line.removeprefix('Keywords: ') for line in inventory if line.startswith('Keywords: ')))
    evidence['keyword_pages_missing'] = [name for name in keywords if not (ROOT.parent / 'website' / ('keyword:' + name + '.md')).exists()]
    payloads = {
        'lsp_oversized_length': ('lsp', b'Content-Length: 18446744073709551615\r\n\r\n'),
        'lsp_invalid_length': ('lsp', b'Content-Length: nope\r\n\r\n'),
        'lsp_eof': ('lsp', b''),
        'json_invalid_length': ('json', b'Content-Length: nope\n'),
        'json_truncated_payload': ('json', b'Content-Length: 10\n{}'),
        'json_invalid_utf8': ('json', b'Content-Length: 1\n\xff'),
        'json_eof': ('json', b''),
    }
    for name, (command, payload) in payloads.items():
        try:
            p = subprocess.run([str(GARDEN), command], cwd=ROOT.parent, input=payload,
                               capture_output=True, timeout=2)
            result = {'exit': p.returncode, 'stdout': p.stdout.decode(errors='replace'), 'stderr': p.stderr.decode(errors='replace')}
        except subprocess.TimeoutExpired as e:
            result = {'exit': 'timeout', 'stdout': (e.stdout or b'').decode(errors='replace'), 'stderr': (e.stderr or b'').decode(errors='replace')}
        result['stdin_hex'] = payload.hex()
        result['command'] = command
        evidence[name] = result
        print(name, result['exit'], flush=True)
    (ROOT / 'evidence/broad.json').write_text(json.dumps(evidence, indent=2) + '\n')
