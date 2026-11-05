"""Observe quickfix output and replay it without modifying the original probes."""
from pathlib import Path
import json
import sys
import tempfile

sys.dont_write_bytecode = True
from probe import ROOT, GARDEN, capture

CASES = [4, 17, 48, 59, 60, 66, 67, 79, 84, 85, 86, 87, 88, 89, 90]

if __name__ == '__main__':
    evidence = {}
    with tempfile.TemporaryDirectory(prefix='garden-usability-fixes-') as directory:
        for number in CASES:
            path = next(p for p in (ROOT / 'probes').glob('*.gdn') if int(p.stem.split('_')[0]) == number)
            relative = str(path.relative_to(ROOT.parent))
            fixed = capture([str(GARDEN), 'check', '--fix', '--stdout', relative])
            record = {'original_source': path.read_text(), 'fix': fixed}
            if fixed['exit'] == 0:
                output = Path(directory) / path.name
                output.write_text(fixed['stdout'])
                record['fixed_check'] = capture([str(GARDEN), 'check', str(output)])
                record['fixed_run'] = capture([str(GARDEN), 'run', str(output)])
                record['original_run'] = capture([str(GARDEN), 'run', relative])
            evidence[path.stem] = record
            print(path.stem, 'fix:', fixed['exit'], 'fixed check:', record.get('fixed_check', {}).get('exit'),
                  'fixed run:', record.get('fixed_run', {}).get('exit'))
        serialized = json.dumps(evidence, indent=2).replace(directory, '<fixed-source-directory>')
    (ROOT / 'evidence/fixes.json').write_text(serialized + '\n')
