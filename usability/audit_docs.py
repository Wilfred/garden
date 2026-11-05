"""Replay documentation examples and controls for skipped/untested code."""
from pathlib import Path
import json
import sys
import tempfile

sys.dont_write_bytecode = True
from probe import ROOT, GARDEN, capture

if __name__ == '__main__':
    evidence = {}
    for relative in ['website/keyword:fun.md', 'website/keyword:let.md', 'website/operator:+.md']:
        evidence[relative] = capture([str(GARDEN), 'run-code-blocks', relative])
    examples = {
        'title_only': '```title:Example\nassert(False)\n```\n',
        'garden_label': '```garden title:Example\nassert(False)\n```\n',
        'failing_test': '```garden\ntest incorrect { assert(False) }\n```\n',
    }
    with tempfile.TemporaryDirectory(prefix='garden-doc-review-') as directory:
        for name, source in examples.items():
            path = Path(directory) / (name + '.txt')
            path.write_text(source)
            evidence[name] = {'source': source, **capture([str(GARDEN), 'run-code-blocks', str(path)])}
        serialized = json.dumps(evidence, indent=2).replace(directory, '<doc-fixture-directory>')
    (ROOT / 'evidence/doc_examples.json').write_text(serialized + '\n')
    for name, record in evidence.items():
        print(name, record['exit'], record['stderr'].strip())
