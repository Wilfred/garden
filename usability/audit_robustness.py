"""Bounded, separate-process parser/runtime robustness experiments."""
from pathlib import Path
import json
import resource
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent
GARDEN = ROOT.parent / 'target/debug/garden'


def no_core_dump():
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))


def capture(args):
    # AST output can be very large for nested trees; retain only diagnostics.
    try:
        p = subprocess.run([str(GARDEN), *args], cwd=ROOT.parent, input='',
                           stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True,
                           timeout=2, preexec_fn=no_core_dump)
        return {'exit': p.returncode, 'stderr': p.stderr}
    except subprocess.TimeoutExpired as e:
        err = e.stderr.decode() if isinstance(e.stderr, bytes) else (e.stderr or '')
        return {'exit': 'timeout', 'stderr': err}


def crashed(result):
    return isinstance(result['exit'], int) and (result['exit'] < 0 or result['exit'] == 101)


if __name__ == '__main__':
    evidence = {}
    for path in sorted((ROOT / 'robustness').glob('*.gdn')):
        name = str(path.relative_to(ROOT.parent))
        results = {command: capture([command, name]) for command in ['reftest-ast', 'run', 'check']}
        evidence[path.stem] = {'source': path.read_text(), **results}
        print(path.stem, ', '.join(f"{cmd}={r['exit']}" for cmd, r in results.items()), flush=True)
    # Deterministically truncate plausible editor input at every character.
    seeds = ['fun f(x: Int): Int { x + 1 }', 'struct P { x: Int, y: String }',
             'enum E<T> { A, B(T) }', 'match Some((1, 2)) { Some((a, b)) => a, None => 0 }',
             'import "__fs.gdn" as fs', 'try { 1 } catch(e) { 2 }', '"hello\\nworld"']
    failures = []
    tested = 0
    with tempfile.TemporaryDirectory(prefix='garden-parser-corpus-') as directory:
        path = Path(directory) / 'input.gdn'
        for seed in seeds:
            for end in range(len(seed) + 1):
                src = seed[:end]
                path.write_text(src)
                result = capture(['reftest-ast', str(path)])
                tested += 1
                if crashed(result) or result['exit'] == 'timeout':
                    result['stderr'] = result['stderr'].replace(directory, '<corpus-directory>')
                    failures.append({'source': src, **result})
    evidence['truncated_input_corpus'] = {'count': tested, 'failures': failures}
    (ROOT / 'evidence/robustness.json').write_text(json.dumps(evidence, indent=2) + '\n')
    print('Truncated input cases:', tested, 'crashes/hangs:', len(failures))
