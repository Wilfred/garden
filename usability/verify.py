"""Check the usable exercises and record their demos and console behavior."""
from pathlib import Path
import json
import subprocess

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parent
GARDEN = REPO / 'target/debug/garden'
PROJECTS = ['http_headers', 'hangman', 'brainfuck', 'json_parser', 'fractal', 'csv']


def run(args, stdin=''):
    result = subprocess.run([str(GARDEN), *args], cwd=REPO, input=stdin,
                            capture_output=True, text=True, timeout=30)
    return dict(exit=result.returncode, stdout=result.stdout, stderr=result.stderr)


if __name__ == '__main__':
    evidence = {}
    failures = []
    for name in PROJECTS:
        path = str((ROOT / (name + '.gdn')).relative_to(REPO))
        results = {command: run([command, path]) for command in ['check', 'test', 'run']}
        results['format'] = run(['format', '--check', path])
        evidence[name] = results
        failures.extend(f'{name} {command}' for command, r in results.items() if r['exit'] != 0)
        print(name, ', '.join(f"{command}={r['exit']}" for command, r in results.items()))
    eof = run(['run', 'usability/hangman.gdn', '--', '--play'])
    evidence['hangman_eof'] = eof
    if eof['exit'] != 0 or 'End of input.' not in eof['stdout']:
        failures.append('hangman EOF')
    played = run(['run', 'usability/hangman.gdn', '--', '--play'], 'ab\nz\nz\n' + '\n'.join('abcdefghijklmnopqrstuvwxyz') + '\n')
    evidence['hangman_console'] = played
    if (played['exit'] != 0 or 'Enter one ASCII letter' not in played['stdout']
            or 'Already guessed: z' not in played['stdout']
            or not any(message in played['stdout'] for message in ['You won!', 'The word was'])):
        failures.append('hangman console')
    (ROOT / 'evidence/projects.json').write_text(json.dumps(evidence, indent=2) + '\n')
    if failures:
        raise SystemExit('Failed: ' + ', '.join(failures))
    print('All exercise checks, tests, demos, formatting and console smoke checks passed.')
