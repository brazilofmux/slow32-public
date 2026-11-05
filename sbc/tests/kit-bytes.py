#!/usr/bin/env python3
"""kit-bytes.py KIT WORKDIR [DIR...]: the kit's s32-bc.s32x and s32-sbc.s32x
against the host builds, over the corpus: the same output file byte for
byte, or the same refusal.  run-kit.sh's first leg."""
import concurrent.futures as cf, os, subprocess, sys
HERE = os.path.dirname(os.path.abspath(__file__))
SBC = os.path.dirname(HERE)
ROOT = os.path.dirname(SBC)
EMU = os.environ.get('EMU', os.path.join(ROOT, 'tools/dbt/slow32-dbt'))
REFS = os.path.expanduser('~/refs/basic/nbs-ecma55-test')

def corpus(dirs):
    out = []
    for d in dirs:
        for r, _, fs in os.walk(d):
            out += [os.path.join(r, f) for f in fs if f.lower().endswith('.bas')]
    return sorted(out)

def run(argv, cwd):
    try:
        return subprocess.run(argv, capture_output=True, cwd=cwd, timeout=120)
    except subprocess.TimeoutExpired:
        return None

def one(job):
    tool, kit, work, n, path = job
    cwd = os.path.dirname(path)
    h, g = os.path.join(work, '%d.%s.h' % (n, tool)), os.path.join(work, '%d.%s.g' % (n, tool))
    host = os.path.join(SBC, 's32-' + tool)
    guest = [EMU, os.path.join(kit, 's32-%s.s32x' % tool)]
    sep = [] if tool == 'bc' else ['-o']
    a = run([host, path] + sep + [h], cwd)
    b = run(guest + [path] + sep + [g], cwd)
    verdict = 'SAME'
    if a is None or b is None:
        verdict = 'TIMEOUT'
    elif (a.returncode != 0) != (b.returncode != 0):
        verdict = 'STATUS host %d guest %d' % (a.returncode, b.returncode)
    elif a.returncode != 0:
        verdict = 'REFUSED' if a.stderr.strip() == b.stderr.strip() else 'WORDS %r %r' % (a.stderr[:90], b.stderr[:90])
    elif open(h, 'rb').read() != open(g, 'rb').read():
        verdict = 'BYTES'
    for f in (h, g):
        if os.path.exists(f):
            os.unlink(f)
    return tool, path, verdict

def main():
    kit, work = sys.argv[1], sys.argv[2]
    os.makedirs(work, exist_ok=True)
    dirs = sys.argv[3:] or [os.path.join(ROOT, 'sbasic/tests'), os.path.join(HERE, 'native'),
                            os.path.join(HERE, 'kernels')] + [os.path.join(REFS, d) for d in ('NBS', 'basic_computer_games', 'fast')]
    files = corpus(dirs)
    jobs = [(t, kit, work, i, p) for t in ('bc', 'sbc') for i, p in enumerate(files)]
    with cf.ThreadPoolExecutor(max_workers=8) as ex:
        res = list(ex.map(one, jobs))
    bad = 0
    for tool in ('bc', 'sbc'):
        same = sum(1 for t, _, v in res if t == tool and v == 'SAME')
        ref = sum(1 for t, _, v in res if t == tool and v == 'REFUSED')
        print('s32-%s: %d the same bytes, %d the same refusal (of %d)' % (tool, same, ref, len(files)))
    for t, p, v in res:
        if v not in ('SAME', 'REFUSED'):
            bad += 1
            print('s32-%s %s: %s' % (t, os.path.relpath(p, ROOT) if p.startswith(ROOT) else p, v))
    sys.exit(1 if bad else 0)

main()
