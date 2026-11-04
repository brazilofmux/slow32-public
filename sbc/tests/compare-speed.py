#!/usr/bin/env python3
"""compare-speed.py -- the two compilers on the same programs: each of
the games (~/refs/basic/nbs-ecma55-test/basic_computer_games, not in
this tree; or the .bas files of the directories given) compiled by
s32-sbc and by s32-bc, run with its keys under slow32-fast, and the
instructions each takes counted beside the interpreter's -- over the
programs both compilers take, whose outputs must agree (False marks
one that does not).  sbc/README.md's speed row is its last line.

    compare-speed.py [DIR ...]
    compare-speed.py --check            # the games, against speed-baseline.txt
    compare-speed.py --record           # the games, written to speed-baseline.txt

--check fails when a total grows by more than 5% or when outputs differ.
Every other gate compares output, so none of them can see a slowdown: a
text-screen model that copied the screen on each scroll made every
program 4x slower for days with all of them green (2026-11-04).
"""
import os, re, subprocess, sys, glob, tempfile, shutil
R = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
EMU = R + '/tools/emulator/slow32-fast'
W = tempfile.mkdtemp(prefix='speed-')
def keys(p):
    for ext in ('.in', '.input'):
        q = os.path.splitext(p)[0] + ext
        if os.path.exists(q): return open(q, 'rb').read()
    return b''
def run(args, inp):
    try: r = subprocess.run(args, input=inp, capture_output=True, timeout=120)
    except subprocess.TimeoutExpired: return None, None
    m = re.search(rb'Instructions executed: (\d+)', r.stdout + r.stderr)
    out = r.stdout.split(b'MMIO enabled for this program\n', 1)[-1].split(b'\nHALT at', 1)[0]
    return (int(m.group(1)) if m else None), out
args = sys.argv[1:]
check = '--check' in args
record = '--record' in args
args = [a for a in args if a not in ('--check', '--record')]
BASE = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'speed-baseline.txt')
dirs = args or [os.path.expanduser('~/refs/basic/nbs-ecma55-test/basic_computer_games')]
progs = sorted(p for d in dirs for p in glob.glob(os.path.join(d, '*.bas')))
tot = [0, 0, 0]; n = 0; rows = []
for p in progs:
    b = os.path.basename(p)
    a = os.path.join(W, b + '.sbc.s32x'); c = os.path.join(W, b + '.bc.s32x')
    if subprocess.run(['bash', R + '/sbc/compile-bas.sh', p, a], capture_output=True).returncode: continue
    if subprocess.run(['bash', R + '/sbc/compile-bc.sh', p, c], capture_output=True).returncode: continue
    k = keys(p)
    i0, o0 = run([EMU, R + '/sbasic/sbasic.s32x', p], k)
    i1, o1 = run([EMU, a], k); i2, o2 = run([EMU, c], k)
    if None in (i0, i1, i2): continue
    same = o1 == o2
    rows.append((b, i0, i1, i2, same)); n += 1
    tot[0] += i0; tot[1] += i1; tot[2] += i2
for r in rows: print('%-16s %12d %12d %12d %s' % r)
print('programs %d  interpreter %d  s32-sbc %d  s32-bc %d' % (n, *tot))
shutil.rmtree(W, ignore_errors=True)
names = ('interpreter', 's32-sbc', 's32-bc')
if record:
    with open(BASE, 'w') as f:
        f.write('# compare-speed.py --record: instructions over the games both compilers take\n')
        f.write('programs %d\n' % n)
        for k, v in zip(names, tot): f.write('%s %d\n' % (k, v))
if check:
    want = dict(l.split() for l in open(BASE) if not l.startswith('#'))
    bad = [r[0] for r in rows if not r[4]]
    if bad: print('FAIL: the compilers differ on', ' '.join(bad))
    if int(want['programs']) != n:
        print('NOTE: %d programs, the baseline had %s' % (n, want['programs']))
    for k, v in zip(names, tot):
        w = int(want[k])
        print('%-12s %12d  baseline %12d  %+.1f%%' % (k, v, w, 100.0 * (v - w) / w))
        if v > w * 1.05: bad.append(k)
    sys.exit(1 if bad else 0)
