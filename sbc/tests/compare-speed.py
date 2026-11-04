#!/usr/bin/env python3
"""compare-speed.py -- the two compilers on the same programs: each of
the games (~/refs/basic/nbs-ecma55-test/basic_computer_games, not in
this tree; or the .bas files of the directories given) compiled by
s32-sbc and by s32-bc, run with its keys under slow32-fast, and the
instructions each takes counted beside the interpreter's -- over the
programs both compilers take, whose outputs must agree (False marks
one that does not).  sbc/README.md's speed row is its last line.

    compare-speed.py [DIR ...]
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
dirs = sys.argv[1:] or [os.path.expanduser('~/refs/basic/nbs-ecma55-test/basic_computer_games')]
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
