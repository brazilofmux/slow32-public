#!/usr/bin/env python3
"""Random typed arithmetic, interpreter against compiled program.

    sbc/tests/fuzz.py [-n PROGRAMS] [-s SEED] [-j N]

Each program assigns edge values (0, +-1, 32767, -32768, 2147483647,
.5, 2.5, 1E+38, ...) to INTEGER, LONG, SINGLE and DOUBLE variables and
then computes random expressions over them -- + - * / \\ MOD ^, the
comparisons, NOT AND OR XOR -- into variables of every type, printing
each result.  ON ERROR prints ERR and resumes, so overflows, division by
zero and illegal calls are compared too.  The compiled program's native
paths (s32-sbc, milestone 2) must print what the interpreter prints.
Seeds are deterministic; a failing one is kept in out/fuzz/.
"""
import concurrent.futures as cf
import os
import random
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SBC = os.path.dirname(HERE)
ROOT = os.path.dirname(SBC)
EMU = os.path.join(ROOT, 'tools/emulator/slow32-fast')
SBASIC = os.path.join(ROOT, 'sbasic/sbasic.s32x')
OUT = os.path.join(SBC, 'out/fuzz')

VARS = ['A%', 'B%', 'C&', 'D&', 'E!', 'F!', 'G#', 'H#', 'X', 'Y']
EDGE = ['0', '1', '-1', '2', '3', '7', '-7', '255', '32767', '-32768', '32766',
        '65535', '2147483647', '-2147483647', '.5', '2.5', '-2.5', '1.5',
        '3.14159', '1E+38', '1E-38', '123456.7', '1D+300', '.1']
BINOPS = ['+', '-', '*', '/', '\\', 'MOD', '^', 'AND', 'OR', 'XOR',
          '=', '<>', '<', '>', '<=', '>=']


def expr(r, depth):
    if depth == 0 or r.random() < 0.3:
        return r.choice(VARS) if r.random() < 0.7 else r.choice(EDGE)
    k = r.random()
    if k < 0.1:
        return '-' + expr(r, depth - 1)
    if k < 0.15:
        return '(NOT ' + expr(r, depth - 1) + ')'
    return '(' + expr(r, depth - 1) + ' ' + r.choice(BINOPS) + ' ' + expr(r, depth - 1) + ')'


def program(seed):
    r = random.Random(seed)
    lines = ['ON ERROR GOTO 900']
    for v in VARS:
        lines.append('%s = %s' % (v, r.choice(EDGE)))
    for i in range(40):
        target = r.choice(VARS)
        lines.append('%s = %s' % (target, expr(r, 3)))
        lines.append('PRINT "%d"; %s' % (i, target))
        if r.random() < 0.3:
            lines.append('IF %s THEN PRINT "t%d" ELSE PRINT "f%d"' % (expr(r, 2), i, i))
        if r.random() < 0.25:
            # a loop: every type of variable, edge bounds and steps, at
            # most 25 turns (EXIT FOR), nested now and then
            v = r.choice(VARS)
            step = r.choice(['', ' STEP 1', ' STEP -1', ' STEP .5', ' STEP 2.5', ' STEP -.1',
                             ' STEP 0', ' STEP 1E+38', ' STEP 7', ' STEP ' + r.choice(VARS)])
            lines.append('K& = 0')
            lines.append('FOR %s = %s TO %s%s' % (v, r.choice(EDGE + VARS), r.choice(EDGE + VARS), step))
            lines.append('K& = K& + 1: PRINT %s;' % v)
            if r.random() < 0.3:
                w = r.choice([x for x in VARS if x != v])
                lines.append('FOR %s = 1 TO 3: PRINT %s;: NEXT %s' % (w, w, w))
            lines.append('IF K& > 25 THEN EXIT FOR')
            lines.append('NEXT %s' % v)
            lines.append('PRINT "L"; K&; %s' % v)
    lines.append('END')
    lines.append('900 PRINT "E"; ERR: RESUME NEXT')
    return '\n'.join(lines) + '\n'


def one(seed):
    d = os.path.join(OUT, str(seed))
    os.makedirs(d, exist_ok=True)
    src = os.path.join(d, 'f.bas')
    open(src, 'w').write(program(seed))
    exe = os.path.join(d, 'f.s32x')
    c = subprocess.run(['bash', os.path.join(SBC, 'compile-bas.sh'), src, exe],
                       capture_output=True, env=dict(os.environ, SBC_WORK=d))
    if c.returncode != 0:
        return seed, 'NOCOMPILE ' + c.stderr.decode()[-200:]
    i = subprocess.run([EMU, '-q', SBASIC, src], capture_output=True, timeout=60).stdout
    i = b'\n'.join(l for l in i.split(b'\n') if not l.startswith(b'Loaded '))
    o = subprocess.run([EMU, '-q', exe], capture_output=True, timeout=60).stdout
    if i == o:
        subprocess.run(['rm', '-rf', d])
        return seed, None
    open(os.path.join(d, 'interp.txt'), 'wb').write(i)
    open(os.path.join(d, 'comp.txt'), 'wb').write(o)
    return seed, 'DIFFER (kept in %s)' % d


def main():
    a = sys.argv[1:]
    n = int(a[a.index('-n') + 1]) if '-n' in a else 50
    s0 = int(a[a.index('-s') + 1]) if '-s' in a else 1
    j = int(a[a.index('-j') + 1]) if '-j' in a else 8
    subprocess.run(['bash', os.path.join(SBC, 'build.sh')], check=True, capture_output=True)
    bad = 0
    with cf.ThreadPoolExecutor(max_workers=j) as ex:
        for seed, verdict in ex.map(one, range(s0, s0 + n)):
            if verdict:
                bad += 1
                print(seed, verdict)
    print('%d of %d agree' % (n - bad, n))
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
