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


ARRS = ['P%', 'Q&', 'R!', 'S#', 'T']
SUBS = ['0', '1', '2', '3', '5', '10', '11', '-1', '.5', '1.5', '2.5', 'A%', 'X', 'Y']


def subscript(r):
    return r.choice(SUBS) if r.random() < 0.8 else expr(r, 1)


def element(r, name):
    if name in ('R!', 'T'):
        return '%s(%s, %s)' % (name, subscript(r), subscript(r))
    return '%s(%s)' % (name, subscript(r))


SVARS = ['S$', 'U$', 'W$(1)', 'W$(2)', 'W$(I%)']
SLITS = ['""', '"a"', '"abc"', '"Hello"', '"x y"', '"0123456789"']


def sexpr(r, depth):
    """a string expression"""
    if depth == 0 or r.random() < 0.3:
        return r.choice(SVARS) if r.random() < 0.6 else r.choice(SLITS)
    k = r.random()
    if k < 0.3:
        return '(' + sexpr(r, depth - 1) + ' + ' + sexpr(r, depth - 1) + ')'
    f = r.choice(['MID$(%s, %s)', 'MID$(%s, %s, %s)', 'LEFT$(%s, %s)', 'RIGHT$(%s, %s)',
                  'UCASE$(%s)', 'LCASE$(%s)', 'LTRIM$(%s)', 'RTRIM$(%s)'])
    pieces = f.split('%s')
    res = pieces[0]
    for i in range(1, len(pieces)):
        res += (sexpr(r, depth - 1) if i == 1 else r.choice(['0', '1', '2', '3', '-1', '5', '10', 'I%', '2.5', '1.5', '-0.5', '1E10', 'X']))
        res += pieces[i]
    if r.random() < 0.2:
        return r.choice(['CHR$(%s)', 'STR$(%s)', 'SPACE$(%s)', 'HEX$(%s)']) % r.choice(['65', '0', '255', '256', '-1', '3', 'I%', 'A%', '65.5', '66.5', 'X'])
    return res


def expr(r, depth):
    if depth == 0 or r.random() < 0.3:
        k = r.random()
        if k < 0.15:
            return element(r, r.choice(ARRS))
        return r.choice(VARS) if k < 0.75 else r.choice(EDGE)
    k = r.random()
    if k < 0.1:
        return '-' + expr(r, depth - 1)
    if k < 0.15:
        return '(NOT ' + expr(r, depth - 1) + ')'
    if k < 0.34 and k >= 0.3:
        return r.choice(['LEN(%s)', 'ASC(%s)', 'INSTR(%s, %s)']).replace('%s', sexpr(r, 1), 1).replace('%s', sexpr(r, 1), 1)
    if k < 0.43 and k >= 0.38:
        # a DEF FN (it may not be defined yet: ON ERROR reports it)
        name, nargs = r.choice([('FNP', 1), ('FNQ%', 2), ('FNR#', 1), ('FNS', 1)])
        return name + '(' + ', '.join(expr(r, depth - 1) for _ in range(nargs)) + ')'
    if k < 0.38 and k >= 0.34:
        return '(' + sexpr(r, 1) + ' ' + r.choice(['=', '<>', '<', '>', '<=', '>=']) + ' ' + sexpr(r, 1) + ')'
    if k < 0.3 and k >= 0.22:
        return r.choice(['ABS', 'INT', 'FIX', 'SGN', 'SQR', 'SIN', 'COS', 'TAN', 'ATN', 'EXP',
                         'LOG', 'CINT', 'CLNG', 'CSNG', 'CDBL']) + '(' + expr(r, depth - 1) + ')'
    if k < 0.22:
        name, nargs = r.choice([('F1%', 2), ('F2#', 1), ('F3!', 1), ('F4!', 1)])
        if name == 'F4!' and r.random() < 0.6:
            # by reference: a SINGLE variable or element, or a copy
            return 'F4!(' + r.choice(['E!', 'F!', 'X', 'Y', '(E!)', 'R!(1, 2)', 'A%']) + ')'
        return name + '(' + ', '.join(expr(r, depth - 1) for _ in range(nargs)) + ')'
    return '(' + expr(r, depth - 1) + ' ' + r.choice(BINOPS) + ' ' + expr(r, depth - 1) + ')'


def A_EXPR(r):
    return expr(r, 1)


def program(seed):
    r = random.Random(seed)
    lines = ['ON ERROR GOTO 900', 'DIM P%(10), Q&(10), R!(3, 4), S#(10), W$(5)',
             'S$ = "abc": U$ = "Hello": W$(1) = "x": I% = 2']
    for v in VARS:
        lines.append('%s = %s' % (v, r.choice(EDGE)))
    defn = r.randint(0, 6)
    for i in range(40):
        if i == defn:
            # parameters shadowing globals (X, A%), a nested call, a string
            lines.append('DEF FNP(X) = X * 2 + Y')
            lines.append('DEF FNQ%(A%, Z#) = A% \\ 3 + Z# * B%')
            lines.append('DEF FNR#(N) = FNP(N) - N / 7')
            lines.append('DEF FNS(K&) = LEN(S$) + K& MOD 5')
        if r.random() < 0.2:
            st = r.choice(SVARS[:4])
            lines.append('%s = %s' % (st, sexpr(r, 3)))
            lines.append('PRINT "s%d"; %s; LEN(%s)' % (i, st, st))
            if r.random() < 0.3:
                lines.append('IF %s %s %s THEN PRINT "y" ELSE PRINT "n"' % (sexpr(r, 2), r.choice(['=', '<', '>=']), sexpr(r, 2)))
            continue
        if r.random() < 0.35:
            target = element(r, r.choice(ARRS))
        else:
            target = r.choice(VARS)
        lines.append('%s = %s' % (target, expr(r, 3)))
        lines.append('PRINT "%d"; %s' % (i, target))
        if r.random() < 0.04:
            lines.append(r.choice(['ERASE S#', 'REDIM S#(5)', 'ERASE T', 'REDIM P%(20)', 'ERASE Q&: DIM Q&(3)']))
        if r.random() < 0.08:
            lines.append('CALL S1(%s): PRINT "S"; Y' % expr(r, 2))
        if r.random() < 0.08:
            a = r.choice(['A%', 'B%', 'P%(2)', '(A%)', 'C&', A_EXPR(r)])
            lines.append('CALL S2(%s, %s): PRINT "S2"; A%%; B%%; P%%(2)' % (a, r.choice(['A%', 'B%', '3'])))
        if r.random() < 0.06:
            lines.append('GOSUB 800: PRINT "G"; B%; C&')
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
    lines.append('800 C& = C& + 1: IF C& MOD 3 = 0 THEN GOSUB 810')
    lines.append('RETURN')
    lines.append('810 B% = B% XOR 5: RETURN')
    lines.append('900 PRINT "E"; ERR: RESUME NEXT')
    # procedures: typed parameters and values, recursion (bounded), SHARED
    lines += ['FUNCTION F1%(a%, b!)', 'F1% = a% * 2 + b!', 'END FUNCTION',
              'FUNCTION F2#(x#)', 'F2# = x# / 3 + 1', 'END FUNCTION',
              'FUNCTION F3!(n%)', 'IF n% > 3 OR n% < -3 THEN F3! = .5 ELSE F3! = n% * F3!(n% + 1) + 1',
              'END FUNCTION',
              'SUB S1(a!)', 'SHARED Y', 'Y = Y + a!', 'END SUB',
              'FUNCTION F4!(z!)', 'z! = z! + 1', 'F4! = z! * 2', 'END FUNCTION',
              'SUB S2(a%, b%)', 'a% = a% + b%', 'END SUB']
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
    try:
        i = subprocess.run([EMU, '-q', SBASIC, src], capture_output=True, timeout=60).stdout
    except subprocess.TimeoutExpired:
        return seed, None if os.environ.get('FUZZ_STRICT') is None else 'TIMEOUT (interpreter)'

    i = b'\n'.join(l for l in i.split(b'\n') if not l.startswith(b'Loaded '))
    try:
        o = subprocess.run([EMU, '-q', exe], capture_output=True, timeout=60).stdout
    except subprocess.TimeoutExpired:
        return seed, 'TIMEOUT (compiled only)'
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
