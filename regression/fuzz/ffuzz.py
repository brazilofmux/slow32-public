#!/usr/bin/env python3
"""ffuzz.py -- random-program differential for the two Fortran compilers.

Each seed makes one deterministic fixed-form Fortran 77 program that the
standard gives one answer: INTEGER values kept under 2**15 (every
assignment reduces MOD 32749, and an expression is at most a sum of two
products of such values, so nothing reaches 2**31 -- flang hands LLVM
overflow as undefined, and it uses it), DOUBLE PRECISION reduced the same
way through a temporary with plain IEEE operations, arrays indexed in
bounds, DO loops with constant bounds and their variables never
redefined, block, logical and arithmetic IF, subroutines that change
their arguments and COMMON, pure INTEGER functions, no argument aliased,
every variable set before it is read, and output only through explicit
FORMATs (list-directed output's layout is the compiler's to choose).

fortran/'s f77 and flang (flang/s32flang) each build it, slow32-dbt -I
runs both, and stdout and exit status must match.  Where they differ,
gfortran on the host (-O0 -ffp-contract=off) says which is right.

    ffuzz.py run    [-n N] [-s FIRST] [-j JOBS] [--keep DIR]
    ffuzz.py show   SEED
    ffuzz.py reduce SEED [-o OUT.f]

`reduce` deletes statements and subprograms while the two still differ
the same way; the result is a test for fortran/tests/f77 or for flang.
"""
import argparse
import os
import random
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
F77 = os.path.join(ROOT, 'fortran/compile.sh')
FLANG = os.path.join(ROOT, 'flang/s32flang')
DBT = os.path.join(ROOT, 'tools/dbt/slow32-dbt')
GFORTRAN = os.environ.get('GFORTRAN', shutil.which('gfortran') or 'gfortran')
NI = 6   # integer scalars per unit
ND = 3   # double scalars per unit
LIM = 32749

# ------------------------------------------------------------------ program
# A unit: kind 'main' | 'sub' | 'fn'; params [(name, 'i'|'d'|'a')] ('a' an
# INTEGER(10) array); body a list of statements:
#   ('ai', lhs, expr)  ('ad', lhs, expr)  ('do', var, lo, hi, body)
#   ('if', cond, then, else)  ('lif', cond, stmt)  ('aif', expr, neg, zero, pos)
#   ('call', name, args)  ('print',)
# Expressions are strings: generation is where the care is.


class Unit:
    def __init__(self, name, kind, params):
        self.name, self.kind, self.params = name, kind, params
        self.ints = [f'I{k}' for k in range(1, NI + 1)]
        self.dbls = [f'D{k}' for k in range(1, ND + 1)]
        self.body = []


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)
        self.units = []

    def pick(self, xs):
        return self.r.choice(xs)

    # -- what a unit may read and write
    def ivars(self, u, loops, write=False):
        vs = list(u.ints) + [p for p, t in u.params if t == 'i'] + ['KC1', 'KC2']
        if u.kind == 'fn' and write:
            vs = list(u.ints)                       # pure: no arguments, no COMMON
        return [v for v in vs if not (write and v in loops)]

    def dvars(self, u, write=False):
        vs = list(u.dbls) + [p for p, t in u.params if t == 'd'] + ['DC1']
        if u.kind == 'fn':
            vs = list(u.dbls)
        return vs

    def arrays(self, u, write=False):
        a = [p for p, t in u.params if t == 'a']
        if u.kind != 'fn' or not write:
            a = a + ['KA']                          # COMMON array
        if u.kind == 'fn' and write:
            a = []
        return a + ['LA']                           # local array

    def index(self, u, loops, n=10):
        if loops and self.r.random() < 0.6:
            v = self.pick(list(loops))
            if loops[v] <= n:
                return v
        if self.r.random() < 0.5:
            return str(self.r.randint(1, n))
        return f'1+MOD(ABS({self.iatom(u, loops, 1)}),{n})'

    # -- integer: |atom| < 2**15
    def iatom(self, u, loops, depth=2):
        r = self.r.random()
        if depth <= 0 or r < 0.30:
            return self.pick(self.ivars(u, loops) + list(loops))
        if r < 0.42:
            return lit(self.r.randint(-999, 999))
        if r < 0.55:
            return f'{self.pick(self.arrays(u))}({self.index(u, loops)})'
        a = lambda: self.iatom(u, loops, depth - 1)
        c = lit(self.r.choice([x for x in range(-97, 98) if x != 0]))
        forms = [lambda: f'ABS({a()})', lambda: f'MAX({a()},{a()})', lambda: f'MIN({a()},{a()},{a()})',
                 lambda: f'MOD({a()},{c})', lambda: f'({a()}/{c})', lambda: f'ISIGN({a()},{a()})',
                 lambda: f'INT({self.datom(u, loops, 0)})', lambda: f'IDIM({a()}/2,{a()}/2)']
        fns = [x for x in self.units if x.kind == 'fn']
        if fns and self.r.random() < 0.25:
            f = self.pick(fns)
            return f'{f.name}({",".join(a() for _ in f.params)})'
        return self.pick(forms)()

    def iexpr(self, u, loops):
        def term():
            r = self.r.random()
            x = self.iatom(u, loops)
            if r < 0.35:
                return f'{x}*{self.iatom(u, loops)}'
            if r < 0.6:
                return f'({x}{self.pick("+-")}{self.iatom(u, loops)})'
            return x
        e = term()
        if self.r.random() < 0.5:
            e = f'{e}{self.pick("+-")}{term()}'
        return e

    # -- double: |atom| < about 4e4
    def datom(self, u, loops, depth=2):
        r = self.r.random()
        if depth <= 0 or r < 0.35:
            return self.pick(self.dvars(u))
        if r < 0.5:
            v = f'{self.r.randint(0, 999)}.{self.r.randint(0, 99):02d}D0'
            return f'(-{v})' if self.r.random() < 0.5 else v
        if r < 0.65:
            return f'DBLE({self.iatom(u, loops, 1)})'
        a = lambda: self.datom(u, loops, depth - 1)
        forms = [lambda: f'ABS({a()})', lambda: f'MAX({a()},{a()})', lambda: f'MIN({a()},{a()})',
                 lambda: f'SQRT(ABS({a()}))', lambda: f'AINT({a()})', lambda: f'ANINT({a()})',
                 lambda: f'SIGN({a()},{a()})', lambda: f'({a()}/{self.pick(["3.0D0", "(-7.0D0)", "0.125D0", "1.1D0"])})',
                 lambda: f'DBLE(REAL({a()}))']
        return self.pick(forms)()

    def dexpr(self, u, loops):
        e = self.datom(u, loops)
        for _ in range(self.r.randint(0, 2)):
            op = self.pick('+-*')
            e = f'{e}{op}{self.datom(u, loops)}' if op != '*' else f'({e})*{self.datom(u, loops, 0)}'
        return e

    def cond(self, u, loops, depth=1):
        r = self.r.random()
        rel = self.pick(['.LT.', '.LE.', '.EQ.', '.NE.', '.GT.', '.GE.'])
        if depth > 0 and r < 0.2:
            return f'({self.cond(u, loops, 0)}{self.pick([".AND.", ".OR.", ".EQV.", ".NEQV."])}{self.cond(u, loops, 0)})'
        if depth > 0 and r < 0.28:
            return f'(.NOT.{self.cond(u, loops, 0)})'
        if r < 0.65:
            return f'{self.iexpr(u, loops)}{rel}{self.iatom(u, loops)}'
        return f'{self.datom(u, loops)}{rel}{self.datom(u, loops)}'

    # -- statements
    def lhs_i(self, u, loops):
        if self.r.random() < 0.25:
            arrs = self.arrays(u, write=True)
            return f'{self.pick(arrs)}({self.index(u, loops)})'
        return self.pick(self.ivars(u, loops, write=True))

    def stmt(self, u, loops, depth):
        r = self.r.random()
        if r < 0.32:
            return ('ai', self.lhs_i(u, loops), self.iexpr(u, loops))
        if r < 0.50:
            return ('ad', self.pick(self.dvars(u, write=True)), self.dexpr(u, loops))
        if r < 0.60 and depth < 2:
            free = [v for v in ('J1', 'J2', 'J3') if v not in loops]
            v = free[0]
            lo = self.r.randint(1, 4)
            hi = self.r.randint(lo - 1, 10)
            step = 1 if self.r.random() < 0.8 else self.pick([2, 3, -1])
            if step < 0:
                lo, hi = hi if hi >= 1 else 1, lo
            nl = dict(loops)
            nl[v] = max(lo, hi)
            return ('do', v, lo, hi, step, self.block(u, nl, depth + 1, 3))
        if r < 0.72 and depth < 2:
            return ('if', self.cond(u, loops), self.block(u, loops, depth + 1, 3),
                    self.block(u, loops, depth + 1, 2) if self.r.random() < 0.5 else [])
        if r < 0.80:
            s = self.stmt(u, loops, 9)
            while s[0] not in ('ai', 'ad'):
                s = self.stmt(u, loops, 9)
            return ('lif', self.cond(u, loops), s)
        if r < 0.86 and depth < 2:
            return ('aif', self.iexpr(u, loops), self.block(u, loops, depth + 1, 2),
                    self.block(u, loops, depth + 1, 1), self.block(u, loops, depth + 1, 2))
        subs = [x for x in self.units if x.kind == 'sub']
        if u.kind != 'fn' and subs and r < 0.97:
            return self.call(u, loops, self.pick(subs))
        return ('ai', self.lhs_i(u, loops), self.iexpr(u, loops))

    def call(self, u, loops, s):
        """Distinct variables for the arguments a subroutine may change; no
        COMMON variable passed (the subroutine may change it through COMMON)."""
        args = []
        ifree = [v for v in u.ints + [p for p, t in u.params if t == 'i'] if v not in loops]
        dfree = list(u.dbls) + [p for p, t in u.params if t == 'd']
        self.r.shuffle(ifree)
        self.r.shuffle(dfree)
        for p, t in s.params:
            if t == 'a':
                args.append('LA')
            elif t == 'i':
                args.append(ifree.pop())            # a variable: the subroutine may set it
            else:
                args.append(dfree.pop())
        return ('call', s.name, args)

    def block(self, u, loops, depth, n):
        return [self.stmt(u, loops, depth) for _ in range(self.r.randint(1, n))]

    def make(self):
        nsub, nfn = self.r.randint(1, 3), self.r.randint(0, 2)
        for k in range(nfn):
            f = Unit(f'IFN{k + 1}', 'fn', [(f'N{j}', 'i') for j in range(1, self.r.randint(1, 3) + 1)])
            f.body = self.block(f, {}, 0, 5)
            f.body.append(('ai', f.name, self.iexpr(f, {})))
            self.units.append(f)
        for k in range(nsub):
            ps = [(f'M{j}', 'i') for j in range(1, self.r.randint(1, 3) + 1)]
            ps += [(f'X{j}', 'd') for j in range(1, self.r.randint(0, 2) + 1)]
            if self.r.random() < 0.5:
                ps.append(('K', 'a'))
            s = Unit(f'SUB{k + 1}', 'sub', ps)
            s.body = self.block(s, {}, 0, 6)
            self.units.append(s)
        m = Unit('FZ', 'main', [])
        for _ in range(self.r.randint(4, 9)):
            m.body.append(self.stmt(m, {}, 0))
            if self.r.random() < 0.5:
                m.body.append(('print',))
        for s in [x for x in self.units if x.kind == 'sub']:
            m.body.append(self.call(m, {}, s))
            m.body.append(('print',))
        m.body.append(('print',))
        self.units.append(m)
        return self.units


# ------------------------------------------------------------------ rendering
def card(text, label=''):
    """A fixed-form statement, continued past column 72 at a comma or operator."""
    out = []
    first = True
    while True:
        lead = (f'{label:<5} ' if first else '     &')
        room = 66
        if len(text) <= room:
            out.append(lead + text)
            return out
        cut = max(text.rfind(c, 0, room) for c in ',+-*(')
        if cut <= 10:
            cut = room
        out.append(lead + text[:cut])
        text = text[cut:]
        first = False


class Render:
    def __init__(self):
        self.lab = 100

    def label(self):
        self.lab += 1
        return str(self.lab)

    def stmts(self, body, ind, out, u):
        for s in body:
            self.stmt(s, ind, out, u)

    def stmt(self, s, ind, out, u):
        pad = ' ' * ind
        k = s[0]
        if k == 'ai':
            out += card(f'{pad}{s[1]} = MOD({s[2]},{LIM})')
        elif k == 'ad':
            out += card(f'{pad}TD = {s[2]}')
            out += card(f'{pad}{s[1]} = TD - AINT(TD/997.0D0)*997.0D0')
        elif k == 'do':
            _, v, lo, hi, step, body = s
            l = self.label()
            out += card(f'{pad}DO {l} {v} = {lo}, {hi}' + (f', {step}' if step != 1 else ''))
            self.stmts(body, ind + 2, out, u)
            out += card(f'{pad}CONTINUE', l)
        elif k == 'if':
            out += card(f'{pad}IF ({s[1]}) THEN')
            self.stmts(s[2], ind + 2, out, u)
            if s[3]:
                out += card(f'{pad}ELSE')
                self.stmts(s[3], ind + 2, out, u)
            out += card(f'{pad}END IF')
        elif k == 'lif':
            inner = []
            self.stmt(s[2], 0, inner, u)
            if len(inner) == 1 and s[2][0] == 'ai':
                out += card(f'{pad}IF ({s[1]}) {inner[0][6:].strip()}')
            else:                                   # a double assignment is two statements
                out += card(f'{pad}IF ({s[1]}) THEN')
                self.stmt(s[2], ind + 2, out, u)
                out += card(f'{pad}END IF')
        elif k == 'aif':
            ln, lz, lp, le = self.label(), self.label(), self.label(), self.label()
            out += card(f'{pad}IF (MOD({s[1]},{LIM})) {ln}, {lz}, {lp}')
            for l, b in ((ln, s[2]), (lz, s[3]), (lp, s[4])):
                out += card(f'{pad}CONTINUE', l)
                self.stmts(b, ind + 2, out, u)
                out += card(f'{pad}GO TO {le}')
            out += card(f'{pad}CONTINUE', le)
        elif k == 'call':
            out += card(f'{pad}CALL {s[1]}({",".join(s[2])})')
        elif k == 'print':
            out += card(f'{pad}WRITE (6, 901) {",".join(u.ints)}')
            out += card(f'{pad}WRITE (6, 902) {",".join(u.dbls)}')
            out += card(f'{pad}WRITE (6, 901) KC1,KC2,(LA(JP),JP=1,10),(KA(JP),JP=1,10)')
            out += card(f'{pad}WRITE (6, 902) DC1')


def render(units):
    out = []
    for u in units:
        rd = Render()
        ps = ','.join(p for p, _ in u.params)
        head = {'main': f'PROGRAM {u.name}', 'sub': f'SUBROUTINE {u.name}({ps})',
                'fn': f'INTEGER FUNCTION {u.name}({ps})'}[u.kind]
        out += card(head)
        decl = [p for p, t in u.params if t == 'i'] + u.ints + ['J1', 'J2', 'J3', 'JP', 'LA(10)']
        out += card('INTEGER ' + ','.join(decl))
        out += card('DOUBLE PRECISION ' + ','.join([p for p, t in u.params if t == 'd'] + u.dbls + ['TD']))
        if any(t == 'a' for _, t in u.params):
            out += card('INTEGER K(10)')
        out += card('INTEGER KC1,KC2,KA(10)')
        out += card('DOUBLE PRECISION DC1')
        out += card('COMMON /CB/ KC1,KC2,KA,DC1')
        for f in units:
            if f.kind == 'fn' and f is not u:
                out += card(f'INTEGER {f.name}')
        # every local set before it is read
        for k, v in enumerate(u.ints):
            out += card(f'{v} = {(k * 7919 + len(u.name) * 31) % 1999 - 999}')
        for k, v in enumerate(u.dbls):
            out += card(f'{v} = {(k * 337 + 11) % 997 - 498}.{(k * 53) % 100:02d}D0')
        out += card('TD = 0.0D0')
        out += card(f'DO 99 J1 = 1, 10', '')
        out += card(f'LA(J1) = MOD(J1*{len(u.name) * 97 + 13}, 1999) - 999')
        if u.kind == 'main':
            out += card('KA(J1) = MOD(J1*577, 1999) - 999')
        out += card('CONTINUE', '99')
        if u.kind == 'main':
            out += card('KC1 = 123')
            out += card('KC2 = -4567')
            out += card('DC1 = 31.25D0')
        rd.stmts(u.body, 0, out, u)
        if u.kind == 'main':
            out += card('FORMAT (1X, 6I8)', '901')
            out += card('FORMAT (1X, 3E26.17E3)', '902')
        out += card('END')
    return '\n'.join(out) + '\n'


def lit(n):
    return f'({n})' if n < 0 else str(n)


def make(seed):
    return Gen(seed).make()


# ------------------------------------------------------------------ running
def sh(cmd, cwd=None, timeout=120, stdin=None):
    try:
        p = subprocess.run(cmd, cwd=cwd, capture_output=True, timeout=timeout, stdin=subprocess.DEVNULL)
        return p.returncode, p.stdout.decode('latin-1'), p.stderr.decode('latin-1')
    except subprocess.TimeoutExpired:
        return 124, '', 'timeout'


def outcome(text, d):
    """(f77, flang, gfortran) as (build-ok, status, stdout) for one program."""
    src = os.path.join(d, 'fz.f')
    with open(src, 'w') as f:
        f.write(text)
    res = {}
    for name, cmd, exe in (('f77', [F77, src, '-o', os.path.join(d, 'a.s32x')], 'a.s32x'),
                           ('flang', [FLANG, src, os.path.join(d, 'b.s32x')], 'b.s32x')):
        rc, o, e = sh(cmd, cwd=d)
        if rc != 0:
            res[name] = ('nobuild', rc, (o + e)[-400:])
            continue
        rc, o, e = sh([DBT, '-I', exe], cwd=d, timeout=60)
        res[name] = ('ran', rc, o)
    return res


def gfortran(text, d):
    src = os.path.join(d, 'g.f')
    with open(src, 'w') as f:
        f.write(text)
    rc, o, e = sh([GFORTRAN, '-O0', '-ffp-contract=off', '-std=legacy', src, '-o', os.path.join(d, 'g')], cwd=d)
    if rc != 0:
        return ('nobuild', rc, e[-400:])
    rc, o, e = sh([os.path.join(d, 'g')], cwd=d, timeout=60)
    return ('ran', rc, o)


def verdict(res):
    a, b = res['f77'], res['flang']
    if a == b:
        return 'same'
    if a[0] == 'nobuild' or b[0] == 'nobuild':
        return 'f77-nobuild' if a[0] == 'nobuild' else 'flang-nobuild'
    return 'differ'


def check(seed, keep):
    text = render(make(seed))
    d = tempfile.mkdtemp(prefix='ffuzz.')
    try:
        res = outcome(text, d)
        v = verdict(res)
        if v == 'same':
            return seed, v, ''
        who = ''
        if v == 'differ':
            g = gfortran(text, d)
            if g == res['f77']:
                who = 'flang wrong (f77 = gfortran)'
            elif g == res['flang']:
                who = 'f77 wrong (flang = gfortran)'
            else:
                who = 'all three differ'
        os.makedirs(keep, exist_ok=True)
        with open(os.path.join(keep, f'fz{seed}.f'), 'w') as f:
            f.write(text)
        return seed, v, who
    finally:
        shutil.rmtree(d, ignore_errors=True)


# ------------------------------------------------------------------ reducing
def lists(body, path=()):
    yield path, body
    for i, s in enumerate(body):
        if s[0] == 'do':
            yield from lists(s[5], path + (i, 5))
        elif s[0] == 'if':
            yield from lists(s[2], path + (i, 2))
            yield from lists(s[3], path + (i, 3))
        elif s[0] == 'aif':
            for k in (2, 3, 4):
                yield from lists(s[k], path + (i, k))


def setlist(body, path, new):
    if not path:
        return new
    i, k = path[0], path[1]
    s = list(body[i])
    s[k] = setlist(s[k], path[2:], new)
    return body[:i] + [tuple(s)] + body[i + 1:]


def shrink_once(units, ok):
    """The first deletion that keeps the difference, or None.  After a change
    the caller starts again: paths into a list that changed are stale."""
    for ui, u in enumerate(units):
        for path, body in list(lists(u.body)):
            for i, s in enumerate(body):
                cands = [body[:i] + body[i + 1:]]
                if s[0] == 'do':
                    cands.append(body[:i] + s[5] + body[i + 1:])
                elif s[0] == 'if':
                    cands.append(body[:i] + s[2] + body[i + 1:])
                for nb in cands:
                    if u.kind == 'fn' and path == () and not any(x[0] == 'ai' and x[1] == u.name for x in nb):
                        continue                    # a function must set its value
                    old = u.body
                    u.body = setlist(u.body, path, nb)
                    if ok(units):
                        return units
                    u.body = old
    for u in units:                                 # subprograms nothing calls
        if u.kind == 'main':
            continue
        rest = [x for x in units if x is not u]
        if (u.name + '(') not in render(rest) and ok(rest):
            return rest
    return None


def reduce(seed, out_path):
    units = make(seed)
    d = tempfile.mkdtemp(prefix='ffuzz.')

    def key(us):
        res = outcome(render(us), d)
        return verdict(res), res['f77'][0], res['flang'][0]

    want = key(units)
    if want[0] == 'same':
        sys.exit(f'seed {seed}: f77 and flang agree')
    while True:
        smaller = shrink_once(units, lambda us: key(us) == want)
        if smaller is None:
            break
        units = smaller
    text = render(units)
    with open(out_path, 'w') as f:
        f.write(text)
    shutil.rmtree(d, ignore_errors=True)
    print(f'{out_path}: {len(text.splitlines())} lines, {want[0]}')


def main():
    ap = argparse.ArgumentParser()
    sp = ap.add_subparsers(dest='cmd', required=True)
    r = sp.add_parser('run')
    r.add_argument('-n', type=int, default=200)
    r.add_argument('-s', type=int, default=1)
    r.add_argument('-j', type=int, default=os.cpu_count() or 4)
    r.add_argument('--keep', default='results-ffuzz')
    s = sp.add_parser('show')
    s.add_argument('seed', type=int)
    d = sp.add_parser('reduce')
    d.add_argument('seed', type=int)
    d.add_argument('-o')
    a = ap.parse_args()
    if a.cmd == 'show':
        sys.stdout.write(render(make(a.seed)))
        return 0
    if a.cmd == 'reduce':
        reduce(a.seed, a.o or f'fz{a.seed}-reduced.f')
        return 0
    bad = 0
    counts = {}
    with ThreadPoolExecutor(a.j) as pool:
        for seed, v, who in pool.map(lambda s: check(s, a.keep), range(a.s, a.s + a.n)):
            counts[v] = counts.get(v, 0) + 1
            if v != 'same':
                bad += 1
                print(f'seed {seed}: {v} {who}', flush=True)
    print(f'{a.n} programs: ' + ', '.join(f'{k} {n}' for k, n in sorted(counts.items())))
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
