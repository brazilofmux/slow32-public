#!/usr/bin/env python3
"""ffuzz.py -- random-program differential for the two Fortran compilers.

Each seed makes one deterministic fixed-form Fortran 77 program that the
standard gives one answer -- INTEGER, REAL, DOUBLE PRECISION, COMPLEX and
CHARACTER, EQUIVALENCE, DATA and BLOCK DATA (the rules for each are over
the generator below): INTEGER values kept under 2**15 (every
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
# A unit: kind 'main' | 'sub' | 'fn'; params [(name, type)] with type
# 'i' INTEGER, 'd' DOUBLE PRECISION, 'r' REAL, 'z' COMPLEX, 'c' CHARACTER*(*)
# (its actual is at least 8 long) or 'a' INTEGER(10); body a list of:
#   ('ai'|'ad'|'ar'|'az'|'as', lhs, expr)  ('do', var, lo, hi, step, body)
#   ('if', cond, then, else)  ('lif', cond, stmt)  ('aif', expr, neg, zero, pos)
#   ('call', name, args)  ('print',)
# Expressions are strings: generation is where the care is.
#
# What each feature keeps out, and why:
#   REAL, DOUBLE: every assignment reduces through a temporary (T - AINT(T/m)*m),
#     divisors are constants of magnitude 1 or more, so nothing overflows.
#   COMPLEX: + - * and CMPLX/CONJG/REAL/AIMAG only.  Division and ABS have more
#     than one right answer to the last place (Smith's algorithm or not, hypot or
#     not); complex times real is left out because whether the imaginary part
#     goes through (a*0 + b*r) decides the sign of a zero.
#   CHARACTER: no assignment reads the variable it sets (F77 forbids the
#     overlap), literals without commas or apostrophes, substrings in bounds.
#   SIGN(A,B): B is never zero.  Fortran 77 gives |A| for a B of -0.0
#     (B >= 0), Fortran 95 on lets it give -|A|; f77 is right by the first,
#     flang and gfortran by the second.  B is an integer plus one half.
#   EQUIVALENCE: only entities of one type are associated -- reading an
#     INTEGER through a DOUBLE's storage is undefined -- and none is an argument.
#   DATA: main's variables (once, at start), a BLOCK DATA for /CD/ and /CS/,
#     and a SAVEd call counter in each subroutine.

CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 '


class Unit:
    def __init__(self, name, kind, params):
        self.name, self.kind, self.params = name, kind, params
        self.ints = [f'I{k}' for k in range(1, NI + 1)]
        self.dbls = [f'D{k}' for k in range(1, ND + 1)]
        self.reals = ['R1', 'R2']
        self.cplx = ['Z1', 'Z2']
        self.chars = [('S1', 8), ('S2', 8), ('S3', 12)]
        self.body = []
        self.data = {}      # main: name -> DATA value list (rendered)
        self.entry = False  # a subroutine with an ENTRY SUBnE
        self.alt = False    # a subroutine with two alternate returns


def lit(n):
    return f'({n})' if n < 0 else str(n)


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)
        self.units = []

    def pick(self, xs):
        return self.r.choice(xs)

    def ptype(self, u, t):
        return [p for p, pt in u.params if pt == t]

    # -- what a unit may read and write
    def ivars(self, u, loops, write=False):
        if u.kind == 'fn' and write:
            vs = list(u.ints)                       # pure: locals only
        else:
            vs = u.ints + self.ptype(u, 'i') + ['KC1', 'KC2', 'KD1', 'KD2']
            if u.kind == 'main':
                vs = vs + ['IE1', 'IE2']
        return [v for v in vs if not (write and v in loops)]

    def iarrays(self, u, write=False):
        """(name, dimensions) of the INTEGER arrays."""
        if u.kind == 'fn' and write:
            return [('LA', (10,))]
        a = [(p, (10,)) for p in self.ptype(u, 'a')] + [('KA', (10,)), ('LA', (10,))]
        a += [(p, (4, 5)) for p in self.ptype(u, 'm')]   # KM(LDM,*): its actual is MA(4,5)
        if u.kind == 'main':
            a += [('IQ', (6,)), ('MA', (4, 5))]
        return a

    def dvars(self, u, write=False):
        if u.kind == 'fn' and write:
            return list(u.dbls)
        vs = u.dbls + self.ptype(u, 'd') + ['DC1']
        if u.kind == 'main':
            vs = vs + ['DE', f'DQ({self.r.randint(1, 3)})',
                       f'DM({self.r.randint(1, 3)},{self.r.randint(1, 2)})']
        return vs

    def rvars(self, u, write=False):
        if u.kind == 'fn' and write:
            return list(u.reals)
        return u.reals + self.ptype(u, 'r') + ['RD1']

    def zvars(self, u, write=False):
        if u.kind == 'fn' and write:
            return list(u.cplx)
        return u.cplx + self.ptype(u, 'z')

    def svars(self, u, write=False):
        """(name, length) of the character variables."""
        if u.kind == 'fn' and write:
            return list(u.chars)
        vs = u.chars + [(p, 8) for p in self.ptype(u, 'c')] + [('SD', 6)]
        if u.kind == 'main':
            vs = vs + [('SQ', 12), ('SE', 4)]
        return vs

    def index(self, u, loops, n=10):
        if loops and self.r.random() < 0.6:
            v = self.pick(list(loops))
            if loops[v] <= n:
                return v
        if self.r.random() < 0.5:
            return str(self.r.randint(1, n))
        return f'1+MOD(ABS({self.iatom(u, loops, 1)}),{n})'

    def aelem(self, u, loops, write=False):
        name, dims = self.pick(self.iarrays(u, write))
        return f'{name}({",".join(self.index(u, loops, n) for n in dims)})'

    # -- character pieces
    def slit(self, n=None):
        n = n or self.r.randint(1, 6)
        return "'" + ''.join(self.pick(CHARS) for _ in range(n)) + "'"

    def spiece(self, u, avoid=()):
        vs = [v for v in self.svars(u) if v[0] not in avoid]
        r = self.r.random()
        if not vs or r < 0.3:
            return self.slit()
        name, n = self.pick(vs)
        if r < 0.6:
            return name
        i = self.r.randint(1, n)
        return f'{name}({i}:{self.r.randint(i, min(n, i + 5))})'

    def sexpr(self, u, avoid):
        return '//'.join(self.spiece(u, avoid) for _ in range(self.r.randint(1, 3)))

    # -- integer: |atom| < 2**15
    def iatom(self, u, loops, depth=2):
        r = self.r.random()
        if depth <= 0 or r < 0.28:
            return self.pick(self.ivars(u, loops) + list(loops))
        if r < 0.38:
            return lit(self.r.randint(-999, 999))
        if r < 0.50:
            return self.aelem(u, loops)
        if r < 0.58:
            name, n = self.pick(self.svars(u))
            k = self.r.randint(1, n)
            return self.pick([f'ICHAR({name}({k}:{k}))', f'LEN({name})',
                              f'INDEX({self.spiece(u)},{self.spiece(u)})'])
        a = lambda: self.iatom(u, loops, depth - 1)
        c = lit(self.r.choice([x for x in range(-97, 98) if x != 0]))
        forms = [lambda: f'ABS({a()})', lambda: f'MAX({a()},{a()})', lambda: f'MIN({a()},{a()},{a()})',
                 lambda: f'MOD({a()},{c})', lambda: f'({a()}/{c})', lambda: f'ISIGN({a()},{a()})',
                 lambda: f'INT({self.datom(u, loops, 0)})', lambda: f'IDIM({a()}/2,{a()}/2)',
                 lambda: f'NINT({self.pick(self.rvars(u))})', lambda: f'INT(REAL({self.pick(self.zvars(u))}))',
                 lambda: f'ISF({a()},{a()})']
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

    # -- double
    def datom(self, u, loops, depth=2):
        r = self.r.random()
        if depth <= 0 or r < 0.35:
            return self.pick(self.dvars(u))
        if r < 0.5:
            v = f'{self.r.randint(0, 999)}.{self.r.randint(0, 99):02d}D0'
            return f'(-{v})' if self.r.random() < 0.5 else v
        if r < 0.6:
            return f'DBLE({self.iatom(u, loops, 1)})'
        if r < 0.65:
            return f'DBLE({self.pick(self.rvars(u))})'
        a = lambda: self.datom(u, loops, depth - 1)
        forms = [lambda: f'ABS({a()})', lambda: f'MAX({a()},{a()})', lambda: f'MIN({a()},{a()})',
                 lambda: f'SQRT(ABS({a()}))', lambda: f'AINT({a()})', lambda: f'ANINT({a()})',
                 lambda: f'SIGN({a()},DBLE({self.iatom(u, loops, 1)})+0.5D0)',
                 lambda: f'({a()}/{self.pick(["3.0D0", "(-7.0D0)", "1.1D0"])})',
                 lambda: f'DBLE(REAL({a()}))', lambda: f'DSF({a()})']
        return self.pick(forms)()

    def dexpr(self, u, loops):
        e = self.datom(u, loops)
        for _ in range(self.r.randint(0, 2)):
            op = self.pick('+-*')
            e = f'{e}{op}{self.datom(u, loops)}' if op != '*' else f'({e})*{self.datom(u, loops, 0)}'
        return e

    # -- real
    def ratom(self, u, loops, depth=2):
        r = self.r.random()
        if depth <= 0 or r < 0.35:
            return self.pick(self.rvars(u))
        if r < 0.47:
            v = f'{self.r.randint(0, 99)}.{self.pick(["0", "5", "25", "75", "125"])}'
            return f'(-{v})' if self.r.random() < 0.5 else v
        if r < 0.57:
            return f'REAL({self.iatom(u, loops, 1)})'
        if r < 0.65:
            z = self.pick(self.zvars(u))
            return self.pick([f'REAL({z})', f'AIMAG({z})', f'REAL({self.pick(self.dvars(u))})'])
        a = lambda: self.ratom(u, loops, depth - 1)
        forms = [lambda: f'ABS({a()})', lambda: f'MAX({a()},{a()})', lambda: f'MIN({a()},{a()})',
                 lambda: f'SQRT(ABS({a()}))', lambda: f'AINT({a()})', lambda: f'ANINT({a()})',
                 lambda: f'SIGN({a()},REAL({self.iatom(u, loops, 1)})+0.5)',
                 lambda: f'({a()}/{self.pick(["3.0", "(-7.0)", "1.5"])})']
        return self.pick(forms)()

    def rexpr(self, u, loops):
        e = self.ratom(u, loops)
        for _ in range(self.r.randint(0, 2)):
            op = self.pick('+-*')
            e = f'{e}{op}{self.ratom(u, loops)}' if op != '*' else f'({e})*{self.ratom(u, loops, 0)}'
        return e

    # -- complex: components under about 4e4 in an atom
    def zatom(self, u, loops, depth=1):
        r = self.r.random()
        if depth <= 0 or r < 0.4:
            return self.pick(self.zvars(u))
        if r < 0.55:
            return f'({self.r.randint(-60, 60)}.5,{self.r.randint(-60, 60)}.25)'
        if r < 0.85:
            return f'CMPLX({self.ratom(u, loops, 0)},{self.ratom(u, loops, 1)})'
        return f'CONJG({self.zatom(u, loops, depth - 1)})'

    def zexpr(self, u, loops):
        e = self.zatom(u, loops)
        r = self.r.random()
        if r < 0.3:
            return f'{e}*{self.zatom(u, loops, 0)}'
        if r < 0.7:
            return f'{e}{self.pick("+-")}{self.zatom(u, loops)}'
        return e

    def cond(self, u, loops, depth=1):
        r = self.r.random()
        rel = self.pick(['.LT.', '.LE.', '.EQ.', '.NE.', '.GT.', '.GE.'])
        if depth > 0 and r < 0.15:
            return f'({self.cond(u, loops, 0)}{self.pick([".AND.", ".OR.", ".EQV.", ".NEQV."])}{self.cond(u, loops, 0)})'
        if depth > 0 and r < 0.2:
            return f'(.NOT.{self.cond(u, loops, 0)})'
        if r < 0.5:
            return f'{self.iexpr(u, loops)}{rel}{self.iatom(u, loops)}'
        if r < 0.65:
            return f'{self.datom(u, loops)}{rel}{self.datom(u, loops)}'
        if r < 0.78:
            return f'{self.ratom(u, loops)}{rel}{self.ratom(u, loops)}'
        if r < 0.85:
            return f'{self.pick(self.zvars(u))}{self.pick([".EQ.", ".NE."])}{self.zatom(u, loops)}'
        if r < 0.93:
            return f'{self.pick(["LLT", "LLE", "LGT", "LGE"])}({self.spiece(u)},{self.spiece(u)})'
        return f'{self.spiece(u)}{rel}{self.spiece(u)}'

    # -- statements
    def lhs_i(self, u, loops):
        if self.r.random() < 0.25:
            return self.aelem(u, loops, write=True)
        return self.pick(self.ivars(u, loops, write=True))

    def assign(self, u, loops):
        r = self.r.random()
        if r < 0.36:
            return ('ai', self.lhs_i(u, loops), self.iexpr(u, loops))
        if r < 0.54:
            return ('ad', self.pick(self.dvars(u, write=True)), self.dexpr(u, loops))
        if r < 0.70:
            return ('ar', self.pick(self.rvars(u, write=True)), self.rexpr(u, loops))
        if r < 0.82:
            return ('az', self.pick(self.zvars(u, write=True)), self.zexpr(u, loops))
        name, n = self.pick(self.svars(u, write=True))
        avoid = {name} | ({'SQ', 'SE'} if name in ('SQ', 'SE') else set())
        lhs = name
        if self.r.random() < 0.3:
            i = self.r.randint(1, n)
            lhs = f'{name}({i}:{self.r.randint(i, n)})'
        return ('as', lhs, self.sexpr(u, avoid))

    def stmt(self, u, loops, depth):
        r = self.r.random()
        if r < 0.50:
            return self.assign(u, loops)
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
            return ('lif', self.cond(u, loops), self.assign(u, loops))
        if r < 0.84 and depth < 2:
            return ('aif', self.iexpr(u, loops), self.block(u, loops, depth + 1, 2),
                    self.block(u, loops, depth + 1, 1), self.block(u, loops, depth + 1, 2))
        if r < 0.87 and depth < 2:
            b = lambda: self.block(u, loops, depth + 1, 2)
            return ('cgoto', self.iexpr(u, loops), b(), b(), b(), b())
        if r < 0.89 and depth < 2:
            return ('agoto', self.cond(u, loops), self.block(u, loops, depth + 1, 2),
                    self.block(u, loops, depth + 1, 2))
        if r < 0.92 and u.kind != 'fn':
            return self.iorw(u, loops)
        subs = [x for x in self.units if x.kind == 'sub' and not x.alt and self.can_call(u, x)]
        alts = [x for x in self.units if x.kind == 'sub' and x.alt and self.can_call(u, x)]
        if u.kind != 'fn' and alts and depth < 2 and r < 0.94:
            c = self.call(u, loops, self.pick(alts))
            b = lambda: self.block(u, loops, depth + 1, 2)
            return ('altcall', c[1], c[2], b(), b(), b())
        if u.kind != 'fn' and subs and r < 0.98:
            return self.call(u, loops, self.pick(subs))
        return self.assign(u, loops)

    def can_call(self, u, s):
        """A 2-D array dummy needs a 4x5 array to pass: main's MA, or the
        caller's own KM."""
        return u.kind == 'main' or not self.ptype(s, 'm') or bool(self.ptype(u, 'm'))

    def iorw(self, u, loops):
        """Values through the character variable SB: written with a format, read
        back with it or list-directed.  Variables only -- a function referenced
        in an I/O statement may not itself do I/O, and this keeps them out."""
        iv = [v for v in self.ivars(u, loops, write=True) if v in u.ints]
        return ('iorw', self.r.random() < 0.5, self.pick(iv), self.pick(u.dbls), self.pick(u.reals),
                self.pick(self.ivars(u, loops)), self.pick(self.dvars(u)), self.pick(self.rvars(u)))

    def call(self, u, loops, s):
        """Distinct variables for the arguments a subroutine may change; no
        COMMON or EQUIVALENCEd variable passed."""
        free = {
            'i': [v for v in u.ints + self.ptype(u, 'i') if v not in loops],
            'd': u.dbls + self.ptype(u, 'd'),
            'r': u.reals + self.ptype(u, 'r'),
            'z': u.cplx + self.ptype(u, 'z'),
            'c': [v for v, n in u.chars if n >= 8] + self.ptype(u, 'c'),
        }
        for xs in free.values():
            self.r.shuffle(xs)
        args = []
        for p, t in s.params:
            if t == 'a':
                args.append('LA')
            elif t == 'm':
                args.append('MA' if u.kind == 'main' else self.ptype(u, 'm')[0])
            elif t == 'n':
                args.append('4')                    # KM's leading dimension
            else:
                args.append(free[t].pop())
        name = s.name + ('E' if s.entry and self.r.random() < 0.5 else '')
        return ('call', name, args)

    def block(self, u, loops, depth, n):
        return [self.stmt(u, loops, depth) for _ in range(self.r.randint(1, n))]

    def dval(self, t):
        r = self.r
        if t == 'i':
            return str(r.randint(-999, 999))
        if t == 'd':
            return f'{r.randint(-999, 999)}.{r.randint(0, 99):02d}D0'
        if t == 'r':
            return f'{r.randint(-99, 99)}.{self.pick(["0", "5", "25", "75"])}'
        if t == 'z':
            return f'({r.randint(-60, 60)}.5,{r.randint(-60, 60)}.25)'
        return self.slit(r.randint(1, 8))

    def make(self):
        nsub, nfn = self.r.randint(1, 3), self.r.randint(0, 2)
        for k in range(nfn):
            f = Unit(f'IFN{k + 1}', 'fn', [(f'N{j}', 'i') for j in range(1, self.r.randint(1, 3) + 1)])
            f.body = self.block(f, {}, 0, 5)
            f.body.append(('ai', f.name, self.iexpr(f, {})))
            self.units.append(f)
        for k in range(nsub):
            ps = [(f'M{j}', 'i') for j in range(1, self.r.randint(1, 3) + 1)]
            for t, nm, most in (('d', 'X', 2), ('r', 'Y', 1), ('z', 'W', 1), ('c', 'SA', 1)):
                ps += [(f'{nm}{j}' if nm != 'SA' else f'SA{j}', t) for j in range(1, self.r.randint(0, most) + 1)]
            if self.r.random() < 0.5:
                ps.append(('K', 'a'))
            if self.r.random() < 0.4:
                ps += [('KM', 'm'), ('LDM', 'n')]
            s = Unit(f'SUB{k + 1}', 'sub', ps)
            s.entry = self.r.random() < 0.4
            s.alt = (not s.entry) and k > 0 and self.r.random() < 0.4
            s.body = self.block(s, {}, 0, 6)
            self.units.append(s)
        m = Unit('FZ', 'main', [])
        for vs, t in ((m.ints, 'i'), (m.dbls, 'd'), (m.reals, 'r'), (m.cplx, 'z')):
            for v in vs:
                m.data[v] = [self.dval(t)]
        for v, n in m.chars:
            m.data[v] = [self.dval('c')]
        m.data['LA'] = [f'{self.r.randint(1, 5)}*{self.dval("i")}']
        m.data['LA'].append(f'{10 - int(m.data["LA"][0].split("*")[0])}*{self.dval("i")}')
        m.data['IQ'] = [self.dval('i') for _ in range(6)]
        m.data['DQ'] = [f'3*{self.dval("d")}']
        m.data['SQ'] = [self.slit(12)]
        m.data['MA'] = [self.dval('i') for _ in range(20)]
        m.data['DM'] = [f'2*{self.dval("d")}', f'4*{self.dval("d")}']
        for _ in range(self.r.randint(4, 9)):
            m.body.append(self.stmt(m, {}, 0))
            if self.r.random() < 0.5:
                m.body.append(('print',))
        for s in [x for x in self.units if x.kind == 'sub']:
            c = self.call(m, {}, s)
            if s.alt:
                c = ('altcall', c[1], c[2], [('print',)], [('print',)], [('print',)])
            m.body.append(c)
            m.body.append(('print',))
        m.body.append(('print',))
        self.units.append(m)
        self.bd = {'KD1': self.dval('i'), 'KD2': self.dval('i'), 'RD1': self.dval('r'), 'SD': self.slit(6)}
        return self.units


# ------------------------------------------------------------------ rendering
def card(text, label=''):
    """A fixed-form statement, continued past column 72 at a comma or an
    operator -- never inside a character constant, where the blanks a short
    line is padded with would become part of the constant."""
    out = []
    first = True
    while True:
        lead = (f'{label:<5} ' if first else '     &')
        room = 66
        if len(text) <= room:
            out.append(lead + text)
            return out
        quoted, ok = False, []
        for i, ch in enumerate(text[:room]):
            if ch == "'":
                quoted = not quoted
            ok.append(not quoted)
        cuts = [i for i in range(11, room) if ok[i] and text[i] in ',+-*(/' and (i == 0 or ok[i - 1])]
        cut = cuts[-1] if cuts else room
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
        elif k == 'ar':
            out += card(f'{pad}TR = {s[2]}')
            out += card(f'{pad}{s[1]} = TR - AINT(TR/97.0)*97.0')
        elif k == 'az':
            out += card(f'{pad}TZ = {s[2]}')
            out += card(f'{pad}TR = REAL(TZ) - AINT(REAL(TZ)/97.0)*97.0')
            out += card(f'{pad}TQ = AIMAG(TZ) - AINT(AIMAG(TZ)/97.0)*97.0')
            out += card(f'{pad}{s[1]} = CMPLX(TR,TQ)')
        elif k == 'as':
            out += card(f'{pad}{s[1]} = {s[2]}')
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
            if len(inner) == 1 and s[2][0] in ('ai', 'as'):
                out += card(f'{pad}IF ({s[1]}) {inner[0][6:].strip()}')
            else:                                   # several statements, or several cards
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
        elif k == 'altcall':
            l1, l2, le = self.label(), self.label(), self.label()
            out += card(f'{pad}CALL {s[1]}({",".join(s[2] + ["*" + l1, "*" + l2])})')
            self.stmts(s[3], ind + 2, out, u)
            out += card(f'{pad}GO TO {le}')
            out += card(f'{pad}CONTINUE', l1)
            self.stmts(s[4], ind + 2, out, u)
            out += card(f'{pad}GO TO {le}')
            out += card(f'{pad}CONTINUE', l2)
            self.stmts(s[5], ind + 2, out, u)
            out += card(f'{pad}CONTINUE', le)
        elif k == 'cgoto':
            ls = [self.label() for _ in range(3)]
            le = self.label()
            out += card(f'{pad}GO TO ({",".join(ls)}), 1+MOD(ABS(MOD({s[1]},{LIM})),4)')
            self.stmts(s[5], ind + 2, out, u)        # the index past the list falls through
            out += card(f'{pad}GO TO {le}')
            for l, b in zip(ls, s[2:5]):
                out += card(f'{pad}CONTINUE', l)
                self.stmts(b, ind + 2, out, u)
                out += card(f'{pad}GO TO {le}')
            out += card(f'{pad}CONTINUE', le)
        elif k == 'agoto':
            l1, l2, le = self.label(), self.label(), self.label()
            out += card(f'{pad}IF ({s[1]}) THEN')
            out += card(f'{pad}  ASSIGN {l1} TO NLAB')
            out += card(f'{pad}ELSE')
            out += card(f'{pad}  ASSIGN {l2} TO NLAB')
            out += card(f'{pad}END IF')
            out += card(f'{pad}GO TO NLAB, ({l1}, {l2})')
            out += card(f'{pad}CONTINUE', l1)
            self.stmts(s[2], ind + 2, out, u)
            out += card(f'{pad}GO TO {le}')
            out += card(f'{pad}CONTINUE', l2)
            self.stmts(s[3], ind + 2, out, u)
            out += card(f'{pad}CONTINUE', le)
        elif k == 'iorw':
            _, listed, il, dl, rl, iv, dv, rv = s
            out += card(f'{pad}WRITE (SB, 905) {iv},{dv},{rv}')
            out += card(f'{pad}READ (SB, {"*" if listed else "905"}) {il},{dl},{rl}')
        elif k == 'print':
            for fmt, items in (('901', ','.join(u.ints) + ',IE1,IE2'),
                               ('902', ','.join(u.dbls) + ',DE,DC1'),
                               ('903', ','.join(u.reals) + ',RD1'),
                               ('903', ','.join(u.cplx)),
                               ('904', ','.join(v for v, _ in u.chars) + ',SQ,SE,SD,SB'),
                               ('901', 'KC1,KC2,KD1,KD2,(LA(JP),JP=1,10),(KA(JP),JP=1,10),(IQ(JP),JP=1,6)'),
                               ('901', 'MA'),
                               ('902', '(DQ(JP),JP=1,3),DM')):
                out += card(f'{pad}WRITE (6, {fmt}) {items}')


def commons():
    return ['INTEGER KC1,KC2,KA(10),KD1,KD2', 'DOUBLE PRECISION DC1', 'REAL RD1', 'CHARACTER*6 SD',
            'COMMON /CB/ KC1,KC2,KA,DC1', 'COMMON /CD/ KD1,KD2,RD1', 'COMMON /CS/ SD']


def render(units):
    bd = getattr(units, 'bd', None)
    out = []
    for u in units:
        rd = Render()
        ps = ','.join(p for p, _ in u.params)
        pt = lambda t: [p for p, x in u.params if x == t]
        sps = ps + (',*,*' if u.alt else '')
        head = {'main': f'PROGRAM {u.name}', 'sub': f'SUBROUTINE {u.name}({sps})',
                'fn': f'INTEGER FUNCTION {u.name}({ps})'}[u.kind]
        out += card(head)
        decl = pt('i') + pt('n') + u.ints + ['J1', 'J2', 'J3', 'JP', 'LA(10)', 'NLAB']
        if u.kind == 'main':
            decl += ['IQ(6)', 'IE1', 'IE2', 'MA(4,5)']
        if pt('m'):
            decl += ['KM(LDM,*)']
        if u.kind == 'sub':
            decl += ['NFLAG', 'NALT']
        out += card('INTEGER ' + ','.join(decl))
        out += card('DOUBLE PRECISION ' + ','.join(pt('d') + u.dbls + ['TD', 'DSF', 'XX'] +
                                                   (['DQ(3)', 'DE', 'DM(3,2)'] if u.kind == 'main' else [])))
        out += card('REAL ' + ','.join(pt('r') + u.reals + ['TR', 'TQ']))
        out += card('COMPLEX ' + ','.join(pt('z') + u.cplx + ['TZ']))
        for v, n in u.chars:
            out += card(f'CHARACTER*{n} {v}')
        for p in pt('c'):
            out += card(f'CHARACTER*(*) {p}')
        if pt('a'):
            out += card('INTEGER K(10)')
        if u.kind == 'main':
            out += card('CHARACTER*12 SQ')
            out += card('CHARACTER*4 SE')
            out += card('EQUIVALENCE (IQ(2),IE1),(IQ(5),IE2),(DQ(2),DE),(SQ(5:8),SE)')
        for c in commons():
            out += card(c)
        for f in units:
            if f.kind == 'fn' and f is not u:
                out += card(f'INTEGER {f.name}')
        if u.kind != 'fn':
            out += card('CHARACTER*60 SB')
        if u.kind == 'sub':
            out += card('INTEGER NCALL')
            out += card('SAVE NCALL')
        # statement functions, after the specifications
        out += card('ISF(IX,IY) = MOD(IX*3-IY,1000) + IX/7')
        out += card('DSF(XX) = XX*0.5D0 - AINT(XX/3.0D0)')
        if u.kind == 'sub':
            out += card('DATA NCALL /0/')
        if u.entry:                                 # both ways in join before the locals are set
            out += card('NFLAG = 1')
            out += card('GO TO 50')
            out += card(f'ENTRY {u.name}E({ps})')
            out += card('NFLAG = 2')
            out += card('CONTINUE', '50')
        elif u.kind == 'sub':
            out += card('NFLAG = 3')
        if u.kind == 'main':
            for v, vals in u.data.items():
                out += card(f'DATA {v} /{",".join(vals)}/')
            out += card("DATA SB /' '/")
        else:                                       # every local set before it is read
            for k, v in enumerate(u.ints):
                out += card(f'{v} = {(k * 7919 + len(u.name) * 31) % 1999 - 999}')
            for k, v in enumerate(u.dbls):
                out += card(f'{v} = {(k * 337 + 11) % 997 - 498}.{(k * 53) % 100:02d}D0')
            for k, v in enumerate(u.reals):
                out += card(f'{v} = {(k * 31 + len(u.name)) % 97 - 48}.5')
            for k, v in enumerate(u.cplx):
                out += card(f'{v} = ({(k * 17 + 3) % 61 - 30}.25,{(k * 29 + 7) % 61 - 30}.5)')
            for k, (v, n) in enumerate(u.chars):
                out += card(f"{v} = '{u.name[:3]}{'QRSTUVWXYZ'[k]}{k}'")
            out += card('DO 99 J1 = 1, 10')
            out += card(f'LA(J1) = MOD(J1*{len(u.name) * 97 + 13}, 1999) - 999')
            out += card('CONTINUE', '99')
            if u.kind == 'sub':
                out += card("SB = ' '")
        if u.kind == 'sub':
            out += card('NCALL = NCALL + 1')
            out += card(f'KD2 = MOD(KD2 + NCALL*{len(u.name) * 7} + NFLAG, {LIM})')
        if u.kind == 'main':
            out += card('DO 99 J1 = 1, 10')
            out += card('KA(J1) = MOD(J1*577, 1999) - 999')
            out += card('CONTINUE', '99')
            out += card('KC1 = 123')
            out += card('KC2 = -4567')
            out += card('DC1 = 31.25D0')
        rd.stmts(u.body, 0, out, u)
        if u.alt:
            out += card('NALT = MOD(ABS(M1), 3)')
            out += card('IF (NALT .EQ. 1) RETURN 1')
            out += card('IF (NALT .EQ. 2) RETURN 2')
        if u.kind != 'fn':
            out += card('FORMAT (I7,1X,E25.16E3,1X,F10.3)', '905')
        if u.kind == 'main':
            out += card('FORMAT (1X, 6I8)', '901')
            out += card('FORMAT (1X, 3E26.17E3)', '902')
            out += card('FORMAT (1X, 4E17.9E2)', '903')
            out += card("FORMAT (1X, 6('[',A,']'))", '904')
        out += card('END')
    if bd:
        out += card('BLOCK DATA BDZ')
        for c in ('INTEGER KD1,KD2', 'REAL RD1', 'CHARACTER*6 SD', 'COMMON /CD/ KD1,KD2,RD1', 'COMMON /CS/ SD'):
            out += card(c)
        out += card(f"DATA KD1,KD2,RD1,SD /{bd['KD1']},{bd['KD2']},{bd['RD1']},{bd['SD']}/")
        out += card('END')
    return '\n'.join(out) + '\n'


class Program(list):
    """The units, and the BLOCK DATA's values (the reducer keeps them)."""
    bd = None


def make(seed):
    g = Gen(seed)
    p = Program(g.make())
    p.bd = g.bd
    return p


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
        elif s[0] == 'cgoto':
            for k in (2, 3, 4, 5):
                yield from lists(s[k], path + (i, k))
        elif s[0] == 'agoto':
            for k in (2, 3):
                yield from lists(s[k], path + (i, k))
        elif s[0] == 'altcall':
            for k in (3, 4, 5):
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
        rest = Program(x for x in units if x is not u)
        rest.bd = units.bd
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
