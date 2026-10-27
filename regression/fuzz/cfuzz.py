#!/usr/bin/env python3
"""cfuzz.py -- random-program differential for the stage08 C compiler.

Each seed makes one deterministic C program with no undefined behaviour:
32- and 64-bit unsigned arithmetic (wraps), shifts by constants below the
width, division by an odd number, small local arrays indexed under a mask,
counted loops (constant trip counts, so full unrolling and last-pass
peeling fire), branches on the loop variable and on data, early returns,
switch, break and continue, and calls between the generated functions.
main prints one line per call.  The program is built with clang (the
oracle) and with stage08's cc.s32x, both run under the reference-speed
emulator, and the outputs must match.

    cfuzz.py run   [-n N] [-s FIRST] [-j JOBS] [--cc CC] [--keep DIR]
    cfuzz.py show  SEED                 # print the program for SEED
    cfuzz.py reduce SEED [--cc CC] [-o OUT.c]

`run` reports each seed whose outputs differ (or whose stage08 build or
run fails where clang's succeeds) and writes it to --keep (default
results-fuzz/).  `reduce` deletes statements, functions and calls and
shrinks expressions while the difference persists, and writes the small
program.  The reduced program is the regression test to add to
selfhost/stage08/tests.

Environment: KIT (default ~/s32x: crt0, libc, include for stage08
builds), LLVM (default ~/llvm-project/build/bin).
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
KIT = os.environ.get('KIT', os.path.expanduser('~/s32x'))
LLVM = os.environ.get('LLVM', os.path.expanduser('~/llvm-project/build/bin'))
AS = os.path.join(ROOT, 'tools/assembler/slow32asm')
LD = os.path.join(ROOT, 'tools/linker/s32-ld')
EMU = os.path.join(ROOT, 'tools/emulator/slow32-fast')
DBT = os.path.join(ROOT, 'tools/dbt/slow32-dbt')
CC08 = os.path.join(ROOT, 'selfhost/stage08/cc.s32x')

# ---------------------------------------------------------------- AST
# A program is a list of functions; a function holds a statement list.
# Statements and expressions are small lists/tuples so the reducer can
# walk and rewrite them.  Every expression has unsigned 32-bit value.


class Fn:
    def __init__(self, name, nparams):
        self.name = name
        self.nparams = nparams
        self.locals = []        # (name, ctype, init expr)
        self.arrays = []        # (name, ctype, n)
        self.ptrs = []          # unsigned * locals
        self.structs = []       # struct S locals
        self.body = []          # statements
        self.ret = None         # expression


PROFILE = 'wide'     # 'narrow': scalars, local arrays and counted loops only


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)
        self.wide_on = PROFILE != 'narrow'
        self.fns = []
        self.depth = 0
        self.loopvars = []      # loop variables in scope (read-only in bodies)
        self.loop_nest = 0
        self.cur = None
        self.nloop = 0

    # -- expressions
    def leaf(self):
        r = self.r
        c = r.random()
        f = self.cur
        if c < 0.18:
            return ('k', r.choice([0, 1, 2, 3, 5, 7, 8, 13, 16, 31, 100, 255, 256, 1000,
                                    0x7f, 0x80, 0xff, 0x1b, 4096, 65535, 0x7fffffff,
                                    0x80000000, 0xffffffff, r.randrange(1 << 32)]))
        if c < 0.30 and f.nparams:
            return ('v', 'p%d' % r.randrange(f.nparams))
        if c < 0.45 and self.loopvars:
            return ('v', '(unsigned)' + r.choice(self.loopvars))
        if c < 0.58 and f.arrays:
            a = r.choice(f.arrays)
            return ('idx', a[0], a[2], self.expr(2))
        if c < 0.64 and self.wide_on:
            return ('v', r.choice(GLOBALS))
        if c < 0.68 and self.wide_on:
            return ('idx', 'ga', 8, self.expr(1))
        if c < 0.74 and f.ptrs:
            return ('deref', r.choice(f.ptrs))
        if c < 0.80 and f.structs:
            return ('fld', r.choice(f.structs), r.choice('abcd'))
        if f.locals:
            return ('v', r.choice(f.locals)[0])
        return ('k', r.randrange(1 << 16))

    def expr(self, d=None):
        r = self.r
        if d is None:
            d = 3
        if d <= 0 or r.random() < 0.25:
            return self.leaf()
        c = r.random()
        if c < 0.40:
            return ('bin', r.choice(['+', '-', '*', '&', '|', '^', '+', '-', '^']),
                    self.expr(d - 1), self.expr(d - 1))
        if c < 0.50:
            return ('shl' if r.random() < 0.5 else 'shr', self.expr(d - 1), r.randrange(32))
        if c < 0.55:
            return ('sar', self.expr(d - 1), r.randrange(32))
        if c < 0.63:
            return ('cmp', r.choice(['<', '<=', '>', '>=', '==', '!=']),
                    r.random() < 0.3, self.expr(d - 1), self.expr(d - 1))
        if c < 0.69:
            return ('sel', self.cond(d - 1), self.expr(d - 1), self.expr(d - 1))
        if c < 0.74:
            return ('cast', r.choice(['unsigned char', 'unsigned short', 'signed char', 'short']),
                    self.expr(d - 1))
        if c < 0.78:
            return ('div', r.choice(['/', '%']), self.expr(d - 1), self.expr(d - 1))
        if c < 0.82:
            return ('mulhi', self.expr(d - 1), self.expr(d - 1))
        if c < 0.86:
            return ('un', r.choice(['~', '-', '!']), self.expr(d - 1))
        if c < 0.92:
            callees = [g for g in self.fns if g is not self.cur]
            if callees and self.loop_nest <= 1:
                g = r.choice(callees)
                return ('call', g.name, [self.expr(d - 2) for _ in range(g.nparams)])
        if c < 0.95 and self.wide_on:
            return self.wide(d)
        return ('addk', self.expr(d - 1), r.choice([1, 4, 5, 100, 1000, 2047, -1, -5, -2048]))

    def wide(self, d):
        """The second-tier forms: 64-bit, signed in range, floating point,
        and the fixed helpers that take pointers and structs."""
        r = self.r
        f = self.cur
        c = r.random()
        if c < 0.30:
            return ('w64', r.choice(['+', '-', '*', '^', '/', '%', '>>']),
                    self.expr(d - 1), self.expr(d - 1), r.randrange(64))
        if c < 0.55:
            return ('sgn', r.choice(['+', '-', '*', '/', '%', '>>', '<', '>=']),
                    self.expr(d - 1), self.expr(d - 1))
        if c < 0.80:
            return ('flt', self.dexpr(2))
        u4 = [a for a in f.arrays if a[1] == 'unsigned' and a[2] >= 4] + [('ga', 'unsigned', 8)]
        if c < 0.90:
            return ('sum4', r.choice(u4)[0])
        if f.structs:
            return ('shash', r.choice(f.structs))
        return ('sum4', 'ga')

    def dexpr(self, d):
        r = self.r
        if d <= 0 or r.random() < 0.3:
            if r.random() < 0.3:
                return ('dk', r.choice(['0.5', '1.0', '3.25', '-2.75', '1e3', '0.1']))
            return ('dv', self.expr(1))
        c = r.random()
        if c < 0.55:
            return ('dbin', r.choice(['+', '-', '*']), self.dexpr(d - 1), self.dexpr(d - 1))
        if c < 0.75:
            return ('ddiv', self.dexpr(d - 1), self.expr(1))
        if c < 0.90:
            return ('dflt', self.dexpr(d - 1))
        return ('dsel', r.choice(['<', '>=', '==']), self.dexpr(d - 1), self.dexpr(d - 1),
                self.dexpr(d - 1), self.dexpr(d - 1))

    def cond(self, d=2):
        r = self.r
        if self.loopvars and r.random() < 0.35:
            # a test on the loop variable against a constant: what the
            # unroller folds and the peeler decides
            return ('lcmp', r.choice(['==', '!=', '<', '>=']), r.choice(self.loopvars),
                    r.randrange(0, 12))
        return ('cmp', r.choice(['<', '<=', '>', '>=', '==', '!=']), r.random() < 0.3,
                self.expr(d), self.expr(d))

    # -- statements
    def lvalue(self):
        f = self.cur
        c = self.r.random()
        if f.arrays and c < 0.25:
            a = self.r.choice(f.arrays)
            return ('idx', a[0], a[2], self.expr(1))
        if c < 0.33 and self.wide_on:
            return ('v', self.r.choice(GLOBALS))
        if c < 0.38 and self.wide_on:
            return ('idx', 'ga', 8, self.expr(1))
        if c < 0.48 and f.ptrs:
            return ('deref', self.r.choice(f.ptrs))
        if c < 0.58 and f.structs:
            return ('fld', self.r.choice(f.structs), self.r.choice('abcd'))
        return ('v', self.r.choice([l for l in f.locals if not l[0].startswith('i')])[0])

    def stmts(self, n):
        out = []
        for _ in range(n):
            s = self.stmt()
            if s:
                out.append(s)
        return out

    def stmt(self):
        r = self.r
        c = r.random()
        if self.depth >= 3:
            c = c * 0.45
        if c < 0.40:
            return ('asg', r.choice(['=', '+=', '^=', '-=', '|=', '=']), self.lvalue(), self.expr())
        self.depth += 1
        try:
            if c < 0.58:
                els = self.stmts(r.randrange(0, 3)) if r.random() < 0.5 else None
                return ('if', self.cond(), self.stmts(r.randrange(1, 4)), els)
            if c < 0.80 and self.loop_nest < 2:
                return self.loop()
            if c < 0.86:
                return ('ret', self.cond(), self.expr())
            if c < 0.92:
                cases = []
                for k in r.sample(range(6), r.randrange(1, 4)):
                    cases.append((k, self.stmts(r.randrange(1, 3)), r.random() < 0.7))
                return ('switch', self.expr(2), cases)
            if c < 0.95 and self.loop_nest > 0:
                return ('brk', r.choice(['break', 'continue']), self.cond())
            if not self.wide_on:
                return ('asg', '=', self.lvalue(), self.expr())
            return self.stmt2()
        finally:
            self.depth -= 1

    def stmt2(self):
        """Pointers, structs, while/do and goto."""
        r = self.r
        f = self.cur
        c = r.random()
        if c < 0.18 and f.ptrs:
            return ('pset', r.choice(f.ptrs), self.ptarget())
        if c < 0.28 and f.ptrs:
            return ('bump', r.choice(f.ptrs), self.expr(2))
        if c < 0.36 and len(f.structs) >= 2:
            a, b = r.sample(f.structs, 2)
            return ('scopy', a, b)
        if c < 0.46 and f.structs:
            return ('smix', r.choice(f.structs), r.choice(f.structs), self.expr(2))
        if c < 0.70 and self.loop_nest < 2:
            return self.wloop()
        if c < 0.80:
            return ('goto', self.cond())
        return ('asg', '=', self.lvalue(), self.expr())

    def ptarget(self):
        """Something an unsigned * may point at, valid for the whole call."""
        r = self.r
        f = self.cur
        opts = [('addr', g) for g in GLOBALS[:3]]
        opts += [('addr', n) for (n, t, _) in f.locals if t == 'unsigned' and not n.startswith('i')]
        opts += [('addre', a[0], a[2], self.expr(1)) for a in f.arrays if a[1] == 'unsigned']
        opts += [('addre', 'ga', 8, self.expr(1))]
        opts += [('addrf', st) for st in f.structs]
        return r.choice(opts)

    def wloop(self):
        r = self.r
        v = 'i%d' % self.nloop
        self.nloop += 1
        self.cur.locals.append((v, 'int', ('k', 0)))
        kind = r.choice(['while', 'do'])
        lim = r.randrange(1, 12)
        self.loopvars.append(v)
        self.loop_nest += 1
        cond = self.cond()
        body = self.stmts(r.randrange(1, 4))
        self.loop_nest -= 1
        self.loopvars.pop()
        return (kind, v, lim, cond, body)

    def loop(self):
        r = self.r
        v = 'i%d' % self.nloop
        self.nloop += 1
        self.cur.locals.append((v, 'int', ('k', 0)))
        lo = r.randrange(0, 4)
        hi = lo + r.randrange(1, 15)
        step = r.choice([1, 1, 1, 2, 3])
        down = r.random() < 0.2
        self.loopvars.append(v)
        self.loop_nest += 1
        body = self.stmts(r.randrange(1, 5))
        self.loop_nest -= 1
        self.loopvars.pop()
        return ('for', v, lo, hi, step, down, body)

    def function(self, name):
        r = self.r
        f = Fn(name, r.randrange(1, 4))
        self.cur = f
        self.nloop = 0
        for k in range(r.randrange(2, 6)):
            t = r.choice(['unsigned', 'unsigned', 'unsigned', 'int', 'unsigned char',
                          'unsigned short', 'unsigned long long'])
            f.locals.append(('v%d' % k, t, None))
        for k in range(r.randrange(0, 3)):
            f.arrays.append(('a%d' % k, r.choice(['unsigned', 'unsigned char', 'unsigned short']),
                             r.choice([4, 8, 16])))
        for k in range(r.choice([0, 0, 1, 2]) if self.wide_on else 0):
            f.structs.append('s%d' % k)
        for k in range(r.choice([0, 0, 1, 2]) if self.wide_on else 0):
            f.ptrs.append('q%d' % k)
        f.locals = [(n, t, self.expr(2)) for (n, t, _) in f.locals]
        f.body = self.stmts(r.randrange(2, 7))
        f.ret = self.expr()
        return f

    def program(self):
        for k in range(self.r.randrange(2, 6)):
            self.fns.append(self.function('f%d' % k))
        calls = []
        for g in self.fns:
            for _ in range(self.r.randrange(1, 4)):
                calls.append((g.name, [self.r.randrange(1 << 32) if self.r.random() < 0.5
                                       else self.r.randrange(64) for _ in range(g.nparams)]))
        return self.fns, calls


# ---------------------------------------------------------------- render
def rx(e):
    k = e[0]
    if k == 'k':
        return '%uu' % (e[1] & 0xffffffff)
    if k == 'v':
        return '(unsigned)%s' % e[1] if not e[1].startswith('(') else e[1]
    if k == 'idx':
        return '(unsigned)%s[(%s) & %du]' % (e[1], rx(e[3]), e[2] - 1)
    if k == 'bin':
        return '(%s %s %s)' % (rx(e[2]), e[1], rx(e[3]))
    if k == 'shl':
        return '(%s << %d)' % (rx(e[1]), e[2])
    if k == 'shr':
        return '(%s >> %d)' % (rx(e[1]), e[2])
    if k == 'sar':
        return '(unsigned)((int)%s >> %d)' % (rx(e[1]), e[2])
    if k == 'cmp':
        if e[2]:
            return '(unsigned)((int)%s %s (int)%s)' % (rx(e[3]), e[1], rx(e[4]))
        return '(unsigned)(%s %s %s)' % (rx(e[3]), e[1], rx(e[4]))
    if k == 'lcmp':
        return '(unsigned)(%s %s %d)' % (e[2], e[1], e[3])
    if k == 'sel':
        return '(%s ? %s : %s)' % (rx(e[1]), rx(e[2]), rx(e[3]))
    if k == 'cast':
        return '(unsigned)(%s)%s' % (e[1], rx(e[2]))
    if k == 'div':
        return '(%s %s (%s | 1u))' % (rx(e[2]), e[1], rx(e[3]))
    if k == 'mulhi':
        return '(unsigned)(((unsigned long long)%s * %s) >> 32)' % (rx(e[1]), rx(e[2]))
    if k == 'un':
        if e[1] == '!':
            return '(unsigned)!%s' % rx(e[2])
        if e[1] == '-':
            return '(0u - %s)' % rx(e[2])
        return '(~%s)' % rx(e[2])
    if k == 'call':
        return '%s(%s)' % (e[1], ', '.join(rx(a) for a in e[2]) or '')
    if k == 'addk':
        return '(%s + %du)' % (rx(e[1]), e[2]) if e[2] >= 0 else '(%s - %du)' % (rx(e[1]), -e[2])
    if k == 'deref':
        return '(*%s)' % e[1]
    if k == 'fld':
        return '(unsigned)%s.%s' % (e[1], e[2])
    if k == 'w64':
        a = '(((unsigned long long)%s << 17) | %s)' % (rx(e[2]), rx(e[3]))
        b = '((unsigned long long)%s * 2654435761u + 1u)' % rx(e[3])
        if e[1] in ('/', '%'):
            return '(unsigned)((%s %s (%s | 1u)) >> %d)' % (a, e[1], b, e[4] % 32)
        if e[1] == '>>':
            return '(unsigned)(%s >> %d)' % (a, e[4])
        return '(unsigned)((%s %s %s) >> %d)' % (a, e[1], b, e[4] % 48)
    if k == 'sgn':
        a = '((int)(%s & 0xffffu) - 32768)' % rx(e[2])
        b = '((int)(%s & 0x7fffu))' % rx(e[3])
        if e[1] in ('/', '%'):
            return '(unsigned)(%s %s (%s | 1))' % (a, e[1], b)
        if e[1] == '>>':
            return '(unsigned)(%s >> (int)(%s & 15u))' % (a, rx(e[3]))
        return '(unsigned)(%s %s %s)' % (a, e[1], b)
    if k == 'flt':
        return 'fcv(%s)' % rd(e[1])
    if k == 'sum4':
        return 'sum4(%s)' % e[1]
    if k == 'shash':
        return 'shash(%s)' % e[1]
    raise ValueError(k)


def rd(e):
    k = e[0]
    if k == 'dk':
        return e[1]
    if k == 'dv':
        return '(double)(%s & 0xffffu)' % rx(e[1])
    if k == 'dbin':
        return '(%s %s %s)' % (rd(e[2]), e[1], rd(e[3]))
    if k == 'ddiv':
        return '(%s / ((double)(%s & 0xffu) + 1.0))' % (rd(e[1]), rx(e[2]))
    if k == 'dflt':
        return '(double)(float)%s' % rd(e[1])
    if k == 'dsel':
        return '(%s %s %s ? %s : %s)' % (rd(e[2]), e[1], rd(e[3]), rd(e[4]), rd(e[5]))
    # an unsigned expression the reducer put in a double's place
    return '(double)%s' % rx(e)


FIELD_T = {'a': 'unsigned', 'b': 'unsigned short', 'c': 'unsigned char', 'd': 'unsigned long long'}
GLOBALS = ['g0', 'g1', 'g2', 'g3']
PRELUDE = '''struct S { unsigned a; unsigned short b; unsigned char c; unsigned long long d; };
static unsigned g0 = 11u, g1 = 0x9e3779b9u, g2, g3 = 7u;
static unsigned ga[8] = { 3u, 1u, 4u, 1u, 5u, 9u, 2u, 6u };
static unsigned fcv(double d) {
    if (d != d) return 1u;
    if (d > 1e15) return 2u;
    if (d < -1e15) return 3u;
    return (unsigned)(long long)d;
}
static void bump(unsigned *q, unsigned k) { q[0] += k; q[0] ^= k >> 3; }
static unsigned sum4(const unsigned *q) { return q[0] + q[1] * 3u + q[2] * 5u + q[3] * 7u; }
static struct S smix(struct S x, unsigned k) {
    x.a += k; x.b ^= (unsigned short)k; x.c += 1u; x.d = x.d * 3u + k;
    return x;
}
static unsigned shash(struct S x) { return x.a ^ x.b ^ ((unsigned)x.c << 8) ^ (unsigned)(x.d >> 7); }
'''


def rlv(lv, types):
    if lv[0] == 'idx':
        return '%s[(%s) & %du]' % (lv[1], rx(lv[3]), lv[2] - 1)
    if lv[0] == 'deref':
        return '*%s' % lv[1]
    if lv[0] == 'fld':
        return '%s.%s' % (lv[1], lv[2])
    return lv[1]


def lv_type(lv, types):
    if lv[0] == 'v':
        return types.get(lv[1], 'unsigned')
    if lv[0] == 'fld':
        return FIELD_T[lv[2]]
    if lv[0] == 'idx' and lv[1] in types:
        return types[lv[1]]
    return 'unsigned'


def rtarget(t):
    if t[0] == 'addr':
        return '&%s' % t[1]
    if t[0] == 'addre':
        return '&%s[(%s) & %du]' % (t[1], rx(t[3]), t[2] - 1)
    return '&%s.a' % t[1]


def rs(s, ind, out, types):
    p = '    ' * ind
    k = s[0]
    if k == 'asg':
        lv = rlv(s[2], types)
        t = lv_type(s[2], types)
        if t == 'int' and s[1] != '=':
            # compound arithmetic on an int could overflow: do it unsigned
            out.append('%s%s = (int)((unsigned)%s %s %s);' % (p, lv, lv, s[1][0], rx(s[3])))
        else:
            out.append('%s%s %s (%s)%s;' % (p, lv, s[1], t, rx(s[3])))
    elif k == 'if':
        out.append('%sif (%s) {' % (p, rx(s[1])))
        for t in s[2]:
            rs(t, ind + 1, out, types)
        if s[3]:
            out.append('%s} else {' % p)
            for t in s[3]:
                rs(t, ind + 1, out, types)
        out.append('%s}' % p)
    elif k == 'for':
        _, v, lo, hi, step, down, body = s
        if down:
            out.append('%sfor (%s = %d; %s >= %d; %s -= %d) {' % (p, v, hi, v, lo, v, step))
        else:
            out.append('%sfor (%s = %d; %s < %d; %s += %d) {' % (p, v, lo, v, hi, v, step))
        for t in body:
            rs(t, ind + 1, out, types)
        out.append('%s}' % p)
    elif k == 'ret':
        out.append('%sif (%s) return %s;' % (p, rx(s[1]), rx(s[2])))
    elif k == 'switch':
        out.append('%sswitch (%s %% 6u) {' % (p, rx(s[1])))
        for (c, body, brk) in s[2]:
            out.append('%scase %d:' % (p, c))
            for t in body:
                rs(t, ind + 1, out, types)
            if brk:
                out.append('%s    break;' % p)
        out.append('%s}' % p)
    elif k == 'brk':
        out.append('%sif (%s) %s;' % (p, rx(s[2]), s[1]))
    elif k == 'pset':
        out.append('%s%s = %s;' % (p, s[1], rtarget(s[2])))
    elif k == 'bump':
        out.append('%sbump(%s, %s);' % (p, s[1], rx(s[2])))
    elif k == 'scopy':
        out.append('%s%s = %s;' % (p, s[1], s[2]))
    elif k == 'smix':
        out.append('%s%s = smix(%s, %s);' % (p, s[1], s[2], rx(s[3])))
    elif k in ('while', 'do'):
        _, v, lim, cond, body = s
        if k == 'while':
            out.append('%s%s = 0;' % (p, v))
            out.append('%swhile (%s < %d && %s) {' % (p, v, lim, rx(cond)))
            out.append('%s    %s++;' % (p, v))
        else:
            out.append('%s%s = 0;' % (p, v))
            out.append('%sdo {' % p)
            out.append('%s    %s++;' % (p, v))
        for t in body:
            rs(t, ind + 1, out, types)
        if k == 'while':
            out.append('%s}' % p)
        else:
            out.append('%s} while (%s < %d && %s);' % (p, v, lim, rx(cond)))
    elif k == 'goto':
        out.append('%sif (%s) goto done;' % (p, rx(s[1])))
    else:
        raise ValueError(k)


def render(fns, calls):
    out = ['#include <stdio.h>', '', PRELUDE]
    for f in fns:
        out.append('static unsigned %s(%s);' % (f.name, ', '.join('unsigned p%d' % i for i in range(f.nparams))))
    out.append('')
    for f in fns:
        types = {n: t for (n, t, _) in f.locals}
        out.append('static unsigned %s(%s) {' % (f.name, ', '.join('unsigned p%d' % i for i in range(f.nparams))))
        # parameters are read before any local is set: initializers may use them
        for (n, t, _) in f.locals:
            out.append('    %s %s = 0;' % (t, n))
        for (n, t, sz) in f.arrays:
            out.append('    %s %s[%d];' % (t, n, sz))
        for n in f.structs:
            out.append('    struct S %s;' % n)
        for n in f.ptrs:
            out.append('    unsigned *%s = &g%d;' % (n, int(n[1:]) % 3))
        for (n, t, sz) in f.arrays:
            out.append('    { int z; for (z = 0; z < %d; z++) %s[z] = (%s)(z * 37u + %du); }' % (sz, n, t, sz))
        for i, n in enumerate(f.structs):
            out.append('    %s.a = %du; %s.b = %du; %s.c = %du; %s.d = %dull;' %
                       (n, 1000 + i, n, 77 + i, n, 5 + i, n, 0x123456789 * (i + 1)))
        for (n, t, init) in f.locals:
            if not n.startswith('i'):
                out.append('    %s = (%s)%s;' % (n, t, rx(init) if init else '0u'))
        for s in f.body:
            rs(s, 1, out, types)
        out.append('done:')
        out.append('    return %s;' % rx(f.ret) if f.ret else '    return 0u;')
        out.append('}')
        out.append('')
    out.append('int main(void) {')
    for (name, args) in calls:
        out.append('    printf("%%s %%u\\n", "%s", %s(%s));' % (name, name, ', '.join('%uu' % a for a in args)))
    out.append('    printf("g %u %u %u %u %u\\n", g0, g1, g2, g3, sum4(ga) ^ sum4(ga + 4));')
    out.append('    return 0;')
    out.append('}')
    return '\n'.join(out) + '\n'


# Calls between generated functions can recurse; keep it finite by only
# calling lower-numbered functions (the generator saw only earlier ones
# when building each function, except through `self.fns` being complete
# for later functions -- forbid upward calls here).
def no_upward_calls(fns):
    order = {f.name: i for i, f in enumerate(fns)}

    def fix(e, me):
        if isinstance(e, tuple):
            if e[0] == 'call' and order.get(e[1], 0) >= order[me]:
                return ('k', 7)
            return tuple(fix(x, me) if isinstance(x, (tuple, list)) else x for x in e)
        if isinstance(e, list):
            return [fix(x, me) for x in e]
        return e
    for f in fns:
        f.locals = [(n, t, fix(i, f.name) if i else i) for (n, t, i) in f.locals]
        f.body = fix(f.body, f.name)
        f.ret = fix(f.ret, f.name)


# Generated functions write the globals and ga.  C leaves the order of a
# call's side effects and the other reads in its full expression
# indeterminate (C11 6.5.2.2p10), so `g2 |= f1()` with f1 bumping g2 has
# two right answers (clang read g2 after the call, stage08 before: seed
# 1011 of the widened generator).  A full expression may therefore hold
# one call, and then touch no memory a callee can change -- the globals,
# ga, anything through a pointer, sum4 of ga; otherwise its calls become
# constants.
def touches_shared(e):
    if isinstance(e, tuple) and e:
        k = e[0]
        if k == 'v' and e[1] in GLOBALS:
            return True
        if k == 'idx' and e[1] == 'ga':
            return True
        if k in ('deref', 'pset'):
            return True
        if k == 'sum4' and e[1] == 'ga':
            return True
    if isinstance(e, (tuple, list)):
        return any(touches_shared(x) for x in e if isinstance(x, (tuple, list)))
    return False


def count_calls(e):
    n = 0
    if isinstance(e, tuple) and e and e[0] == 'call':
        n = 1
    if isinstance(e, (tuple, list)):
        n += sum(count_calls(x) for x in e if isinstance(x, (tuple, list)))
    return n


def drop_calls(e):
    if isinstance(e, tuple) and e and e[0] == 'call':
        return ('k', 7)
    if isinstance(e, tuple):
        return tuple(drop_calls(x) if isinstance(x, (tuple, list)) else x for x in e)
    if isinstance(e, list):
        return [drop_calls(x) for x in e]
    return e


def full_expr(*parts):
    """parts of one full expression, rewritten so its order is fixed"""
    n = sum(count_calls(x) for x in parts)
    if n > 1 or (n == 1 and any(touches_shared(x) for x in parts)):
        return tuple(drop_calls(x) for x in parts)
    return parts


def sequence(stmts):
    out = []
    for s in stmts:
        k = s[0]
        if k == 'asg':
            lv, e = full_expr(s[2], s[3])
            s = ('asg', s[1], lv, e)
        elif k == 'if':
            (c,) = full_expr(s[1])
            s = ('if', c, sequence(s[2]), sequence(s[3]) if s[3] else s[3])
        elif k == 'for':
            s = s[:6] + (sequence(s[6]),)
        elif k == 'ret':
            (c,) = full_expr(s[1])
            (e,) = full_expr(s[2])
            s = ('ret', c, e)
        elif k == 'switch':
            (c,) = full_expr(s[1])
            s = ('switch', c, [(cv, sequence(b), brk) for (cv, b, brk) in s[2]])
        elif k in ('brk',):
            (c,) = full_expr(s[2])
            s = ('brk', s[1], c)
        elif k in ('while', 'do'):
            (c,) = full_expr(s[3])
            s = (k, s[1], s[2], c, sequence(s[4]))
        elif k == 'goto':
            (c,) = full_expr(s[1])
            s = ('goto', c)
        elif k == 'pset':
            (t,) = full_expr(s[2])
            s = ('pset', s[1], t)
        elif k == 'bump':
            (e,) = full_expr(('deref', s[1]), s[2])[1:] or (s[2],)
            s = ('bump', s[1], e)
        elif k == 'smix':
            (e,) = full_expr(s[3])
            s = ('smix', s[1], s[2], e)
        out.append(s)
    return out


def make(seed):
    g = Gen(seed)
    fns, calls = g.program()
    no_upward_calls(fns)
    for f in fns:
        f.locals = [(n, t, full_expr(i)[0] if i else i) for (n, t, i) in f.locals]
        f.body = sequence(f.body)
        (f.ret,) = full_expr(f.ret) if f.ret else (f.ret,)
    return fns, calls


# ---------------------------------------------------------------- build/run
def sh(cmd, cwd=None, timeout=120):
    try:
        p = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout)
        return p.returncode, p.stdout.decode('latin-1')
    except subprocess.TimeoutExpired:
        return 124, 'timeout'


STATS = ('Starting execution', 'Instructions executed', 'Simulated cycles', 'Wall time', 'Performance',
         'HALT', 'MMIO', 'Program', 'instructions/second', 'Cycles', 'Exit code')


def run_out(exe, d):
    rc, out = sh([EMU, exe], cwd=d, timeout=60)
    lines = [l for l in out.splitlines() if l.strip() and not any(l.strip().startswith(s) or s in l for s in STATS)]
    return rc, '\n'.join(lines)


def build_clang(src, d):
    rt = os.path.join(ROOT, 'runtime')
    steps = [[os.path.join(LLVM, 'clang'), '-target', 'slow32-unknown-none', '-S', '-emit-llvm', '-O2', '-w',
              '-I' + os.path.join(rt, 'include'), src, '-o', d + '/c.ll'],
             [os.path.join(LLVM, 'llc'), '-mtriple=slow32-unknown-none', d + '/c.ll', '-o', d + '/c.s'],
             [AS, d + '/c.s', d + '/c.s32o'],
             [LD, '-o', d + '/c.s32x', '--mmio', '64K', rt + '/crt0.s32o', d + '/c.s32o',
              rt + '/libc_mmio.s32a', rt + '/libs32.s32a']]
    for st in steps:
        rc, out = sh(st)
        if rc != 0:
            return None, out
    return d + '/c.s32x', ''


def build_s08(src, d, cc):
    sh8 = os.path.join(KIT, 'selfhost')
    rc, out = sh([DBT, cc, '-I' + os.path.join(sh8, 'include'), src, d + '/s.s'], timeout=120)
    if rc != 0 or not os.path.exists(d + '/s.s') or os.path.getsize(d + '/s.s') == 0:
        return None, 'cc: rc=%d %s' % (rc, out[-400:])
    for st in [[AS, d + '/s.s', d + '/s.s32o'],
               [LD, '-o', d + '/s.s32x', '--mmio', '64K', sh8 + '/crt0.s32o', d + '/s.s32o', sh8 + '/libc.s32a']]:
        rc, out = sh(st)
        if rc != 0:
            return None, out[-400:]
    return d + '/s.s32x', ''


def check(text, cc):
    """('ok'|'skip'|'differ'|'s08-build'|'s08-run', detail)"""
    d = tempfile.mkdtemp(prefix='cfuzz.')
    try:
        src = d + '/p.c'
        open(src, 'w').write(text)
        ce, why = build_clang(src, d)
        if not ce:
            return 'skip', 'clang: ' + why[-300:]
        crc, cout = run_out(ce, d)
        if crc == 124:
            return 'skip', 'clang build loops'
        se, why = build_s08(src, d, cc)
        if not se:
            return 's08-build', why
        src_, sout = run_out(se, d)
        if src_ == 124:
            return 's08-run', 'timeout'
        if sout != cout:
            # a third opinion: the host's compiler, run natively.  When
            # stage08 agrees with it, the SLOW-32 clang build is the odd
            # one out (the LLVM backend, not stage08).
            hrc, hout = sh(['cc', '-O0', '-w', src, '-o', d + '/h.exe'])
            if hrc == 0:
                hrc, hout = sh([d + '/h.exe'], timeout=60)
                hout = '\n'.join(l for l in hout.splitlines() if l.strip())
                if hout == sout:
                    return 'clang-differs', '--- clang\n%s\n--- stage08 = host\n%s' % (cout[:600], sout[:600])
            return 'differ', '--- clang\n%s\n--- stage08\n%s' % (cout[:600], sout[:600])
        return 'ok', ''
    finally:
        shutil.rmtree(d, ignore_errors=True)


# ---------------------------------------------------------------- reduce
def candidates(fns, calls):
    """Yield smaller variants of the program, one change each."""
    # drop a function nothing calls, and locals and arrays nothing names
    called = {c[0] for c in calls}
    text = render(fns, calls)
    for fi, f in enumerate(fns):
        if f.name not in called and text.count(f.name + '(') <= 2:
            yield fns[:fi] + fns[fi + 1:], calls
    for fi, f in enumerate(fns):
        used = repr((f.body, f.ret, [i for (_, _, i) in f.locals]))
        for li, (n, t, init) in enumerate(f.locals):
            if ("'%s'" % n) not in used and ('(unsigned)%s' % n) not in used:
                yield with_fn(fns, fi, locals=f.locals[:li] + f.locals[li + 1:]), calls
        for ai, a in enumerate(f.arrays):
            if ("'%s'" % a[0]) not in used:
                yield with_fn(fns, fi, arrays=f.arrays[:ai] + f.arrays[ai + 1:]), calls
    # drop a call from main
    for i in range(len(calls)):
        if len(calls) > 1:
            yield fns, calls[:i] + calls[i + 1:]
    # drop statements anywhere (and replace compound statements by their bodies)
    for fi, f in enumerate(fns):
        for path, lst in stmt_lists(f.body):
            for j in range(len(lst)):
                s = lst[j]
                repl = [[]]
                if s[0] == 'if':
                    repl += [s[2], s[3] or []]
                if s[0] == 'for':
                    repl.append(s[6])
                if s[0] in ('while', 'do'):
                    repl.append(s[4])
                for r in repl:
                    nb = replace_list(f.body, path, lst[:j] + r + lst[j + 1:])
                    yield with_fn(fns, fi, body=nb), calls
    # simplify expressions: each expression node -> a constant or a child
    for fi, f in enumerate(fns):
        for alt in shrink_tree(f.ret):
            yield with_fn(fns, fi, ret=alt), calls
        for alt in shrink_tree(f.body):
            yield with_fn(fns, fi, body=alt), calls
        for li, (n, t, init) in enumerate(f.locals):
            if init and init[0] != 'k':
                nl = list(f.locals)
                nl[li] = (n, t, ('k', 0))
                yield with_fn(fns, fi, locals=nl), calls
                for alt in shrink_tree(init):
                    nl = list(f.locals)
                    nl[li] = (n, t, alt)
                    yield with_fn(fns, fi, locals=nl), calls


def with_fn(fns, fi, **kw):
    out = []
    for i, f in enumerate(fns):
        if i == fi:
            g = Fn(f.name, f.nparams)
            g.locals, g.arrays, g.body, g.ret = f.locals, f.arrays, f.body, f.ret
            g.ptrs, g.structs = f.ptrs, f.structs
            for k, v in kw.items():
                setattr(g, k, v)
            out.append(g)
        else:
            out.append(f)
    return out


def stmt_lists(body, path=()):
    yield path, body
    for j, s in enumerate(body):
        if s[0] == 'if':
            yield from stmt_lists(s[2], path + (j, 2))
            if s[3]:
                yield from stmt_lists(s[3], path + (j, 3))
        elif s[0] == 'for':
            yield from stmt_lists(s[6], path + (j, 6))
        elif s[0] in ('while', 'do'):
            yield from stmt_lists(s[4], path + (j, 4))
        elif s[0] == 'switch':
            for ci, (c, b, brk) in enumerate(s[2]):
                yield from stmt_lists(b, path + (j, 2, ci))


def replace_list(body, path, new):
    if not path:
        return new
    j, field = path[0], path[1]
    s = list(body[j])
    if s[0] == 'switch':
        ci = path[2]
        cases = list(s[2])
        c, b, brk = cases[ci]
        cases[ci] = (c, replace_list(b, path[3:], new), brk)
        s[2] = cases
    else:
        s[field] = replace_list(s[field], path[2:], new)
    nb = list(body)
    nb[j] = tuple(s)
    return nb


EXPR_KINDS = {'bin', 'shl', 'shr', 'sar', 'cmp', 'sel', 'cast', 'div', 'mulhi', 'un', 'call', 'addk', 'idx',
              'w64', 'sgn', 'flt', 'sum4', 'shash', 'deref', 'fld'}


def shrink_tree(t):
    """Every variant of t with one expression node replaced by 0, 1 or a child."""
    if isinstance(t, tuple) and t and t[0] in EXPR_KINDS:
        yield ('k', 0)
        yield ('k', 1)
        for x in t[1:]:
            if isinstance(x, tuple) and x and (x[0] in EXPR_KINDS or x[0] in ('k', 'v')):
                yield x
    if isinstance(t, (tuple, list)):
        for i, x in enumerate(t):
            if isinstance(x, (tuple, list)):
                for alt in shrink_tree(x):
                    if isinstance(t, tuple):
                        yield t[:i] + (alt,) + t[i + 1:]
                    else:
                        yield t[:i] + [alt] + t[i + 1:]


def reduce(seed, cc, out_path):
    fns, calls = make(seed)
    kind, _ = check(render(fns, calls), cc)
    if kind in ('ok', 'skip'):
        print('seed %d does not fail (%s)' % (seed, kind))
        return 1
    print('reducing seed %d (%s), %d lines' % (seed, kind, render(fns, calls).count('\n')))
    progress = True
    while progress:
        progress = False
        size = len(render(fns, calls))
        for nf, nc in candidates(fns, calls):
            t = render(nf, nc)
            if len(t) >= size:
                continue
            k, _ = check(t, cc)
            if k == kind:
                fns, calls = nf, nc
                progress = True
                print('  %d lines' % render(fns, calls).count('\n'), flush=True)
                break
    text = render(fns, calls)
    open(out_path, 'w').write(text)
    print('wrote %s (%d lines)' % (out_path, text.count('\n')))
    print(check(text, cc)[1])
    return 0


# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('cmd', choices=['run', 'show', 'reduce'])
    ap.add_argument('seed', nargs='?', type=int)
    ap.add_argument('-n', type=int, default=200)
    ap.add_argument('-s', type=int, default=1)
    ap.add_argument('-j', type=int, default=os.cpu_count() or 4)
    ap.add_argument('--cc', default=CC08)
    ap.add_argument('--keep', default=os.path.join(ROOT, 'regression/results-fuzz'))
    ap.add_argument('-o', default=None)
    ap.add_argument('--profile', choices=['wide', 'narrow'], default='wide',
                    help='narrow: the first generator\'s scalar subset, where the shapes it found stay dense')
    a = ap.parse_args()
    global PROFILE
    PROFILE = a.profile
    if a.cmd == 'show':
        sys.stdout.write(render(*make(a.seed)))
        return 0
    if a.cmd == 'reduce':
        return reduce(a.seed, a.cc, a.o or 'cfuzz-%d.c' % a.seed)
    os.makedirs(a.keep, exist_ok=True)
    seeds = range(a.s, a.s + a.n)
    tally = {}
    bad = []

    def one(seed):
        return seed, check(render(*make(seed)), a.cc)
    with ThreadPoolExecutor(a.j) as ex:
        for seed, (kind, detail) in ex.map(one, seeds):
            tally[kind] = tally.get(kind, 0) + 1
            if kind not in ('ok', 'skip'):
                bad.append(seed)
                open(os.path.join(a.keep, 'seed-%d.c' % seed), 'w').write(render(*make(seed)))
                open(os.path.join(a.keep, 'seed-%d.txt' % seed), 'w').write(kind + '\n' + detail + '\n')
                print('seed %d: %s' % (seed, kind), flush=True)
    print('cfuzz: %d programs: %s' % (len(seeds), ', '.join('%s %d' % kv for kv in sorted(tally.items()))))
    if tally.get('skip'):
        print('  (skip = clang could not build or run it: a generator problem, not a finding)')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
