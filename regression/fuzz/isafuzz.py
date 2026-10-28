#!/usr/bin/env python3
"""isafuzz: random SLOW-32 machine code, run on every engine, compared.

cfuzz (beside this) tests the compilers; this tests the ENGINES, below
any compiler.  Everything that reaches slow32-fast, the DBT and qemu was
written by clang, stage08, f77 or s32-cobc, and compilers have habits:
whole corners of the ISA never execute.  The two engine bugs that hid
longest sat in exactly such corners -- DBT-15 translated "bge zero, rX"
backwards, and qemu's fused compare-branch clobbered its own operand on
"seq r3, r3, r0" -- each behind a pattern only one compiler emitted.

Each program is generated as a small tree, rendered to assembly, built
with slow32asm and s32-ld (no crt0: main is the entry at address 0), and
run on the reference interpreter and every engine configuration.  At the
end it stores all 32 registers into its buffer and prints the buffer in
hex between markers with DEBUG; the engines must print the same bytes and
exit the same way.  docs/SPEC.md decides who is right.

Defined by construction (SPEC.md's Unspecified cases are avoided):
- memory goes only to a scratch buffer: (rX & 255) + base, offset -16..15,
  or base + 0..300 directly; unaligned is permitted and used;
- every floating-point result that could be a NaN is canonicalized at
  once (feq x,x; mask), since a NaN's bit pattern is unspecified;
- float-to-integer conversions take an operand loaded just before from a
  list of values that fit the destination;
- doubles use even pairs below 31, never (r0, r1);
- branches go forward, loops count down in r27 and do not nest, and every
  JALR target is a label.
Reserved: r26 (scratch for addresses, canonicalization, conversion
operands), r27 (loop counter), r28 (buffer base).

  ./isafuzz.py run -n 2000            # seeds 1..2000
  ./isafuzz.py show SEED              # the assembly
  ./isafuzz.py reduce SEED [-o f.s]   # shrink a divergence
"""
import argparse
import os
import random
import shutil
import struct
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
AS = os.path.join(ROOT, 'tools/assembler/slow32asm')
LD = os.path.join(ROOT, 'tools/linker/s32-ld')
REF = os.path.join(ROOT, 'tools/emulator/slow32')
FAST = os.path.join(ROOT, 'tools/emulator/slow32-fast')
DBT = os.path.join(ROOT, 'tools/dbt/slow32-dbt')
QEMU = os.environ.get('QEMU_S32', os.path.expanduser('~/qemu/build/qemu-system-slow32'))

# name -> argv prefix (the program path is appended; qemu's comes after -kernel)
ENGINES = {
    'fast': [FAST],
    'dbt': [DBT],
    'dbt-1': [DBT, '-1'],
    'dbt-2': [DBT, '-2'],
    'dbt-3': [DBT, '-3'],
    'dbt-noP': [DBT, '-P'],
    'dbt-noS': [DBT, '-S'],
    'dbt-noR': [DBT, '-R'],
    'qemu': [QEMU, '-machine', 'slow32-tcg', '-nographic', '-monitor', 'none', '-kernel'],
}

POOL = list(range(1, 26)) + [29, 30, 31]          # registers the program may write
PAIRS = [n for n in range(2, 31, 2) if n in POOL and n + 1 in POOL]
BUF = 512          # bytes; the dump of the 32 registers sits at 384
DUMP = 384

R_OPS = ['add', 'sub', 'xor', 'or', 'and', 'sll', 'srl', 'sra', 'slt', 'sltu', 'mul', 'mulh',
         'mulhu', 'div', 'rem', 'seq', 'sne', 'sgt', 'sgtu', 'sle', 'sleu', 'sge', 'sgeu']
I_SIGNED = ['addi', 'slti']
I_UNSIGNED = ['ori', 'andi', 'xori', 'sltiu']
I_SHIFT = ['slli', 'srli', 'srai']
LOADS = ['ldb', 'ldh', 'ldw', 'ldbu', 'ldhu']
STORES = ['stb', 'sth', 'stw']
BRANCHES = ['beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu']
F_S_ARITH = ['fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s']
F_D_ARITH = ['fadd.d', 'fsub.d', 'fmul.d', 'fdiv.d']
F_S_CMP = ['feq.s', 'flt.s', 'fle.s']
F_D_CMP = ['feq.d', 'flt.d', 'fle.d']

INTERESTING = [0, 1, 2, 3, 0xffffffff, 0xfffffffe, 0x80000000, 0x7fffffff, 0x80000001, 31, 32, 33,
               0xff, 0x100, 0x7ff, 0x800, 0xfff, 0x1000, 0xffff, 0x10000, 0xffff8000,
               0x3f800000, 0xbf800000, 0x7f800000, 0xff800000, 0x7fc00000, 0x00000001, 0x00800000,
               0x40490fdb, 0x3ff00000, 0x400921fb, 0x54442d18, 0x7ff00000, 0xc0000000]


def f32(v):
    return struct.unpack('<I', struct.pack('<f', v))[0]


def f64(v):
    lo, hi = struct.unpack('<II', struct.pack('<d', v))
    return lo, hi


# float-to-integer operands that fit their destination (the rest is Unspecified)
SAFE = {
    'w': [0.0, -0.0, 0.5, -0.75, 1.0, 3.99, -7.5, 1e6, -123456.7, 2147483520.0, -2147483648.0, 1e-30],
    'wu': [0.0, 0.5, 1.0, 3.99, 1e6, 2147483648.0, 4294967040.0, 1e-30],
    'l': [0.0, -1.5, 3.99, 1e6, 4294967296.5, -9.2e18, 9.2e18, 4.6e18, 1e-30],
    'lu': [0.0, 0.5, 3.99, 4294967296.5, 9.3e18, 1.8e19, 1e-30],
}


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)
        self.nlabel = 0

    def label(self):
        self.nlabel += 1
        return '.L%d' % self.nlabel

    def reg(self, write=False):
        r = self.r
        if r.random() < (0.06 if write else 0.12):
            return 0
        if r.random() < 0.15:
            return r.choice([1, 2, 3, 29, 30, 31])
        return r.choice(POOL)

    def pair(self):
        return self.r.choice(PAIRS)

    def value(self):
        r = self.r
        c = r.random()
        if c < 0.45:
            return r.choice(INTERESTING)
        if c < 0.6:
            return r.randrange(-2048, 2048) & 0xffffffff
        return r.randrange(1 << 32)

    def insn(self):
        """One leaf: a tuple ('i', text...) or a short canned sequence."""
        r = self.r
        c = r.random()
        if c < 0.30:
            return ('i', '%s r%d, r%d, r%d' % (r.choice(R_OPS), self.reg(True), self.reg(), self.reg()))
        if c < 0.40:
            op = r.choice(I_SIGNED)
            return ('i', '%s r%d, r%d, %d' % (op, self.reg(True), self.reg(),
                                               r.choice([0, 1, -1, 2047, -2048, r.randrange(-2048, 2048)])))
        if c < 0.48:
            op = r.choice(I_UNSIGNED)
            return ('i', '%s r%d, r%d, %d' % (op, self.reg(True), self.reg(),
                                               r.choice([0, 1, 0x7ff, 0x800, 0xfff, r.randrange(4096)])))
        if c < 0.54:
            # the shift amount is imm & 31, and the immediate is 12 bits signed
            return ('i', '%s r%d, r%d, %d' % (r.choice(I_SHIFT), self.reg(True), self.reg(),
                                               r.choice([0, 1, 31, 32, 33, -1, r.randrange(-2048, 2048)])))
        if c < 0.57:
            return ('i', 'lui r%d, %d' % (self.reg(True), r.randrange(1 << 20)))
        if c < 0.70:
            return self.mem()
        if c < 0.84:
            return self.fp()
        if c < 0.86:
            return ('i', r.choice(['nop', 'yield r0']))
        if c < 0.87:
            return ('i', 'debug r%d' % self.reg())
        return ('i', '%s r%d, r%d, r%d' % (r.choice(R_OPS), self.reg(True), self.reg(), self.reg()))

    def mem(self):
        r = self.r
        if r.random() < 0.3:
            # base + 0..300 directly
            off = r.randrange(0, 300)
            if r.random() < 0.5:
                return ('i', '%s r%d, r28+%d' % (r.choice(LOADS), self.reg(True), off))
            return ('i', '%s r28+%d, r%d' % (r.choice(STORES), off, self.reg()))
        # (rX & 255) + base, then offset -16..15 on top of 16
        off = 16 + r.randrange(-16, 16)
        seq = ['andi r26, r%d, 255' % self.reg(), 'add r26, r26, r28']
        if r.random() < 0.5:
            seq.append('%s r%d, r26+%d' % (r.choice(LOADS), self.reg(True), off))
        else:
            seq.append('%s r26+%d, r%d' % (r.choice(STORES), off, self.reg()))
        return ('seq', seq)

    def canon_s(self, rd):
        if rd == 0:
            return []
        return ['feq.s r26, r%d, r%d' % (rd, rd), 'sub r26, r0, r26', 'and r%d, r%d, r26' % (rd, rd)]

    def canon_d(self, rd):
        return ['feq.d r26, r%d, r%d' % (rd, rd), 'sub r26, r0, r26',
                'and r%d, r%d, r26' % (rd, rd), 'and r%d, r%d, r26' % (rd + 1, rd + 1)]

    def li(self, rd, v):
        return ['lui r%d, %d' % (rd, v >> 12), 'ori r%d, r%d, %d' % (rd, rd, v & 0xfff)]

    def fp(self):
        r = self.r
        c = r.random()
        if c < 0.22:
            rd = self.reg(True)
            return ('seq', ['%s r%d, r%d, r%d' % (r.choice(F_S_ARITH), rd, self.reg(), self.reg())] + self.canon_s(rd))
        if c < 0.40:
            rd = self.pair()
            return ('seq', ['%s r%d, r%d, r%d' % (r.choice(F_D_ARITH), rd, self.pair(), self.pair())] + self.canon_d(rd))
        if c < 0.46:
            rd = self.reg(True)
            return ('seq', ['fsqrt.s r%d, r%d' % (rd, self.reg())] + self.canon_s(rd))
        if c < 0.50:
            rd = self.pair()
            return ('seq', ['fsqrt.d r%d, r%d' % (rd, self.pair())] + self.canon_d(rd))
        if c < 0.58:
            return ('i', '%s r%d, r%d, r%d' % (r.choice(F_S_CMP), self.reg(True), self.reg(), self.reg()))
        if c < 0.66:
            return ('i', '%s r%d, r%d, r%d' % (r.choice(F_D_CMP), self.reg(True), self.pair(), self.pair()))
        if c < 0.72:
            # bit operations: defined on any pattern, NaNs included
            if r.random() < 0.5:
                return ('i', '%s r%d, r%d' % (r.choice(['fneg.s', 'fabs.s']), self.reg(True), self.reg()))
            return ('i', '%s r%d, r%d' % (r.choice(['fneg.d', 'fabs.d']), self.pair(), self.pair()))
        if c < 0.80:
            # integer to float: defined for every integer
            if r.random() < 0.5:
                return ('i', '%s r%d, r%d' % (r.choice(['fcvt.s.w', 'fcvt.s.wu']), self.reg(True), self.reg()))
            if r.random() < 0.5:
                return ('i', '%s r%d, r%d' % (r.choice(['fcvt.d.w', 'fcvt.d.wu']), self.pair(), self.reg()))
            if r.random() < 0.5:
                return ('i', '%s r%d, r%d' % (r.choice(['fcvt.s.l', 'fcvt.s.lu']), self.reg(True), self.pair()))
            return ('i', '%s r%d, r%d' % (r.choice(['fcvt.d.l', 'fcvt.d.lu']), self.pair(), self.pair()))
        if c < 0.86:
            # float <-> double: canonicalize (a NaN stays a NaN)
            if r.random() < 0.5:
                rd = self.pair()
                return ('seq', ['fcvt.d.s r%d, r%d' % (rd, self.reg())] + self.canon_d(rd))
            rd = self.reg(True)
            return ('seq', ['fcvt.s.d r%d, r%d' % (rd, self.pair())] + self.canon_s(rd))
        # float to integer: a safe operand loaded just before
        kind = r.choice(['w', 'wu', 'l', 'lu'])
        v = r.choice(SAFE[kind])
        if r.random() < 0.5:
            src = f32(v)
            op = {'w': 'fcvt.w.s', 'wu': 'fcvt.wu.s', 'l': 'fcvt.l.s', 'lu': 'fcvt.lu.s'}[kind]
            pre = self.li(26, src)
            srcr = 26
        else:
            lo, hi = f64(v)
            op = {'w': 'fcvt.w.d', 'wu': 'fcvt.wu.d', 'l': 'fcvt.l.d', 'lu': 'fcvt.lu.d'}[kind]
            srcr = self.pair()
            pre = self.li(srcr, lo) + self.li(srcr + 1, hi)
        rd = self.pair() if kind in ('l', 'lu') else self.reg(True)
        return ('seq', pre + ['%s r%d, r%d' % (op, rd, srcr)])

    def block(self, n, depth, in_loop):
        out = []
        for _ in range(n):
            r = self.r
            c = r.random()
            if depth < 3 and c < 0.10:
                # forward conditional branch over a few statements (none: offset 0)
                out.append(('br', r.choice(BRANCHES), self.reg(), self.reg(), self.label(),
                            self.block(r.choice([0, 1, 2, 3, 5]), depth + 1, in_loop)))
            elif depth < 3 and not in_loop and c < 0.15:
                out.append(('loop', r.choice([1, 2, 3, 7, 16, 100, 500]), r.choice(['bne', 'blt', 'bltu']), self.label(),
                            self.block(r.randrange(2, 12), depth + 1, True)))
            elif depth < 3 and c < 0.18:
                out.append(('jal', self.reg(True), self.label(), self.block(r.randrange(0, 3), depth + 1, in_loop)))
            elif depth < 3 and c < 0.21:
                tr = r.choice([x for x in POOL])
                out.append(('jalr', self.reg(True), tr, r.choice([0, 0, 4, -4]), self.label(),
                            self.block(r.randrange(0, 3), depth + 1, in_loop)))
            else:
                out.append(self.insn())
        return out

    def program(self):
        init = {rr: self.value() for rr in POOL}
        body = self.block(self.r.randrange(20, 90), 0, False)
        return init, body


def render_stmts(stmts, out):
    for s in stmts:
        k = s[0]
        if k == 'i':
            out.append('\t' + s[1])
        elif k == 'seq':
            out.extend('\t' + x for x in s[1])
        elif k == 'br':
            _, op, a, b, lab, body = s
            out.append('\t%s r%d, r%d, %s' % (op, a, b, lab))
            render_stmts(body, out)
            out.append(lab + ':')
        elif k == 'loop':
            _, n, op, lab, body = s
            out.append('\taddi r27, r0, %d' % n)
            out.append(lab + ':')
            render_stmts(body, out)
            out.append('\taddi r27, r27, -1')
            if op == 'bne':
                out.append('\tbne r27, r0, %s' % lab)
            elif op == 'blt':
                out.append('\tblt r0, r27, %s' % lab)
            else:
                out.append('\tbltu r0, r27, %s' % lab)
        elif k == 'jal':
            _, rd, lab, body = s
            out.append('\tjal r%d, %s' % (rd, lab))
            render_stmts(body, out)
            out.append(lab + ':')
        elif k == 'jalr':
            # the target is the label; imm moves the base by -imm so they cancel
            _, rd, tr, imm, lab, body = s
            out.append('\tlui r%d, %%hi(%s)' % (tr, lab))
            out.append('\taddi r%d, r%d, %%lo(%s)' % (tr, tr, lab))
            if imm:
                out.append('\taddi r%d, r%d, %d' % (tr, tr, -imm))
            out.append('\tjalr r%d, r%d, %d' % (rd, tr, imm))
            render_stmts(body, out)
            out.append(lab + ':')


EPILOGUE = '''
	# the dump: every register into the buffer, then the buffer in hex
{stores}
	addi r2, r28, 0
	addi r3, r28, {buf}
	addi r8, r0, 39
.Ldw:
	ldw r4, r2+0
	addi r5, r0, 8
.Ldn:
	srli r6, r4, 28
	slli r4, r4, 4
	sltiu r7, r6, 10
	xori r7, r7, 1
	mul r7, r7, r8
	add r6, r6, r7
	addi r6, r6, 48
	debug r6
	addi r5, r5, -1
	bne r5, r0, .Ldn
	addi r6, r0, 10
	debug r6
	addi r2, r2, 4
	bne r2, r3, .Ldw
{endmark}
	addi r1, r0, 0
	halt
	.data
buf:
	.space {buf}
'''


def marker(text):
    out = []
    for ch in text:
        out.append('\taddi r26, r0, %d' % ord(ch))
        out.append('\tdebug r26')
    return out


def render(init, body):
    out = ['\t.text', '\t.globl main', 'main:', '\tlui r28, %hi(buf)', '\taddi r28, r28, %lo(buf)']
    out += marker('#P\n')
    for rr in sorted(init):
        v = init[rr]
        out.append('\tlui r%d, %d' % (rr, v >> 12))
        out.append('\tori r%d, r%d, %d' % (rr, rr, v & 0xfff))
    out.append('\taddi r27, r0, 0')
    render_stmts(body, out)
    out += marker('\n#S\n')
    stores = '\n'.join('\tstw r28+%d, r%d' % (DUMP + 4 * i, i) for i in range(32))
    return '\n'.join(out) + EPILOGUE.format(stores=stores, buf=BUF, endmark='\n'.join(marker('#E\n'))) + '\n'


def make(seed):
    return Gen(seed).program()


# ---------------------------------------------------------------- build/run
def sh(cmd, timeout=20):
    try:
        p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout)
        return p.returncode, p.stdout.decode('latin-1')
    except subprocess.TimeoutExpired:
        return 124, 'timeout'


def framed(rc, out):
    """what the program printed (from #P to #E), and how it ended"""
    a = out.find('#P\n')
    b = out.find('#E\n')
    if a < 0:
        return 'rc=%d no-start\n%s' % (rc, out[-400:])
    if b < 0:
        return 'rc=%d no-end\n%s' % (rc, out[a:][-1200:])
    return 'rc=%d\n%s' % (rc, out[a:b + 3])


def build(text, d):
    open(d + '/p.s', 'w').write(text)
    rc, out = sh([AS, d + '/p.s', d + '/p.s32o'])
    if rc != 0:
        return None, 'as: ' + out[-400:]
    rc, out = sh([LD, '-o', d + '/p.s32x', d + '/p.s32o'])
    if rc != 0:
        return None, 'ld: ' + out[-400:]
    return d + '/p.s32x', ''


def check(text, engines):
    """('ok'|'diverge'|'build'|'ref', detail, divergent engine names)"""
    d = tempfile.mkdtemp(prefix='isafuzz.')
    try:
        exe, why = build(text, d)
        if not exe:
            return 'build', why, ()
        ref = framed(*sh([REF, exe]))
        if not ref.startswith('rc=0\n'):
            return 'ref', ref[:600], ()
        bad = []
        detail = []
        for name in engines:
            got = framed(*sh(ENGINES[name] + [exe]))
            if got != ref:
                bad.append(name)
                if len(detail) < 2:
                    detail.append(diff_text(ref, got, name))
        if bad:
            return 'diverge', '\n'.join(detail), tuple(bad)
        return 'ok', '', ()
    finally:
        shutil.rmtree(d, ignore_errors=True)


def diff_text(ref, got, name):
    rl, gl = ref.split('\n'), got.split('\n')
    lines = ['--- ref vs %s' % name]
    for i in range(max(len(rl), len(gl))):
        a = rl[i] if i < len(rl) else '<none>'
        b = gl[i] if i < len(gl) else '<none>'
        if a != b:
            # lines: rc, #P, what DEBUG printed in the body (if anything), #S,
            # then the words; count back from #S
            w = i - (rl.index('#S') + 1 if '#S' in rl else 4)
            what = ('r%d' % (w - DUMP // 4)) if w >= DUMP // 4 else ('buf+%d' % (4 * w))
            lines.append('  line %d (%s): ref %s  %s %s' % (i, what, a[:40], name, b[:40]))
            if len(lines) > 8:
                break
    return '\n'.join(lines)


# ---------------------------------------------------------------- reduce
def candidates(init, body):
    # drop or unwrap a statement anywhere
    def walk(stmts, path):
        for j, s in enumerate(stmts):
            yield path, j, s
            if s[0] in ('br', 'loop', 'jal', 'jalr'):
                for x in walk(s[-1], path + (j,)):
                    yield x

    def replace(stmts, path, j, repl):
        if not path:
            return stmts[:j] + repl + stmts[j + 1:]
        i = path[0]
        s = stmts[i]
        return stmts[:i] + [s[:-1] + (replace(s[-1], path[1:], j, repl),)] + stmts[i + 1:]
    sites = list(walk(body, ()))
    for path, j, s in sites:
        yield init, replace(body, path, j, [])
    for path, j, s in sites:
        if s[0] in ('br', 'loop', 'jal', 'jalr'):
            yield init, replace(body, path, j, list(s[-1]))
        if s[0] == 'seq' and len(s[1]) > 1:
            for k in range(len(s[1])):
                yield init, replace(body, path, j, [('seq', s[1][:k] + s[1][k + 1:])])
    for rr in sorted(init):
        if init[rr]:
            ni = dict(init)
            ni[rr] = 0
            yield ni, body


def reduce(seed, engines, out_path):
    init, body = make(seed)
    kind, detail, bad = check(render(init, body), engines)
    if kind == 'ok':
        print('seed %d does not diverge' % seed)
        return 1
    target = bad[0] if bad else None
    print('reducing seed %d (%s %s), %d lines' % (seed, kind, ','.join(bad), render(init, body).count('\n')))
    progress = True
    while progress:
        progress = False
        size = len(render(init, body))
        for ni, nb in candidates(init, body):
            t = render(ni, nb)
            if len(t) > size:
                continue
            k, _, b = check(t, [target] if target else engines)
            if k == kind and (target is None or target in b):
                if len(t) == size and (ni, nb) == (init, body):
                    continue
                init, body = ni, nb
                progress = True
                print('  %d lines' % render(init, body).count('\n'), flush=True)
                break
    text = render(init, body)
    open(out_path, 'w').write(text)
    print('wrote %s' % out_path)
    print(check(text, engines)[1])
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('cmd', choices=['run', 'show', 'reduce'])
    ap.add_argument('seed', nargs='?', type=int)
    ap.add_argument('-n', type=int, default=500)
    ap.add_argument('-s', type=int, default=1)
    ap.add_argument('-j', type=int, default=os.cpu_count() or 4)
    ap.add_argument('--engines', default=','.join(ENGINES),
                    help='comma-separated, from: ' + ','.join(ENGINES))
    ap.add_argument('--keep', default=os.path.join(ROOT, 'regression/results-fuzz/isa'))
    ap.add_argument('-o', default=None)
    a = ap.parse_args()
    engines = [e for e in a.engines.split(',') if e]
    for e in engines:
        if e not in ENGINES:
            ap.error('unknown engine %s' % e)
    if 'qemu' in engines and not os.path.exists(QEMU):
        engines.remove('qemu')
        print('isafuzz: no qemu at %s, skipping it' % QEMU)
    if a.cmd == 'show':
        sys.stdout.write(render(*make(a.seed)))
        return 0
    if a.cmd == 'reduce':
        # into the results directory (gitignored), not the cwd -- the tree
        os.makedirs(a.keep, exist_ok=True)
        return reduce(a.seed, engines, a.o or os.path.join(a.keep, 'reduced-%d.s' % a.seed))
    os.makedirs(a.keep, exist_ok=True)
    seeds = range(a.s, a.s + a.n)
    tally = {}
    per_engine = {}
    bad = []

    def one(seed):
        return seed, check(render(*make(seed)), engines)
    with ThreadPoolExecutor(a.j) as ex:
        for seed, (kind, detail, engs) in ex.map(one, seeds):
            tally[kind] = tally.get(kind, 0) + 1
            for e in engs:
                per_engine[e] = per_engine.get(e, 0) + 1
            if kind != 'ok':
                bad.append(seed)
                open(os.path.join(a.keep, 'seed-%d.s' % seed), 'w').write(render(*make(seed)))
                open(os.path.join(a.keep, 'seed-%d.txt' % seed), 'w').write(
                    '%s %s\n%s\n' % (kind, ','.join(engs), detail))
                print('seed %d: %s %s' % (seed, kind, ','.join(engs)), flush=True)
    print('isafuzz: %d programs on %s: %s' % (len(seeds), ','.join(engines),
                                             ', '.join('%s %d' % kv for kv in sorted(tally.items()))))
    if per_engine:
        print('  diverging, by engine: ' + ', '.join('%s %d' % kv for kv in sorted(per_engine.items())))
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
