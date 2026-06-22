#!/usr/bin/env python3
"""
gen_corpus.py -- Directed ISA-conformance corpus generator for SLOW-32.

Emits self-checking SLOW-32 assembly programs that exercise instruction
semantics at boundary values and walk the immediate-decode bit fields.

The EXPECTED result of every operation is computed *here*, by an independent
reference model derived from the SLOW-32 ISA and RISC-V M-extension semantics
-- NOT copied from any emulator -- and baked into each program as an
`assert_eq` against an `li`-materialized constant.

Run the resulting .s32x on every emulator. If an engine computes a result that
differs from this reference, its `assert_eq` fires and it halts *before*
printing the success marker (`CHK=XXXXXXXX`). Because the expected value is the
reference's, a bug shared by ALL engines still fails every engine's assert --
which is exactly the correlated-error residual that the bootstrap's
artifact-equality cross-check cannot see.

Each program also folds every result into a rotating checksum (r28) printed at
the end. The harness compares that checksum across engines and against the
reference value computed here, which catches an engine that fails to halt on a
bad assert (it would print a divergent checksum instead).

Output: one `test_<group>.s` per instruction group, plus `expected.txt`
(group -> reference checksum), under the given build dir.

Usage: gen_corpus.py <outdir>
"""

import os
import sys

# ----------------------------------------------------------------------
# Independent reference model (the "different mind").
# Pure 32-bit semantics. No loader, no MMIO, no dispatch -- just the ALU.
# ----------------------------------------------------------------------

M32 = 0xFFFFFFFF


def u32(x):
    return x & M32


def s32(x):
    x &= M32
    return x - (1 << 32) if (x & 0x80000000) else x


def ctrunc_div(x, y):
    """C / RISC-V division: truncate toward zero (Python // floors)."""
    q = abs(x) // abs(y)
    return -q if (x < 0) != (y < 0) else q


def ref_div(a, b):
    # RISC-V M-extension conventions (a legitimate independent spec source):
    if b == 0:
        return 0xFFFFFFFF                       # x / 0  = all ones
    if a == 0x80000000 and b == 0xFFFFFFFF:
        return 0x80000000                       # INT_MIN / -1 overflow
    return u32(ctrunc_div(s32(a), s32(b)))


def ref_rem(a, b):
    if b == 0:
        return u32(a)                           # x % 0  = x
    if a == 0x80000000 and b == 0xFFFFFFFF:
        return 0                                # INT_MIN % -1 overflow
    return u32(s32(a) - ctrunc_div(s32(a), s32(b)) * s32(b))


REF_R = {
    'add':   lambda a, b: u32(a + b),
    'sub':   lambda a, b: u32(a - b),
    'xor':   lambda a, b: a ^ b,
    'or':    lambda a, b: a | b,
    'and':   lambda a, b: a & b,
    'sll':   lambda a, b: u32(a << (b & 31)),
    'srl':   lambda a, b: a >> (b & 31),
    'sra':   lambda a, b: u32(s32(a) >> (b & 31)),
    'slt':   lambda a, b: 1 if s32(a) < s32(b) else 0,
    'sltu':  lambda a, b: 1 if a < b else 0,
    'mul':   lambda a, b: u32(a * b),
    'mulh':  lambda a, b: u32((s32(a) * s32(b)) >> 32),
    'mulhu': lambda a, b: u32((a * b) >> 32),
    'seq':   lambda a, b: 1 if a == b else 0,
    'sne':   lambda a, b: 1 if a != b else 0,
    'sgt':   lambda a, b: 1 if s32(a) > s32(b) else 0,
    'sgtu':  lambda a, b: 1 if a > b else 0,
    'sle':   lambda a, b: 1 if s32(a) <= s32(b) else 0,
    'sleu':  lambda a, b: 1 if a <= b else 0,
    'sge':   lambda a, b: 1 if s32(a) >= s32(b) else 0,
    'sgeu':  lambda a, b: 1 if a >= b else 0,
    'div':   ref_div,
    'rem':   ref_rem,
}


def ref_addi(a, imm):
    return u32(a + imm)               # imm is the sign-extended 12-bit value


def ref_shimm(mn, a, sh):
    if mn == 'slli':
        return u32(a << (sh & 31))
    if mn == 'srli':
        return a >> (sh & 31)
    if mn == 'srai':
        return u32(s32(a) >> (sh & 31))
    raise ValueError(mn)


# ----------------------------------------------------------------------
# Boundary operand alphabets
# ----------------------------------------------------------------------

VALUES = [
    0x00000000, 0x00000001, 0x00000002, 0x7FFFFFFF, 0x80000000,
    0xFFFFFFFF, 0xFFFFFFFE, 0x55555555, 0xAAAAAAAA, 0x0000FFFF, 0xFFFF0000,
]
# R-type shift amount operand; ISA masks to low 5 bits, so 32/33/-1 probe masking.
SHAMTS = [0, 1, 2, 7, 8, 15, 16, 17, 31, 32, 33, 0xFFFFFFFF]
# Shift-immediate amount; assembler requires 0..31.
SHIMM = [0, 1, 2, 7, 8, 15, 16, 31]
# addi immediate: 12-bit signed boundaries.
IMM12 = [0, 1, -1, 2, -2, 5, -5, 1024, -1024, 2046, 2047, -2047, -2048]


# ----------------------------------------------------------------------
# Assembly program builder
# ----------------------------------------------------------------------

class Prog:
    def __init__(self, name):
        self.name = name
        self.lines = []
        self.chk = 0          # mirror of the guest r28 checksum
        self._lc = 0

    def lab(self, base):
        self._lc += 1
        return ".L_%s_%d" % (base, self._lc)

    def ins(self, s):
        self.lines.append("    " + s)

    def at(self, s):
        self.lines.append(s)

    def li(self, reg, val):
        self.ins("li %s, 0x%08X" % (reg, u32(val)))

    def fold(self, result_reg, expected):
        """Fold guest result_reg into r28 (rotate-left-1 XOR); mirror in Python."""
        self.ins("xor r28, r28, %s" % result_reg)
        self.ins("slli r11, r28, 1")
        self.ins("srli r12, r28, 31")
        self.ins("or r28, r11, r12")
        self.chk ^= u32(expected)
        self.chk = u32((self.chk << 1) | (self.chk >> 31))

    def assert_case(self, expected):
        """assert r5 == expected, then fold r5."""
        self.li("r6", expected)
        self.ins("assert_eq r5, r6")
        self.fold("r5", expected)

    # --- case shapes ---
    def case_rr(self, mn, a, b, expected):
        self.li("r3", a)
        self.li("r4", b)
        self.ins("%s r5, r3, r4" % mn)
        self.assert_case(expected)

    def case_addi(self, a, imm, expected):
        self.li("r3", a)
        self.ins("addi r5, r3, %d" % imm)
        self.assert_case(expected)

    def case_shimm(self, mn, a, sh, expected):
        self.li("r3", a)
        self.ins("%s r5, r3, %d" % (mn, sh))
        self.assert_case(expected)

    # --- prologue / epilogue ---
    def header(self):
        self.at("# AUTO-GENERATED by gen_corpus.py -- do not edit")
        self.at("# group: %s" % self.name)
        self.at(".global _start")
        self.at(".text")
        self.at("_start:")
        self.ins("li r28, 0")          # checksum = 0

    def footer(self):
        # Print "CHK=" then 8 uppercase hex nibbles of r28, then newline, then halt.
        for ch in "CHK=":
            self.ins("addi r11, r0, %d" % ord(ch))
            self.ins("debug r11")
        for sh in (28, 24, 20, 16, 12, 8, 4, 0):
            self.ins("srli r11, r28, %d" % sh)
            self.ins("andi r11, r11, 0xF")
            self.ins("slti r12, r11, 10")       # r12 = (nibble < 10)
            le = self.lab("hx")
            self.ins("addi r13, r11, 48")       # tentative '0'+n
            self.ins("bne r12, r0, %s" % le)    # nibble<10 -> keep tentative
            self.ins("addi r13, r11, 55")       # else 'A'+(n-10)
            self.at("%s:" % le)
            self.ins("debug r13")
        self.ins("addi r11, r0, 10")
        self.ins("debug r11")
        self.ins("halt")

    def text(self):
        return "\n".join(self.lines) + "\n"


# ----------------------------------------------------------------------
# Group generators
# ----------------------------------------------------------------------

def gen_rr(mn, a_vals, b_vals):
    p = Prog(mn)
    p.header()
    for a in a_vals:
        for b in b_vals:
            p.case_rr(mn, a, b, REF_R[mn](a, b))
    p.footer()
    return p


def gen_addi():
    p = Prog("addi")
    p.header()
    for a in VALUES:
        for imm in IMM12:
            p.case_addi(a, imm, ref_addi(a, imm))
    p.footer()
    return p


def gen_shimm(mn):
    p = Prog(mn)
    p.header()
    for a in VALUES:
        for sh in SHIMM:
            p.case_shimm(mn, a, sh, ref_shimm(mn, a, sh))
    p.footer()
    return p


def gen_decode():
    """
    Immediate-decode bit-walk: force a single bit of the branch/jump offset
    field to be set, then verify control lands on the intended target.

    A mis-decode of offset bit k shifts the landing point by 2^k, so control
    falls into the poison region (each poison insn bumps r20) or overshoots --
    either way r20 != 0 and the assert fires.

    Branch (imm_b) is PC+4-relative (target = beq+4 + imm_b); forward range is
    13-bit signed, so bit 12 is the sign bit -> exercised via a backward branch.
    JAL (imm_j) is PC-relative (target = jal + imm_j), 21-bit signed; we walk
    the low/mid bits. NOTE: imm_j bits 13..20 are NOT covered here (each would
    need up to 1 MB of poison) -- a documented coverage cap, not silent.
    """
    p = Prog("decode")
    p.header()

    # --- BEQ forward: walk offset bits 2..11 (PC+4 base) ---
    for k in range(2, 12):
        npois = (1 << k) // 4          # imm_b = 2^k  => 2^(k-2) poison insns
        lab = p.lab("brf%d" % k)
        p.li("r20", 0)
        p.ins("beq r0, r0, %s" % lab)  # r0==r0 always taken
        for _ in range(npois):
            p.ins("addi r20, r20, 1")  # poison
        p.at("%s:" % lab)
        p.li("r21", 0)
        p.ins("assert_eq r20, r21")
        p.li("r5", 0xB000 | k)
        p.fold("r5", 0xB000 | k)

    # --- BLT backward: negative offset (imm_b sign bit + sign extension) ---
    p.li("r22", 0)
    back = p.lab("brb")
    p.at("%s:" % back)
    p.ins("addi r22, r22, 1")
    p.li("r23", 3)
    p.ins("blt r22, r23, %s" % back)   # loop while r22 < 3 (backward branch)
    p.li("r24", 3)
    p.ins("assert_eq r22, r24")        # looped exactly 3 times
    p.li("r5", 0x600D)
    p.fold("r5", 0x600D)

    # --- JAL forward: walk offset bits 3..12 (PC base) ---
    for k in range(3, 13):
        npois = ((1 << k) - 4) // 4    # imm_j = 2^k => 2^(k-2)-1 poison insns
        lab = p.lab("jf%d" % k)
        p.li("r20", 0)
        p.ins("jal r0, %s" % lab)      # link discarded into r0
        for _ in range(npois):
            p.ins("addi r20, r20, 1")  # poison
        p.at("%s:" % lab)
        p.li("r21", 0)
        p.ins("assert_eq r20, r21")
        p.li("r5", 0x4A00 | k)
        p.fold("r5", 0x4A00 | k)

    p.footer()
    return p


# ----------------------------------------------------------------------
# Driver
# ----------------------------------------------------------------------

ARITH = ['add', 'sub', 'xor', 'or', 'and', 'mul', 'mulh', 'mulhu']
CMP = ['slt', 'sltu', 'seq', 'sne', 'sgt', 'sgtu', 'sle', 'sleu', 'sge', 'sgeu']
SHIFT_R = ['sll', 'srl', 'sra']
DIVREM = ['div', 'rem']
SHIMM_OPS = ['slli', 'srli', 'srai']


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: gen_corpus.py <outdir>\n")
        return 2
    outdir = sys.argv[1]
    os.makedirs(outdir, exist_ok=True)

    progs = []
    for mn in ARITH + CMP + DIVREM:
        progs.append(gen_rr(mn, VALUES, VALUES))
    for mn in SHIFT_R:
        progs.append(gen_rr(mn, VALUES, SHAMTS))
    progs.append(gen_addi())
    for mn in SHIMM_OPS:
        progs.append(gen_shimm(mn))
    progs.append(gen_decode())

    expected = []
    for p in progs:
        path = os.path.join(outdir, "test_%s.s" % p.name)
        with open(path, "w") as f:
            f.write(p.text())
        expected.append("test_%s %08X" % (p.name, p.chk))

    with open(os.path.join(outdir, "expected.txt"), "w") as f:
        f.write("\n".join(expected) + "\n")

    sys.stderr.write("gen_corpus: wrote %d test programs to %s\n"
                     % (len(progs), outdir))
    return 0


if __name__ == "__main__":
    sys.exit(main())
