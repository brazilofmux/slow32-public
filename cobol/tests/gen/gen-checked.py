#!/usr/bin/env python3
"""Generate a random program for the checked 64-bit arithmetic
(docs/plans/performance.md; GEN=checked tests/gen/run-self.sh REV ...).

    gen-checked.py SEED [STATEMENTS] > prog.cbl

A COMPUTE whose intermediate could pass 18 digits by its pictures is
computed in 64 bits, each operation's inputs tested, with the wide
stack's code behind the tests.  The two must store the same value, so
this writes such statements -- products and sums of items of up to 18
digits, in DISPLAY, BINARY, PACKED-DECIMAL and COMP-5, scaled and not;
literal multipliers; FUNCTION MOD, ABS, INTEGER, INTEGER-PART, MAX and
MIN over them; FUNCTION NUMVAL of one character, a digit or not; unary minus;
ROUNDED receivers -- and gives the operands values on
both sides of every test: small ones, ones at 2^30, 2^31 and 2^62 and
either side of them, and the largest their pictures hold.  No SIZE ERROR
phrase: that takes the stack.  A value too large for its receiver is
stored as this compiler stores it, which is what the compiler before
the change did with the wide stack; that compiler is the oracle
(run-self.sh), not GnuCOBOL -- the standard leaves such a store
undefined.
"""
import random
import sys

USAGES = ["DISPLAY", "BINARY", "PACKED-DECIMAL", "COMP-5"]
EDGES = [0, 1, 2, 7, 99, 1000, 2**15, 2**30 - 1, 2**30, 2**30 + 1, 2**31 - 1, 2**31, 2**31 + 1,
         2**32, 10**9, 2**40, 10**12, 2**53, 10**17, 2**62 - 1, 2**62, 2**62 + 1, 10**18 - 1]


class Item:
    def __init__(self, name, signed, ints, decs, usage):
        self.name, self.signed, self.ints, self.decs, self.usage = name, signed, ints, decs, usage

    def pic(self):
        p = "S" if self.signed else ""
        if self.ints:
            p += "9(%d)" % self.ints
        if self.decs:
            p += "V9(%d)" % self.decs
        return p


def rand_item(r, name, big):
    while True:
        ints = r.randint(9, 18) if big else r.randint(1, 9)
        decs = r.choice([0, 0, 0, 2, 4]) if not big else r.choice([0, 0, 0, 0, 2])
        if 1 <= ints + decs <= 18:
            break
    return Item(name, r.random() < 0.7, ints, decs, r.choice(USAGES))


def value(r, it):
    """a literal for the item: an edge value or a random one, within its picture"""
    digits = it.ints + it.decs
    lim = 10 ** digits - 1
    k = r.random()
    if k < 0.45:
        v = r.choice(EDGES)
    elif k < 0.7:
        v = r.randint(0, 999)
    elif k < 0.85:
        v = r.randint(0, 2**31)
    else:
        v = r.randint(0, lim)
    v = min(v, lim)
    s = str(v).rjust(digits, "0")
    text = s[:it.ints] or "0"
    if it.decs:
        text += "." + s[it.ints:]
    if it.signed and v and r.random() < 0.35:
        text = "-" + text
    return text


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    r = random.Random(seed)
    big = [rand_item(r, "B%02d" % i, True) for i in range(8)]
    small = [rand_item(r, "S%02d" % i, False) for i in range(6)]
    ints = [Item("I%02d" % i, r.random() < 0.7, r.randint(9, 18), 0, r.choice(USAGES)) for i in range(5)]
    recv = [rand_item(r, "R%02d" % i, r.random() < 0.7) for i in range(8)]
    wides = [Item("W%02d" % i, i != 1, 18, 0, r.choice(USAGES)) for i in range(3)]      # eighteen digits, for sums of products
    out = []
    w = out.append
    w("IDENTIFICATION DIVISION.")
    w("PROGRAM-ID. GENCHK.")
    w("DATA DIVISION.")
    w("WORKING-STORAGE SECTION.")
    for it in big + small + ints + recv + wides:
        w("01  %s PIC %s USAGE %s." % (it.name, it.pic(), it.usage))
    w('01  DG PIC X(16) VALUE "0123456789 -.A+9".')
    w("01  DC PIC X.")
    w("01  DP PIC 9(4) COMP.")
    w("PROCEDURE DIVISION.")
    w("MAIN.")

    def lit():
        return str(r.choice([2, 3, 7, 10, 31, 100, 1000, 65536, 1103515245, 10**9, 2**31, 12345678901]))

    for k in range(nstmt):
        a, b, c = r.sample(big + small, 3)
        i1, i2 = r.sample(ints, 2)
        res = r.choice(recv)
        shape = r.randrange(20)
        ops = [a, b, c]
        if shape == 0:
            e = "%s * %s" % (a.name, b.name)
        elif shape == 1:
            e = "%s * %s + %s" % (a.name, b.name, c.name)
        elif shape == 2:
            e = "%s * %s + %s" % (a.name, lit(), b.name)
        elif shape == 3:
            e = "(%s + %s) * %s" % (a.name, b.name, c.name)
        elif shape == 4:
            e = "%s + %s - %s" % (a.name, b.name, c.name)
        elif shape == 5:
            e = "- (%s * %s)" % (a.name, b.name)
        elif shape == 6:
            e = "FUNCTION MOD(%s * %s + %s, %s)" % (i1.name, lit(), r.randint(0, 99999), r.choice([26, 97, 9999991, 2147483648, 1000000007]))
            ops = [i1]
        elif shape == 7:
            e = "FUNCTION MOD(%s * %s + %s, %s)" % (i1.name, i2.name, lit(), r.choice([7, 1000003, 2147483648]))
            ops = [i1, i2]
        elif shape == 18:
            # MOD by an ITEM (2026-10-08): the checked path with a zero test; a zero divisor is the stack's answer on both sides
            e = "FUNCTION MOD(%s * %s + %s, %s)" % (i1.name, lit(), r.randint(0, 99999), i2.name)
            ops = [i1, i2]
        elif shape == 19:
            e = "FUNCTION REM(%s * %s + %s, %s)" % (i1.name, i2.name, lit(), i2.name)
            ops = [i1, i2]
        elif shape == 8:
            e = "FUNCTION ABS(%s * %s - %s)" % (a.name, b.name, c.name)
        elif shape == 9:
            e = "FUNCTION INTEGER(%s * %s) + %s" % (a.name, b.name, i1.name)
            ops = [a, b, i1]
        elif shape == 10:
            e = "%s * %s * %s" % (a.name, b.name, c.name)
        elif shape == 11:
            # two products the pictures bound below 9*10^18, whose sum they do not
            # -- and values near the top of eighteen digits, so that it does pass it
            w1, w2 = r.sample(wides, 2)
            sign = r.choice("+-")
            e = "%s * %d %s %s * %d" % (w1.name, r.choice([2, 3, 5, 7]), sign, w2.name, r.choice([2, 3, 5, 7]))
            ops = []
            for o in (w1, w2):
                v = r.choice([10**18 - 1, 10**18 - 1, 999999999999999998, 6 * 10**17, 2**59, r.randint(0, 10**18 - 1), r.randint(0, 9999)])
                neg = "-" if o.signed and v and r.random() < 0.5 else ""
                w("    MOVE %s%d TO %s" % (neg, v, o.name))
        elif shape == 15:
            e = "FUNCTION %s(%s, %s) * %s" % (r.choice(["MAX", "MIN"]), a.name, b.name, lit())
            ops = [a, b]
        elif shape == 16:
            e = "FUNCTION %s(%s, %s, %s) + %s" % (r.choice(["MAX", "MIN"]), a.name, lit(), b.name, c.name)
        elif shape == 17:
            e = "FUNCTION MIN(%s * %s, FUNCTION MAX(%s, %s - %s))" % (a.name, lit(), b.name, c.name, a.name)
        elif shape == 13:
            # a number read a digit at a time: NUMVAL of one character, which
            # is a digit nearly always -- and now and then is not
            pos = r.randint(1, 10) if r.random() < 0.8 else r.randint(11, 16)
            w("    MOVE %d TO DP" % pos)
            e = "%s * 10 + FUNCTION NUMVAL(DG(DP:1))" % a.name
            ops = [a]
        elif shape == 14:
            w('    MOVE "%s" TO DC' % r.choice("0123456789012345678959 -"))
            e = "FUNCTION NUMVAL(DC) + %s * %s" % (a.name, lit())
            ops = [a]
        else:
            e = "FUNCTION INTEGER-PART(%s * %s) - %s * %s" % (a.name, lit(), b.name, c.name)
        for o in ops:
            w("    MOVE %s TO %s" % (value(r, o), o.name))
        w("    MOVE 0 TO %s" % res.name)
        rnd = " ROUNDED" if r.random() < 0.3 else ""
        w("    COMPUTE %s%s = %s" % (res.name, rnd, e))
        w('    DISPLAY "%d " %s' % (k, res.name))
    w("    STOP RUN.")
    print("\n".join(out))


main()
