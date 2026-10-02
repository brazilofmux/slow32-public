#!/usr/bin/env python3
"""Generate a random program of computed positions: subscripts and
reference modification whose values are expressions (GEN=pos
tests/gen/run-self.sh REV ..., and GEN=pos tests/gen/run-gen.sh ...
against GnuCOBOL).

    gen-pos.py SEED [STATEMENTS] > prog.cbl

A subscript or a leftmost position or a length that is an integer
expression over integer items is computed in registers
(docs/performance.md, 2026-10-01: positions); anything else -- a packed
item, a decimal one, a division, an operand that is itself subscripted
-- still goes through the runtime's stack.  The two must find the same
bytes, so this writes references of every such kind, into one item, a
table, a table of two dimensions and a numeric table, each with its
position made of items of every usage the two paths divide on: COMP of
two and four bytes, signed and not, COMP-5, DISPLAY of one to nine
digits, signed DISPLAY, COMP-3, an item with a decimal place.

Every position is in range by construction: the generator picks the
position first, then an expression and operand values that make it, and
MOVEs those values into the operands before the statement.  Each
statement DISPLAYs what it found or changed.
"""
import os
import random
import sys

# picture, largest value it is given, whether a subscript of the 85 form may be it
KINDS = [
    ("pic 9(4) comp", 9999), ("pic s9(4) comp", 9999), ("pic 9(9) comp", 99999), ("pic s9(9) comp", 99999),
    ("pic 9(4) comp-5", 9999), ("pic s9(9) comp-5", 99999), ("pic 9(2) comp", 99),
    ("pic 9", 9), ("pic 99", 99), ("pic 9(4)", 9999), ("pic 9(9)", 99999),
    ("pic s99", 99), ("pic s9(5)", 99999),
    ("pic 9(3) comp-3", 999), ("pic s9(7) comp-3", 99999),
    ("pic 9(3)v9", 999),
]
# An intermediate result below zero (8 - 9 + 2): the old and the new
# compiler must agree on it, and do; GnuCOBOL computes a position in an
# unsigned BINARY operand's type and wraps (docs/oracles.md,
# 2002/refmodneg), so run-gen.sh asks for programs without one.
NEG = os.environ.get("GENPOS_NEG", "1") != "0"
XLEN = 40
NT = 12          # T occurs
R, C = 4, 5      # the table of two dimensions
NN = 8           # the numeric table: N(k) holds k


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    r = random.Random(seed * 7919 + 5)
    items = []
    for i in range(14):
        pic, cap = r.choice(KINDS)
        items.append(("V%02d" % i, pic, cap))
    out = []
    w = out.append
    w("identification division.")
    w("program-id. genpos.")
    w("data division.")
    w("working-storage section.")
    w('01  X pic x(%d) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcd".' % XLEN)
    w("01  TB.")
    w("    05  T occurs %d times pic x(5)." % NT)
    w("01  T2.")
    w("    05  RW occurs %d times." % R)
    w("        10  CL occurs %d times pic x(3)." % C)
    w("01  NTB.")
    w("    05  N occurs %d times pic 9(2) comp." % NN)
    w("01  CTB.")
    w("    05  CN occurs 6 times pic 9(4).")
    w("01  W pic x(%d)." % XLEN)
    w("01  K pic 9(4) comp.")
    for name, pic, cap in items:
        w("01  %s %s." % (name, pic))
    w("procedure division.")
    w("main-para.")
    w("    perform varying K from 1 by 1 until K > %d" % NT)
    w("        move X(K:5) to T(K)")
    w("    end-perform")
    w("    perform varying K from 1 by 1 until K > %d" % (R * C))
    w("        move X(K + 3:3) to T2((K - 1) * 3 + 1:3)")
    w("    end-perform")
    w("    perform varying K from 1 by 1 until K > %d" % NN)
    w("        move K to N(K)")
    w("    end-perform")
    w("    perform varying K from 1 by 1 until K > 6")
    w("        move 0 to CN(K)")
    w("    end-perform")

    for k in range(nstmt):
        used = {}           # item name -> value in this statement

        def item(v, integer=False):
            """an item to hold v in this statement (integer: one with no decimal place)"""
            for _ in range(60):
                name, pic, cap = r.choice(items)
                if integer and "v" in pic:
                    continue
                if cap >= v and (name not in used or used[name] == v):
                    used[name] = v
                    return name
            used_lit.append(v)
            return str(v)

        used_lit = []

        def expr(v, depth=0):
            """an expression whose value is v (v >= 0)"""
            c = r.randrange(17 if depth == 0 else 9)
            if c == 0 or (c == 1 and v > 9999):
                return item(v, True)            # alone it is a subscript of the 1985 form: an integer item
            if c == 1:
                return str(v) if r.random() < 0.3 else item(v, True)
            if c == 2:
                a = r.randint(0, min(v, 5)); return "%s + %d" % (item(v - a, True), a)     # the 1985 forms again
            if c == 3:
                a = r.randint(1, 4); return "%s - %d" % (item(v + a, True), a)
            if c == 4:
                a = r.randint(0, v); return "%s + %s" % (item(a), item(v - a))
            if c == 5:
                a = r.randint(1, 6); b = v // a; return "%s * %s + %d" % (item(a), item(b), v - a * b)
            if c == 6:
                a = (v + 1) // 2 + r.randint(0, 3); return "%s * 2 - %s" % (item(a), item(2 * a - v))
            if c == 7:
                a = r.randint(0, 5); b = r.randint(0, 5); return "(%s + %s) - %s" % (item(v + a), item(b), item(a + b))
            if c == 8:
                a = r.randint(1, 3); return "%d + %s" % (a, item(v - a)) if v >= a else item(v, True)
            if c == 9:
                m = r.choice([7, 12, 40, 100]); q = r.randint(0, 50)
                if v - 1 < m and v >= 1:
                    return "function mod(%s, %d) + 1" % (item(v - 1 + m * q, True), m)
                return item(v, True)
            if c == 10:      # a division (exact: a position is an integer): the stack's
                d = r.choice([2, 3]); return "%s / %d" % (item(v * d), d)
            if c == 11:      # an operand that is itself subscripted
                if 1 <= v <= NN:
                    return "N(%s)" % expr(v, 1)
                return item(v, True)
            if c == 14:      # the least of the value and something larger: a length kept inside its item
                return "function min(%s, %s)" % (item(v, True), r.choice([str(v + r.randint(0, 50)), item(v + r.randint(0, 9), True)]))
            if c == 15:
                return "function max(%s, %s)" % (r.choice([str(r.randint(0, v)), item(r.randint(0, v), True)]), item(v, True))
            if c == 16:
                return "function min(%s + %s, %d)" % (item(v, True), item(r.randint(0, 3), True), v)
            if c == 12:
                if 2 <= v <= NN + 1:
                    return "N(%s) + 1" % item(v - 1, True)
                return "%s - %s + %s" % (item(v + 7 if NEG else v + 9), item(9), item(2 if NEG else 0))
            return "%s - %s + %s" % (item(v + 3 if NEG else v + 5), item(5), item(2 if NEG else 0))

        def sub85(v):
            """a subscript of the 1985 forms: an item, or an item + or - an integer"""
            c = r.randrange(4)
            if c == 0 and v >= 1:
                a = r.randint(0, min(v - 1, 3)); return "%s + %d" % (item(v - a, True), a) if a else item(v, True)
            if c == 1:
                a = r.randint(1, 3); return "%s - %d" % (item(v + a, True), a)
            return item(v, True)

        shape = r.randrange(15)
        tail = []
        if shape == 0:
            s = r.randint(1, XLEN); l = r.randint(1, XLEN + 1 - s)
            st = "move X(%s:%s) to W" % (expr(s), expr(l)); tail = ['display "%d " W' % k]
        elif shape == 1:
            s = r.randint(1, XLEN)
            st = "move X(%s:) to W" % expr(s); tail = ['display "%d " W' % k]
        elif shape == 2:
            s = r.randint(1, XLEN - 2); l = r.randint(1, min(6, XLEN + 1 - s))
            st = 'move "%s" to X(%s:%s)' % ("".join(r.choice("pqrstuvwxyz") for _ in range(l)), expr(s), expr(l))
            tail = ['display "%d " X' % k]
        elif shape == 3:
            st = "move T(%s) to W" % expr(r.randint(1, NT)); tail = ['display "%d " W' % k]
        elif shape == 4:
            s = r.randint(1, 5); l = r.randint(1, 6 - s)
            st = "move T(%s)(%s:%s) to W" % (sub85(r.randint(1, NT)), expr(s), expr(l)); tail = ['display "%d " W' % k]
        elif shape == 5:
            st = "move CL(%s, %s) to W" % (expr(r.randint(1, R)), expr(r.randint(1, C))); tail = ['display "%d " W' % k]
        elif shape == 6:
            s = r.randint(1, 3)
            st = "move CL(%s, %s)(%s:1) to W" % (sub85(r.randint(1, R)), sub85(r.randint(1, C)), expr(s)); tail = ['display "%d " W' % k]
        elif shape == 7:
            a = r.randint(1, XLEN); b = r.randint(1, XLEN)
            st = 'if X(%s:1) = X(%s:1) display "%d same" else display "%d differ" end-if' % (expr(a), expr(b), k, k)
        elif shape == 8:
            i = r.randint(1, 6)
            e = expr(i)
            st = "add %d to CN(%s)" % (r.randint(1, 50), e); tail = ['display "%d " CN(%d)' % (k, i)]
        elif shape == 9:
            i = r.randint(1, NT); s = r.randint(1, 5)
            st = 'move "%s" to T(%s)(%s:1)' % (r.choice("0123456789"), expr(i), expr(s)); tail = ['display "%d " T(%d)' % (k, i)]
        elif shape == 10:
            i = r.randint(1, NN)
            st = "move N(%s) to W" % expr(i); tail = ['display "%d " W(1:4)' % k]
        elif shape == 11:
            s = r.randint(1, XLEN - 1); l = r.randint(1, XLEN + 1 - s)
            st = "move X(%s:%s) to W(%s:%s)" % (expr(s), expr(l), expr(r.randint(1, XLEN + 1 - l)), expr(l)); tail = ['display "%d " W' % k]
        elif shape == 13:    # an item to a part: padded when the part is the longer
            s = r.randint(1, XLEN - 1); l = r.randint(1, XLEN + 1 - s)
            st = "move T(%s) to X(%s:%s)" % (sub85(r.randint(1, NT)), expr(s), expr(l)); tail = ['display "%d " X' % k]
        elif shape == 14:    # a part to the end of the item, to a part to the end
            s = r.randint(1, XLEN); d = r.randint(1, XLEN)
            st = "move X(%s:) to W(%s:)" % (expr(s), expr(d)); tail = ['display "%d " W' % k]
        else:
            i = r.randint(1, R); j = r.randint(1, C)
            st = 'move "%s" to CL(%s, %s)' % ("".join(r.choice("JKL") for _ in range(3)), expr(i), expr(j))
            tail = ['display "%d " RW(%d)' % (k, i)]
        for name, v in used.items():
            w("    move %d to %s" % (v, name))
        if shape in (0, 1, 3, 4, 5, 6, 10, 11, 14):
            w("    move spaces to W")
        w("    " + st)
        for t in tail:
            w("    " + t)
    w("    stop run.")
    print("\n".join(out))


main()
