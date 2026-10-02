#!/usr/bin/env python3
"""Generate a random program of numeric literals moved to numeric items
(GEN=lit tests/gen/run-self.sh REV ...).

    gen-lit.py SEED [STATEMENTS] > prog.cbl

MOVE of a numeric literal to a numeric item stores bytes the compiler
can work out -- it runs the store's own kernel on the literal
(docs/performance.md; move_lit_bytes in src/cobc/move.h) -- where the
runtime's store was called before.  The bytes must be the ones the
store leaves, for every usage and picture it takes and for every
literal: more digits than the item, fewer, a fraction the item cuts, a
sign the item has no place for, zero, the figurative ZERO.  So this
writes items of every such kind -- DISPLAY signed and not, the sign
leading, trailing, separate; BINARY, COMP-5 and the 2002 binary types of
every size; PACKED-DECIMAL; COMP-X; decimal places; P scaling and BLANK
WHEN ZERO, which stay the runtime's -- moves literals to them, and
prints each item's value and every byte it occupies.  The compiler and
runtime before the change are the oracle (run-self.sh): a byte dump of
binary items is this machine's, so GnuCOBOL is not asked.
"""
import random
import sys


def pic(r):
    """(picture and usage, digits before the point, after it)"""
    k = r.randrange(14)
    i = r.randint(1, 9); f = r.choice([0, 0, 0, 1, 2, 4])
    if k == 0:
        return "pic 9(%d)" % i, i, 0
    if k == 1:
        return "pic s9(%d)%s" % (i, "v9(%d)" % f if f else ""), i, f
    if k == 2:
        return "pic s9(%d) sign %s%s" % (i, r.choice(["leading", "trailing"]), r.choice(["", " separate"])), i, 0
    if k == 3:
        i = r.randint(1, 18); return "pic %s9(%d) binary" % (r.choice(["", "s"]), i), i, 0
    if k == 4:
        i = r.randint(1, 16); return "pic s9(%d)v99 comp" % i, i, 2
    if k == 5:
        i = r.randint(1, 18); return "pic %s9(%d) comp-5" % (r.choice(["", "s"]), i), i, 0
    if k == 6:
        i = r.randint(1, 18); return "pic %s9(%d) packed-decimal" % (r.choice(["", "s"]), i), i, 0
    if k == 7:
        i = r.randint(1, 14); return "pic s9(%d)v9(%d) comp-3" % (i, f or 2), i, f or 2
    if k == 8:
        return r.choice(["binary-char", "binary-char unsigned", "binary-short", "binary-short unsigned",
                         "binary-long", "binary-long unsigned", "binary-double", "binary-double unsigned"]), 9, 0
    if k == 9:
        i = r.randint(1, 12); return "pic 9(%d) comp-x" % i, i, 0
    if k == 10:
        return "pic 9(%d)v9(%d)" % (i, f or 1), i, f or 1
    if k == 11:
        return r.choice(["pic 9(3)pp", "pic sp(2)9(3)", "pic 9(4) blank when zero", "pic s9(3)v99 sign leading separate"]), 3, 2
    if k == 12:
        i = r.randint(10, 18); return "pic s9(%d)" % i, i, 0
    return "pic 9(%d)v9(%d) comp-3" % (i, f or 3), i, f or 3


def literal(r, i, f):
    """a literal for an item of i integer and f fraction digits: fitting, or not"""
    c = r.randrange(10)
    if c == 0:
        return r.choice(["0", "zero", "zeros", "0.0", "-0"])
    ni = max(1, min(18, i + r.choice([0, 0, 0, -1, 1, 2, -i + 1])))
    nf = max(0, min(18 - ni, f + r.choice([0, 0, 0, 1, -1, 3])))
    digits = "".join(r.choice("0123456789") for _ in range(ni))
    if c == 1:
        digits = "9" * ni
    if c == 2:
        digits = "1" + "0" * (ni - 1)
    frac = "".join(r.choice("0123456789") for _ in range(nf))
    s = digits + ("." + frac if nf else "")
    if r.random() < 0.35:
        s = "-" + s
    elif r.random() < 0.1:
        s = "+" + s
    return s


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    r = random.Random(seed * 15485863 + 11)
    items = [pic(r) for _ in range(18)]
    out = []
    w = out.append
    w("identification division.")
    w("program-id. genlit.")
    w("data division.")
    w("working-storage section.")
    for k, (p, i, f) in enumerate(items):
        w("01  G%02d." % k)
        w("    05  I%02d %s." % (k, p))
    w("01  J pic 9(4) comp.")
    w("procedure division.")
    w("main-para.")
    for n in range(nstmt):
        k = r.randrange(len(items))
        p, i, f = items[k]
        w("    move %s to I%02d" % (literal(r, i, f), k))
        w('    display "%d " I%02d' % (n, k))
        w("    perform varying J from 1 by 1 until J > function length(G%02d)" % k)
        w("        display function ord(G%02d(J:1))" % k)
        w("    end-perform")
    w("    stop run.")
    print("\n".join(out))


main()
