#!/usr/bin/env python3
"""Generate a random COBOL 85 arithmetic program for differential testing
against GnuCOBOL (tests/gen/run-gen.sh).

    gen-arith.py SEED [STATEMENTS] > prog.cbl

Every statement stands alone: its operands are set from literals first,
and its result is DISPLAYed with a label, so one disagreement names one
statement.  The program stays inside what X3.23-1985 defines exactly:
- COMPUTE uses + - * only, on operands small enough that no
  intermediate passes 30 digits (the precision of intermediate results
  is the implementor's, so a division or a long product inside an
  expression may legitimately differ);
- division is the DIVIDE statement, whose truncation and ROUNDED are
  defined;
- every statement has ON SIZE ERROR, so an oversized or undefined
  result (division by zero) is defined too.
Where the two compilers disagree, the 85 text decides
(docs/oracles.md).
"""
import os
import random
import sys
from decimal import Decimal

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import arith85   # the 85 rules, the arithmetic reference

USAGES = ["DISPLAY", "BINARY", "PACKED-DECIMAL"]


class Item:
    def __init__(self, name, signed, ints, decs, usage):
        self.name, self.signed, self.ints, self.decs, self.usage = \
            name, signed, ints, decs, usage

    def pic(self):
        p = "S" if self.signed else ""
        if self.ints:
            p += "9(%d)" % self.ints
        if self.decs:
            p += "V9(%d)" % self.decs
        return p

    def digits(self):
        return self.ints + self.decs


def rand_item(r, name, maxdig=18):
    while True:
        ints = r.randint(0, 12)
        decs = r.randint(0, 6)
        if 1 <= ints + decs <= maxdig:
            break
    return Item(name, r.random() < 0.6, ints, decs, r.choice(USAGES))


def literal(r, it, fit=True):
    """A numeric literal for item it: usually fitting its PICTURE, sometimes
    one digit wider (the MOVE must truncate), sign only if signed."""
    ints = it.ints if fit else it.ints + 1
    ints = max(ints, 0)
    ip = r.randint(0, 10 ** ints - 1) if ints else 0
    dp = r.randint(0, 10 ** it.decs - 1) if it.decs else 0
    s = str(ip)
    if it.decs:
        s += "." + str(dp).zfill(it.decs)
    if it.signed and r.random() < 0.4 and s.strip("0.") != "":
        s = "-" + s
    return s


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 60
    r = random.Random(seed)
    items = [rand_item(r, "N%02d" % i) for i in range(24)]
    small = [rand_item(r, "K%02d" % i, maxdig=9) for i in range(8)]
    out = []
    w = out.append
    w("identification division.")
    w("program-id. gen%d." % seed)
    w("data division.")
    w("working-storage section.")
    for it in items + small:
        w("01 %s pic %s usage %s." % (it.name, it.pic(), it.usage))
    w("procedure division.")
    refs = []                       # (label, expected line), from arith85
    val = {}                        # each item's current value, as the reference sees it

    def it(o):
        return (o.signed, o.ints, o.decs)

    def setlit(o, text):
        w("    move %s to %s" % (text, o.name))
        val[o.name] = arith85.move(it(o), Decimal(text))
    for k in range(nstmt):
        kind = r.choice(["add", "sub", "mul", "div", "divrem", "move", "compute"])
        tag = '"%d %s"' % (k, kind)
        if kind == "compute":
            ops = r.sample(small, r.randint(2, 3))
        elif kind == "move":
            ops = r.sample(items, 1)
        else:
            ops = r.sample(items, 2)
        res = r.choice([x for x in items if x not in ops])
        for o in ops:
            setlit(o, literal(r, o))
        setlit(res, literal(r, res))
        rnd = " rounded" if r.random() < 0.5 else ""
        a, b = ops[0].name, ops[-1].name
        if kind == "add":
            stmt = "add %s to %s giving %s%s" % (a, b, res.name, rnd)
        elif kind == "sub":
            stmt = "subtract %s from %s giving %s%s" % (a, b, res.name, rnd)
        elif kind == "mul":
            stmt = "multiply %s by %s giving %s%s" % (a, b, res.name, rnd)
        elif kind == "div":
            stmt = "divide %s by %s giving %s%s" % (a, b, res.name, rnd)
        elif kind == "divrem":
            rem = r.choice([x for x in items if x not in ops and x is not res])
            setlit(rem, literal(r, rem))
            stmt = "divide %s by %s giving %s%s remainder %s" % (a, b, res.name, rnd, rem.name)
        elif kind == "move":
            stmt = "move %s to %s" % (a, res.name)
        else:
            expr = ops[0].name
            for o in ops[1:]:
                expr += " %s %s" % (r.choice(["+", "-", "*"]), o.name)
            stmt = "compute %s%s = %s" % (res.name, rnd, expr)
        # the reference's expectation for the line(s) this statement shows
        av, bv = val[a], val[b]
        lab = "%d %s" % (k, kind)
        if kind == "move":
            val[res.name] = arith85.move(it(res), av)
            refs.append(lab + " " + arith85.fmt(it(res), val[res.name]))
        elif kind == "divrem":
            se, q, rm = arith85.divide_remainder(av, bv, it(res), bool(rnd), it(rem))
            if q is not None:
                val[res.name] = q
            if rm is not None:
                val[rem.name] = rm
            refs.append(lab + (" SIZE ERROR " if se else " ") + arith85.fmt(it(res), val[res.name]))
            refs.append(lab + " rem " + arith85.fmt(it(rem), val[rem.name]))
        else:
            if kind == "add":
                exact = av + bv
            elif kind == "sub":
                exact = bv - av
            elif kind == "mul":
                exact = av * bv
            elif kind == "div":
                exact = None if bv == 0 else av / bv
            else:
                exact = eval(stmt.split("=", 1)[1], {}, dict(val))   # + - * only: Python's precedence is the text's
            ok, sv = (False, None) if exact is None else arith85.store(it(res), exact, bool(rnd))
            if ok:
                val[res.name] = sv
            refs.append(lab + (" " if ok else " SIZE ERROR ") + arith85.fmt(it(res), val[res.name]))
        if kind == "move":
            w("    " + stmt)
            w("    display %s \" \" %s" % (tag, res.name))
        else:
            w("    " + stmt)
            w("        on size error display %s \" SIZE ERROR \" %s" % (tag, res.name))
            w("        not on size error display %s \" \" %s" % (tag, res.name))
            w("    end-%s" % stmt.split()[0])
            if kind == "divrem":
                w("    display %s \" rem \" %s" % (tag, rem.name))
    w("    stop run.")
    print("\n".join(out))
    for line in refs:                        # the reference's lines, for run-gen.sh
        print(line, file=sys.stderr)


if __name__ == "__main__":
    main()
