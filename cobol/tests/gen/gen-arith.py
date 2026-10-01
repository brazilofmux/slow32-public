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
import random
import sys

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
    for k in range(nstmt):
        kind = r.choice(["add", "sub", "mul", "div", "divrem", "move", "compute"])
        tag = '"%d %s"' % (k, kind)
        if kind == "compute":
            ops = r.sample(small, r.randint(2, 3))
        elif kind == "move":
            ops = r.sample(items, 1)
        else:
            ops = r.sample(items, 2)
        res = r.choice([it for it in items if it not in ops])
        for o in ops:
            w("    move %s to %s" % (literal(r, o), o.name))
        w("    move %s to %s" % (literal(r, res), res.name))
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
            rem = r.choice([it for it in items if it not in ops and it is not res])
            w("    move %s to %s" % (literal(r, rem), rem.name))
            stmt = "divide %s by %s giving %s%s remainder %s" % (a, b, res.name, rnd, rem.name)
        elif kind == "move":
            stmt = "move %s to %s" % (a, res.name)
        else:
            expr = ops[0].name
            for o in ops[1:]:
                expr += " %s %s" % (r.choice(["+", "-", "*"]), o.name)
            stmt = "compute %s%s = %s" % (res.name, rnd, expr)
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


if __name__ == "__main__":
    main()
