#!/usr/bin/env python3
"""Generate a random COBOL 85 program of conditions, for differential
testing against GnuCOBOL (GEN=cond tests/gen/run-gen.sh ...).

    gen-cond.py SEED [STATEMENTS] > prog.cbl

Each statement sets its operands from literals, then evaluates one
condition and DISPLAYs T or F with its label:
- relation conditions: numeric against numeric across usages and scales;
  alphanumeric of unequal lengths (the shorter takes spaces); figurative
  constants; an unsigned integer DISPLAY item against an alphanumeric one
  (compared as characters);
- class conditions: NUMERIC, ALPHABETIC, ALPHABETIC-UPPER and -LOWER on
  alphanumeric content, and NUMERIC on an unsigned DISPLAY item given
  other characters through its group;
- sign conditions: POSITIVE, NEGATIVE, ZERO, with and without NOT;
- combined conditions with AND, OR, NOT and parentheses, and
  abbreviated combined relations (a > b AND < c, a = b OR c, with NOT
  before the relational operator).
"""
import random
import sys
from decimal import Decimal

USAGES = ["DISPLAY", "BINARY", "PACKED-DECIMAL"]
OPS = ["=", "<", ">", "<=", ">=", "NOT =", "NOT <", "NOT >"]
CHARS = "ABCabc 019Z"


def num_item(r, name):
    signed = r.random() < 0.6
    ints = r.randint(0, 7)
    decs = r.randint(0, 3)
    if ints + decs == 0:
        ints = 1
    pic = ("S" if signed else "") + ("9(%d)" % ints if ints else "") + ("V9(%d)" % decs if decs else "")
    return dict(name=name, pic=pic, usage=r.choice(USAGES), signed=signed, ints=ints, decs=decs, kind="n")


def num_lit(r, it):
    if r.random() < 0.15:
        return "0"
    ip = r.randint(0, 10 ** it["ints"] - 1) if it["ints"] else 0
    s = str(ip)
    if it["decs"]:
        s += "." + str(r.randint(0, 10 ** it["decs"] - 1)).zfill(it["decs"])
    if it["signed"] and r.random() < 0.4 and s.strip("0.") != "":
        s = "-" + s
    return s


def alnum_lit(r, n):
    return '"' + "".join(r.choice(CHARS) for _ in range(r.randint(1, n))) + '"'


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 60
    r = random.Random(seed * 104729 + 7)
    nums = [num_item(r, "N%02d" % i) for i in range(10)]
    alns = [dict(name="A%02d" % i, n=r.randint(1, 8), kind="x") for i in range(8)]
    ints = [dict(name="I%02d" % i, n=r.randint(1, 5), kind="i") for i in range(3)]
    out = []
    w = out.append
    w("identification division.")
    w("program-id. gcd%d." % seed)
    w("data division.")
    w("working-storage section.")
    for it in nums:
        w("01 %s pic %s usage %s." % (it["name"], it["pic"], it["usage"]))
    for it in alns:
        w("01 %s pic x(%d)." % (it["name"], it["n"]))
    for it in ints:
        w("01 %s pic 9(%d)." % (it["name"], it["n"]))
    # an unsigned DISPLAY item reached through its group, for NUMERIC
    w("01 G1.")
    w("   05 G1N pic 9(4).")
    w("procedure division.")

    def setn(it):
        return "    move %s to %s" % (num_lit(r, it), it["name"])

    def seta(it):
        return "    move %s to %s" % (alnum_lit(r, it["n"] + 2), it["name"])

    def seti(it):
        return "    move %d to %s" % (r.randint(0, 10 ** it["n"] - 1), it["name"])

    for k in range(nstmt):
        kind = r.choice(["numrel", "numrel", "alnrel", "fig", "mixed", "class", "classn",
                         "sign", "comb", "abbr", "abbr"])
        sets = []
        tag = ""
        if kind == "numrel":
            a, b = r.sample(nums, 2)
            sets = [setn(a), setn(b)]
            obj = r.choice([b["name"], num_lit(r, b)])
            cond = "%s %s %s" % (a["name"], r.choice(OPS), obj)
            # a negative literal with more integer digits than the subject:
            # the oracle reads it unsigned (docs/oracles.md, free/negcmp);
            # labelled so run-gen.sh can count exactly that apart
            if obj.startswith("-") and len(obj[1:].split(".")[0]) > a["ints"]:
                # the algebraic truth (VI-55), so run-gen.sh can require
                # ours to be it before counting the oracle's apart
                av = Decimal(sets[0].split()[1])
                op = cond.split()[1:-1]
                rel = " ".join(op)
                lv = Decimal(obj)
                truth = {"=": av == lv, "<": av < lv, ">": av > lv, "<=": av <= lv, ">=": av >= lv,
                         "NOT =": av != lv, "NOT <": not av < lv, "NOT >": not av > lv}[rel]
                tag = " neglit=%s" % ("T" if truth else "F")
        elif kind == "alnrel":
            a, b = r.sample(alns, 2)
            sets = [seta(a), seta(b)]
            cond = "%s %s %s" % (a["name"], r.choice(OPS), r.choice([b["name"], alnum_lit(r, 6)]))
        elif kind == "fig":
            if r.random() < 0.5:
                a = r.choice(alns)
                sets = [r.choice([seta(a), "    move spaces to %s" % a["name"],
                                  "    move high-values to %s" % a["name"],
                                  "    move low-values to %s" % a["name"]])]
                fig = r.choice(["SPACE", "SPACES", "HIGH-VALUE", "LOW-VALUE", "ZERO", 'ALL "A"', 'ALL "ab"'])
            else:
                a = r.choice(nums)
                sets = [setn(a)]
                fig = "ZERO"
            cond = "%s %s %s" % (a["name"], r.choice(OPS), fig)
        elif kind == "mixed":
            a, b = r.choice(ints), r.choice(alns)
            sets = [seti(a), r.choice([seta(b), '    move "%0*d" to %s' % (b["n"], r.randint(0, 99), b["name"])])]
            if r.random() < 0.5:
                cond = "%s %s %s" % (a["name"], r.choice(OPS), b["name"])
            else:
                cond = "%s %s %s" % (b["name"], r.choice(OPS), a["name"])
        elif kind == "class":
            a = r.choice(alns)
            sets = [r.choice([seta(a), '    move "%s" to %s' % ("".join(r.choice("0123456789") for _ in range(a["n"])), a["name"]),
                              '    move "%s" to %s' % ("".join(r.choice("ABCabc ") for _ in range(a["n"])), a["name"])])]
            cls = r.choice(["NUMERIC", "ALPHABETIC", "ALPHABETIC-UPPER", "ALPHABETIC-LOWER"])
            cond = "%s %s%s" % (a["name"], "NOT " if r.random() < 0.3 else "", cls)
        elif kind == "classn":
            sets = ['    move "%s" to G1' % "".join(r.choice("0123456789 A") for _ in range(4))]
            cond = "G1N %sNUMERIC" % ("NOT " if r.random() < 0.3 else "")
        elif kind == "sign":
            a = r.choice(nums)
            sets = [setn(a)]
            cond = "%s %s%s" % (a["name"], "NOT " if r.random() < 0.3 else "", r.choice(["POSITIVE", "NEGATIVE", "ZERO"]))
        elif kind == "comb":
            a, b, c = r.sample(nums, 3)
            sets = [setn(a), setn(b), setn(c)]
            t1 = "%s %s %s" % (a["name"], r.choice(OPS), b["name"])
            # NOT before a relation whose operator has its own NOT
            # (OR NOT b NOT < c) is refused by the oracle: whole programs
            # were lost, so a negated relation keeps a positive operator
            t2 = "%s %s %s" % (b["name"], r.choice(["=", "<", ">", "<=", ">="]), c["name"])
            t3 = "%s %s" % (c["name"], r.choice(["POSITIVE", "NEGATIVE", "ZERO"]))
            shape = r.randint(0, 3)
            if shape == 0:
                cond = "%s AND %s OR %s" % (t1, t2, t3)
            elif shape == 1:
                cond = "%s OR %s AND %s" % (t1, t2, t3)
            elif shape == 2:
                cond = "NOT (%s OR %s) AND %s" % (t1, t2, t3)
            else:
                cond = "(%s OR NOT %s) AND NOT %s" % (t1, t2, t3)
        else:  # abbreviated combined relation
            a, b, c, d = r.sample(nums, 4)
            sets = [setn(a), setn(b), setn(c), setn(d)]
            shape = r.randint(0, 4)
            o1, o2 = r.choice(OPS), r.choice(OPS)
            if shape == 0:      # a op1 b AND op2 c
                cond = "%s %s %s AND %s %s" % (a["name"], o1, b["name"], o2, c["name"])
            elif shape == 1:    # a op1 b OR c   (subject and operator implied)
                cond = "%s %s %s OR %s" % (a["name"], o1, b["name"], c["name"])
            elif shape == 2:    # a op1 b OR op2 c AND d
                cond = "%s %s %s OR %s %s AND %s" % (a["name"], o1, b["name"], o2, c["name"], d["name"])
            elif shape == 3:    # a = b OR NOT op2 c   (NOT before an operator is part of it)
                cond = "%s %s %s OR NOT %s %s" % (a["name"], o1, b["name"], r.choice(["=", "<", ">"]), c["name"])
            else:               # an abbreviated sequence inside parentheses (the text's own example)
                cond = "NOT (%s %s %s OR %s %s)" % (a["name"], o1, b["name"], o2, c["name"])
        for st in sets:
            w(st)
        w("    if %s" % cond)
        w('        display "%d T%s"' % (k, tag))
        w("    else")
        w('        display "%d F%s"' % (k, tag))
        w("    end-if")
    w("    stop run.")
    print("\n".join(out))


if __name__ == "__main__":
    main()
