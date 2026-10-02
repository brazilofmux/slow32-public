#!/usr/bin/env python3
"""Generate a random program of integer items, some standing alone and
some not, used as numbers and used as bytes
(GEN=native tests/gen/run-flag.sh -fno-native-items FIRST COUNT).

    gen-native.py SEED [STATEMENTS] > prog.cbl

An item that stands alone and is only ever used as a number is written
the machine's way (src/cobc/native.h; docs/plans/census.md): an unsigned
DISPLAY integer as a binary one, a COMP item in the machine's byte
order.  Nothing a program prints may change with that.  What decides it
is everything this generator writes:

- items at level 77 and 01, and under groups that are named by a
  statement or never are; with a numeric VALUE, ZERO, none, and under a
  group's VALUE; redefined, and redefining, with the other name used or
  not; with condition-names whose values are numbers;
- uses of the number: the arithmetic verbs with ROUNDED and SIZE ERROR,
  MOVE of a literal, to and from items of the same description and of
  others, to edited and alphanumeric items, relations with literals and
  with one another, condition-names, SET ... TO TRUE, subscripts,
  PERFORM VARYING and TIMES, STRING's POINTER, DISPLAY;
- uses of the bytes: a MOVE from an alphanumeric item or from a field
  of a record that was filled with spaces, a MOVE to a group, a
  figurative constant or ALL literal moved in, a relation with a
  nonnumeric literal, a class test, reference modification, STRING and
  UNSTRING and INSPECT of the item, its LENGTH, INITIALIZE of the group
  over it, a MOVE of that group.

After every few statements every item is displayed, and the groups and
the alphanumeric items as they are.  The same compiler with
-fno-native-items is the oracle: the two programs print the same bytes.
"""
import random
import sys


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 70
    r = random.Random(seed * 7919 + 3)
    out = []
    w = out.append

    # ---- the items ------------------------------------------------------
    items = []      # (name, digits, signed, usage)
    data = []

    def pic(d, sg, us):
        return "pic %s9(%d)%s" % ("s" if sg else "", d, " comp" if us == "comp" else "")

    def new_item(prefix, d=None, us=None, sg=None):
        n = "%s%d" % (prefix, len(items))
        d = d or r.choice([1, 1, 2, 2, 3, 4, 4, 5, 6, 7, 8, 9, 9])
        us = us or r.choice(["display", "display", "display", "comp"])
        if sg is None:
            sg = us == "comp" and r.random() < 0.5
        items.append((n, d, sg, us))
        return n, d, sg, us

    def value(d, sg, us="comp"):
        c = r.randrange(5)
        if c == 0:
            return ""
        if c == 1:
            return " value zero"
        v = r.randrange(10 ** d)
        if sg and r.random() < 0.4:
            return " value -%d" % v
        return " value %d" % v

    def cond88(name, d):
        k = r.randrange(4)
        if k == 0:
            return ["    88 %s-a value %d." % (name, r.randrange(10 ** d))]
        if k == 1:
            lo = r.randrange(10 ** d); hi = min(10 ** d - 1, lo + r.randrange(1, 9))
            return ["    88 %s-a value %d thru %d." % (name, lo, hi)]
        if k == 2:
            return ["    88 %s-a value zero." % name, "    88 %s-b value %d %d." % (name, r.randrange(10 ** d), r.randrange(10 ** d))]
        return []

    conds = []
    # alone, at 77 and 01
    for _ in range(r.randint(5, 8)):
        n, d, sg, us = new_item("w")
        lv = r.choice(["77", "01"])
        data.append("%s %s %s%s." % (lv, n, pic(d, sg, us), value(d, sg, us)))
        if r.random() < 0.35:
            c = cond88(n, d)
            data.extend(c)
            conds.extend(x.split()[1] for x in c)
    for d in (4, 2):
        if r.random() < 0.6:
            n, d, sg, us = new_item("w", d=d, us="display", sg=False)
            data.append("77 %s %s%s." % (n, pic(d, sg, us), value(d, sg, us)))
    # pairs of one description (moved and compared byte for byte)
    for _ in range(r.randint(1, 2)):
        n, d, sg, us = new_item("w", us="display", sg=False)
        data.append("77 %s %s%s." % (n, pic(d, sg, us), value(d, sg)))
        n2, _, _, _ = new_item("w", d=d, us="display", sg=False)
        data.append("77 %s %s%s." % (n2, pic(d, sg, us), value(d, sg)))
    # groups: named by a statement or not
    groups = []     # (name, [members], named)
    for g in range(r.randint(2, 3)):
        gn = "g%d" % g
        named = r.random() < 0.5
        gval = r.random() < 0.3
        mem = []
        lines = []
        size = 0
        for _ in range(r.randint(2, 4)):
            n, d, sg, us = new_item("m")
            lines.append("    05 %s %s%s." % (n, pic(d, sg, us), "" if gval else value(d, sg)))
            mem.append(n)
            size += d if us == "display" else (2 if d <= 4 else 4)
        if gval:
            data.append("01 %s value all \"%s\"." % (gn, r.choice("0379 "))) if all(i[3] == "display" for i in items[-len(mem):]) else data.append("01 %s." % gn)
        else:
            data.append("01 %s." % gn)
        data.extend(lines)
        groups.append((gn, mem, named, size))
    # a redefinition: characters over a number, or a number over characters
    rd = []
    for k in range(r.randint(1, 2)):
        n, d, sg, us = new_item("r", us="display", sg=False)
        used = r.random() < 0.5
        c = r.randrange(3)
        if c == 0:
            data.append("01 %s %s%s." % (n, pic(d, sg, us), value(d, sg)))
            data.append("01 %sx redefines %s pic x(%d)." % (n, n, d))

        else:
            data.append("01 %sx pic x(%d)%s." % (n, d, r.choice(["", " value all \"1\""])))
            data.append("01 %s redefines %sx %s." % (n, n, pic(d, sg, us)))
        rd.append((n, d, used))
    # the others: a dirty record, alphanumeric and edited items, a table
    data.append("01 dirty.")
    data.append("    05 d1 pic 9(4).")
    data.append("    05 d2 pic 9(2).")
    data.append("    05 d3 pic x(3).")
    data.append("01 an1 pic x(12).")
    data.append("01 an2 pic x(4) value \"0042\".")
    data.append("01 an3 pic x(6) value \" 12 4 \".")
    data.append("01 ed1 pic zzz,zzz,zz9.")
    data.append("01 ed2 pic -(9)9.")
    data.append("01 buf pic x(30).")
    data.append("01 tbl.")
    data.append("    05 te pic 9(3) occurs 9.")
    data.append("77 tix pic 9(2) comp.")
    data.append("77 guard pic 9(4) comp.")

    names = [i[0] for i in items]
    info = dict((i[0], i) for i in items)
    disp_unsigned = [i[0] for i in items if i[3] == "display" and not i[2]]

    def any_item():
        return r.choice(names)

    def lit(d, signed=False):
        v = r.randrange(10 ** r.randint(1, min(d + 1, 9)))
        if signed and r.random() < 0.3:
            return "-%d" % v
        return "%d" % v

    def show():
        w("    display \"--\"")
        for i in range(0, len(names), 6):
            w("    display " + " \" \" ".join(names[i:i + 6]))
        for gn, mem, named, size in groups:
            if named:
                w("    display \"[\" %s \"]\"" % gn)
        for n, d, used in rd:
            if used:
                w("    display \"<\" %sx \">\"" % n)
        w("    display \"[\" dirty \"] [\" an1 \"] [\" an2 \"] [\" ed1 \"] [\" ed2 \"] [\" buf \"]\"")

    def stmt(ind):
        p = " " * ind
        k = r.randrange(46)
        a = any_item(); b = any_item()
        da = info[a][1]; db = info[b][1]
        if k < 5:
            w("%smove %s to %s" % (p, lit(da, info[a][2]), a))
        elif k < 9:
            w("%smove %s to %s" % (p, a, b))
        elif k < 12:
            op = r.choice(["add %s to %s", "subtract %s from %s", "multiply %s by %s"])
            src = r.choice([lit(db), a])
            se = r.random() < 0.3
            w(p + op % (src, b) + (r.choice(["", " rounded"])))
            if se:
                w("%s    on size error display \"se %s\"" % (p, b))
                w("%send-%s" % (p, op.split()[0]))
        elif k < 14:
            c = any_item()
            w("%scompute %s%s = %s %s %s %s %s" % (p, c, r.choice(["", " rounded"]), a, r.choice(["+", "-", "*"]), b, r.choice(["+", "-"]), lit(3)))
        elif k < 16:
            c = any_item(); q = any_item()
            w("%sif %s not = zero" % (p, b))
            w("%s    divide %s into %s giving %s remainder %s" % (p, b, a, c, q))
            w("%send-if" % p)
        elif k < 20:
            rel = r.choice(["=", "<", ">", "not =", ">=", "<="])
            rhs = r.choice([b, lit(da), "zero"])
            w("%sif %s %s %s" % (p, a, rel, rhs))
            w("%s    display \"t %s\"" % (p, a))
            if r.random() < 0.5:
                stmt(ind + 4)
            w("%selse" % p)
            w("%s    display \"f\"" % p)
            w("%send-if" % p)
        elif k < 22 and conds:
            c = r.choice(conds)
            if r.random() < 0.6:
                w("%sif %s display \"88 %s\" end-if" % (p, c, c))
            else:
                w("%sset %s to true" % (p, c))
        elif k < 24:
            w("%sperform varying %s from 1 by 1 until %s > 3" % (p, a, a) if da >= 1 else p + "continue")
            w("%s    add 1 to guard" % p)
            w("%s    add %s to te(%s)" % (p, a, a))
            w("%send-perform" % p)
        elif k < 25:
            w("%smove %d to tix" % (p, r.randint(1, 9)))
            w("%smove %s to te(tix)" % (p, a))
            w("%sadd te(tix) to %s" % (p, b))
        elif k < 26:
            w("%sif %s > 0 and %s < 10" % (p, a, a))
            w("%s    move %s to te(%s)" % (p, lit(3), a))
            w("%s    display \"te \" te(%s)" % (p, a))
            w("%send-if" % p)
        elif k < 28:
            w("%smove %s to %s" % (p, a, r.choice(["an1", "ed1", "ed2", "buf"])))
        elif k < 30:
            w("%smove %s to %s" % (p, r.choice(["an2", "an3", "d1", "d2", "d3"]), a))
        elif k < 31 and r.random() < 0.35:
            # an item's bytes, as they are, to a group and from one
            c = r.randrange(3)
            if c == 0:
                w("%smove %s to dirty" % (p, a))
            elif c == 1:
                w("%smove \"%s\" to dirty" % (p, "".join(r.choice("0123456789") for _ in range(9))))
                w("%smove dirty to %s" % (p, a))
            else:
                w("%smove %s to buf(1:9)" % (p, a)) if info[a][3] == "display" else w("%smove %s to dirty" % (p, a))
        elif k < 31:
            w("%smove spaces to dirty" % p)
            x4 = [i[0] for i in items if i[1] == 4 and i[3] == "display" and not i[2]]
            x2 = [i[0] for i in items if i[1] == 2 and i[3] == "display" and not i[2]]
            for xs, f in ((x4, "d1"), (x2, "d2")):
                if xs and r.random() < 0.7:
                    x = r.choice(xs)
                    w("%smove 0 to %s" % (p, x))
                    w("%sif %s %s %s display \"%s eq %s\" else display \"%s ne %s\" end-if" % (p, x, r.choice(["=", "=", ">", "<", "not <"]), f, x, f, x, f))
                    if r.random() < 0.5:
                        w("%smove %s to %s" % (p, f, x))
                        w("%sdisplay \"<\" %s \">\"" % (p, x))
        elif k < 32:
            w("%smove %s to d1" % (p, a))
            w("%smove %s to d2" % (p, b))
        elif k < 33:
            gn, mem, named, size = r.choice(groups)
            if named:
                c = r.randrange(4)
                if c == 0:
                    w("%sinitialize %s" % (p, gn))
                elif c == 1:
                    w("%smove %s to buf" % (p, gn))
                elif c == 2:
                    w("%smove all \"3\" to %s" % (p, gn)) if all(info[m][3] == "display" for m in mem) else w("%sinitialize %s" % (p, gn))
                else:
                    w("%sif %s = spaces display \"sp\" end-if" % (p, gn))
        elif k < 34:
            n, d, used = r.choice(rd)
            if used:
                w("%smove %s to %sx" % (p, r.choice(["\"%s\"" % ("7" * d), "all \"5\"", "an2"]), n))
        elif k < 35 and disp_unsigned:
            x = r.choice(disp_unsigned)
            c = r.randrange(5)
            if c == 0:
                w("%smove all \"%d\" to %s" % (p, r.randrange(10), x))
            elif c == 1:
                w("%sif %s = \"%s\" display \"alnum eq\" end-if" % (p, x, "0" * info[x][1]))
            elif c == 2:
                w("%sif %s is numeric display \"num %s\" end-if" % (p, x, x))
            elif c == 3:
                w("%smove %s(1:1) to buf(3:1)" % (p, x))
            else:
                w("%smove zeros to %s" % (p, x))
        elif k < 36 and disp_unsigned:
            x = r.choice(disp_unsigned)
            c = r.randrange(4)
            if c == 0:
                w("%smove 1 to tix" % p)
                w("%sstring \"v=\" %s delimited by size into buf with pointer tix" % (p, x))
            elif c == 1:
                w("%sunstring an2 into %s" % (p, x))
            elif c == 2:
                w("%sinspect %s replacing all \"0\" by \"9\"" % (p, x))
            else:
                w("%smove function length(%s) to tix" % (p, x))
                w("%sdisplay \"len \" tix" % p)
        elif k < 37:
            w("%smove 1 to %s" % (p, a))
            w("%smove spaces to buf" % p)
            w("%sstring \"abc\" \"de\" delimited by size into buf with pointer %s" % (p, a)) if da >= 2 else w("%scontinue" % p)
        elif k < 38:
            w("%smove 0 to %s" % (p, a))
            w("%sinspect an3 tallying %s for all \" \"" % (p, a))
        elif k < 39:
            w("%sinitialize %s" % (p, a))
        elif k < 41:
            w("%sdisplay \"%s=\" %s \" %s=\" %s" % (p, a, a, b, b))
        elif k < 43:
            w("%sperform %d times" % (p, r.randint(1, 3)))
            stmt(ind + 4)
            w("%send-perform" % p)
        elif k < 44:
            w("%smove %s to %s %s" % (p, a, b, any_item()))
        else:
            w("%sadd %s %s giving %s" % (p, a, lit(db), b))

    w("identification division.")
    w("program-id. gennative.")
    w("data division.")
    w("working-storage section.")
    for l in data:
        w(l)
    w("procedure division.")
    w("main-para.")
    show()
    for n in range(nstmt):
        stmt(4)
        if n % 5 == 4:
            show()
    show()
    w("    display \"guard \" guard")
    w("    stop run.")
    print("\n".join(out))


main()
