#!/usr/bin/env python3
"""Generate a random COBOL 85 program of table handling, for differential
testing against GnuCOBOL (GEN=table tests/gen/run-gen.sh ...).

    gen-table.py SEED [STATEMENTS] > prog.cbl

One keyed table, T, of ten entries (key TK ascending and unique, value
TV), indexed by TX and TY; one variable-length group, G, whose table GE
OCCURS 1 TO 8 TIMES DEPENDING ON GN, an item outside the group (the
rules with the object inside are left alone); and a second such group,
H, depending on HN.  Statements, each DISPLAYing what it produced:
- subscripted and indexed references, with relative subscripts
  (TV(SB + 2)) and relative indexing (TV(TX - 1)), always in range;
- SET index TO, UP BY, DOWN BY, and an index converted to an integer;
- SEARCH from a set starting index (a SEARCH starts there, not at 1),
  with AT END and one to three WHEN phrases;
- SEARCH ALL on the ascending key, finding and missing;
- MOVEs from and to the variable-length groups at several counts, and
  between them.
"""
import random
import sys


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 50
    r = random.Random(seed * 32452843 + 13)
    keys = sorted(r.sample(range(1, 999), 10))
    vals = ["".join(r.choice("ABCXYZ") for _ in range(3)) for _ in range(10)]
    out = []
    w = out.append
    w("identification division.")
    w("program-id. gtb%d." % seed)
    w("data division.")
    w("working-storage section.")
    w("01 TBL.")
    w("   05 T occurs 10 times ascending key TK indexed by TX TY.")
    w("      10 TK pic 999.")
    w("      10 TV pic x(3).")
    w("01 GN pic 99.")
    w("01 G.")
    w("   05 GH pic x(2).")
    w("   05 GE occurs 1 to 8 times depending on GN pic x(2).")
    w("01 HN pic 99.")
    w("01 H.")
    w("   05 HH pic x(2).")
    w("   05 HE occurs 1 to 8 times depending on HN pic x(2).")
    w("01 W pic x(20).")
    w("01 SB pic 99.")
    w("01 IX pic 99.")
    w("01 FOUND pic x(5).")
    w("procedure division.")
    for i in range(10):
        w("    move %d to TK(%d)  move \"%s\" to TV(%d)" % (keys[i], i + 1, vals[i], i + 1))
    for k in range(nstmt):
        kind = r.choice(["ref", "ref", "set", "search", "search", "all", "all", "odo", "odo"])
        w("*> %d %s" % (k, kind))
        if kind == "ref":
            a = r.randint(1, 10)
            if r.random() < 0.5:
                d = r.randint(0, 10 - a) if r.random() < 0.5 else -r.randint(0, a - 1)
                w("    move %d to SB" % a)
                ref = "TV(SB %s %d)" % ("+" if d >= 0 else "-", abs(d))
            else:
                d = r.randint(0, 10 - a) if r.random() < 0.5 else -r.randint(0, a - 1)
                w("    set TX to %d" % a)
                ref = "TV(TX %s %d)" % ("+" if d >= 0 else "-", abs(d))
            w('    display "%d " %s " " TK(%d)' % (k, ref, r.randint(1, 10)))
        elif kind == "set":
            a = r.randint(1, 10)
            w("    set TX to %d" % a)
            m = r.randint(0, 9)
            if r.random() < 0.5 and a + m <= 10:
                w("    set TX up by %d" % m)
            elif a - m >= 1:
                w("    set TX down by %d" % m)
            w("    set TY to TX")
            w("    set IX to TY")
            w('    display "%d " IX " " TV(TX) " " TV(TY)' % k)
        elif kind == "search":
            start = r.randint(1, 10)
            w("    set TX to %d" % start)
            w('    move "none" to FOUND')
            w("    search T")
            w('        at end move "end" to FOUND')
            for _ in range(r.randint(1, 3)):
                c = r.choice(["key", "val", "keygt"])
                if c == "key":
                    w('        when TK(TX) = %d move "key" to FOUND' % r.choice(keys + [r.randint(1, 999)]))
                elif c == "val":
                    w('        when TV(TX) = "%s" move "val" to FOUND' % r.choice(vals + ["QQQ"]))
                else:
                    w('        when TK(TX) > %d move "gt" to FOUND' % r.randint(1, 999))
            w("    end-search")
            w("    set IX to TX")
            w('    display "%d " FOUND " " IX' % k)
        elif kind == "all":
            target = r.choice(keys + [r.randint(1, 999)])
            w('    move "none" to FOUND')
            w("    search all T")
            w('        at end move "end" to FOUND')
            w('        when TK(TX) = %d move "hit" to FOUND' % target)
            w("    end-search")
            w('    if FOUND = "hit" set IX to TX else move 0 to IX end-if')
            w('    display "%d " FOUND " " IX' % k)
        else:
            gn = r.randint(1, 8)
            w("    move %d to GN" % gn)
            op = r.choice(["fill", "out", "in", "between"])
            if op == "fill":
                w('    move "%s" to G' % "".join(r.choice("abcdef12") for _ in range(r.randint(1, 18))))
                w('    display "%d [" G "]"' % k)
            elif op == "out":
                w('    move "%s" to G' % "".join(r.choice("abcdef12") for _ in range(18)))
                w('    move all "." to W')
                w("    move G to W")
                w('    display "%d [" W "]"' % k)
            elif op == "in":
                # the whole group first (working storage without a VALUE is
                # undefined), then a move at the current count, which must
                # leave the rest alone
                w("    move 8 to GN")
                w('    move all "-" to G')
                w("    move %d to GN" % gn)
                w('    move "%s" to G' % "".join(r.choice("abcdef12") for _ in range(r.randint(1, 18))))
                w("    move 8 to GN")
                w('    display "%d [" G "]"' % k)
            else:
                hn = r.randint(1, 8)
                w('    move "%s" to G' % "".join(r.choice("abcdef12") for _ in range(18)))
                w("    move 8 to HN")
                w('    move all "=" to H')
                w("    move %d to HN" % hn)
                w("    move G to H")
                w("    move 8 to HN")
                w('    display "%d [" H "]"' % k)
    w("    stop run.")
    print("\n".join(out))


if __name__ == "__main__":
    main()
