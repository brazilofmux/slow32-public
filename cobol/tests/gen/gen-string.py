#!/usr/bin/env python3
"""Generate a random COBOL 85 program of STRING, UNSTRING and INSPECT
statements, for differential testing against GnuCOBOL
(GEN=string tests/gen/run-gen.sh ...).

    gen-string.py SEED [STATEMENTS] > prog.cbl

Subjects and literals come from a small alphabet so that delimiters and
patterns match.  Every receiving item is given known content first, and
every result is DISPLAYed: receivers between brackets, pointers, counts,
tallies, and which overflow branch ran.
- STRING: several sources DELIMITED BY SIZE, a literal or an item, INTO
  a receiver WITH POINTER (sometimes starting out of range), ON OVERFLOW
  and NOT ON OVERFLOW.
- UNSTRING: DELIMITED BY [ALL] d [OR [ALL] d], into several receivers,
  some with DELIMITER IN and COUNT IN, WITH POINTER, TALLYING IN, ON
  OVERFLOW and NOT ON OVERFLOW.
- INSPECT: TALLYING (CHARACTERS, ALL, LEADING), REPLACING (CHARACTERS BY,
  ALL, LEADING, FIRST), several phrases in one statement, BEFORE and AFTER
  INITIAL; and CONVERTING (each character of its first operand once, as
  the 85 text requires).
"""
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from inspect85 import inspect, converting   # the 85 rules, the INSPECT oracle

ALPHA = "ABab 1,"


def text(r, lo, hi):
    return "".join(r.choice(ALPHA) for _ in range(r.randint(lo, hi)))


def lit(s):
    return '"' + s + '"'


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    r = random.Random(seed * 15485863 + 11)
    out = []
    w = out.append
    w("identification division.")
    w("program-id. gst%d." % seed)
    w("data division.")
    w("working-storage section.")
    slen = [r.randint(4, 16) for _ in range(4)]
    for i in range(4):
        w("01 S%d pic x(%d)." % (i, slen[i]))
    refs = []      # (label, expected line) for INSPECT, from inspect85
    for i in range(4):
        w("01 R%d pic x(%d)." % (i, r.randint(1, 8)))
    w("01 D0 pic x(2).")
    w("01 DL0 pic x(2).")
    w("01 DL1 pic x(2).")
    w("01 C0 pic 99.")
    w("01 C1 pic 99.")
    w("01 P pic 99.")
    w("01 T pic 99.")
    w("01 K pic 9(4).")
    w("01 K2 pic 9(4).")
    w("01 OV pic x.")
    w("procedure division.")
    for k in range(nstmt):
        kind = r.choice(["string", "unstring", "unstring", "tally", "replace", "both", "convert"])
        w("*> %d %s" % (k, kind))
        if kind == "string":
            n = r.randint(1, 3)
            srcs = []
            for i in range(n):
                w("    move %s to S%d" % (lit(text(r, 1, 10)), i))
                d = r.choice(["size", lit(r.choice(ALPHA)), lit(text(r, 2, 2)), "D0"])
                srcs.append((r.choice(["S%d" % i, lit(text(r, 1, 5))]), d))
            w("    move %s to D0" % lit(text(r, 1, 2)))
            w("    move %s to R0" % lit(text(r, 1, 8)))
            w("    move %d to P" % r.choice([1, 1, 2, 3, 5, 0, 9]))
            w('    move "-" to OV')
            w("    string")
            for s, d in srcs:
                w("        %s delimited by %s" % (s, d))
            w("        into R0 with pointer P")
            w('        on overflow move "O" to OV')
            w('        not on overflow move "N" to OV')
            w("    end-string")
            w('    display "%d [" R0 "] " P " " OV' % k)
        elif kind == "unstring":
            w("    move %s to S0" % lit(text(r, 0 if False else 1, 14)))
            d1 = r.choice(ALPHA)
            d2 = r.choice([None, r.choice(ALPHA), text(r, 2, 2)])
            delim = ("all " if r.random() < 0.4 else "") + lit(d1)
            if d2:
                delim += " or " + ("all " if r.random() < 0.4 else "") + lit(d2)
            nrec = r.randint(1, 3)
            for i in range(nrec):
                w('    move "%s" to R%d' % ("#" * 8, i))
            w('    move "##" to DL0  move "##" to DL1')
            w("    move 99 to C0  move 99 to C1")
            w("    move %d to P" % r.choice([1, 1, 1, 2, 4, 0, 30]))
            w("    move %d to T" % r.choice([0, 0, 3]))
            w('    move "-" to OV')
            w("    unstring S0 delimited by %s" % delim)
            into = []
            for i in range(nrec):
                ph = "R%d" % i
                if i < 2 and r.random() < 0.6:
                    ph += " delimiter in DL%d" % i
                if i < 2 and r.random() < 0.6:
                    ph += " count in C%d" % i
                into.append(ph)
            w("        into " + "\n             ".join(into))
            w("        with pointer P tallying in T")
            w('        on overflow move "O" to OV')
            w('        not on overflow move "N" to OV')
            w("    end-unstring")
            shown = " ".join('"[" R%d "]"' % i for i in range(nrec))
            w('    display "%d " %s' % (k, shown))
            w('    display "%d  " DL0 " " DL1 " " C0 " " C1 " " P " " T " " OV' % (k))
        elif kind in ("tally", "replace", "both"):
            subj = text(r, 1, 14)
            w("    move %s to S1" % lit(subj))
            w("    move 0 to K K2")
            phr = []
            subject = subj[:slen[1]].ljust(slen[1])

            def where():
                if r.random() < 0.4:
                    return (r.choice(["before", "after"]), r.choice(ALPHA))
                return None

            def wtxt(wh):
                return " %s initial %s" % (wh[0], lit(wh[1])) if wh else ""
            k1 = k2 = 0
            if kind in ("tally", "both"):
                t = []
                for _ in range(r.randint(1, 3)):
                    f = r.choice(["characters", "all", "leading"])
                    t.append((f, None if f == "characters" else text(r, 1, 2), None, where()))
                # CHARACTERS last: the oracle refuses an ALL or LEADING group
                # after one, though the 85 format allows any order
                t.sort(key=lambda x: x[0] == "characters")
                tx = " ".join(("characters" if f == "characters" else "%s %s" % (f, lit(l))) + wtxt(wh)
                              for f, l, _, wh in t)
                phrases = list(t)
                tally = "tallying K for " + tx
                if r.random() < 0.3:
                    c2 = r.choice(ALPHA)
                    tally += " K2 for all %s" % lit(c2)
                    phrases.append(("all", c2, None, None))
                tl, _ = inspect(subject, phrases, False)
                k1 = sum(tl[:len(t)])
                k2 = sum(tl[len(t):])
                phr.append(tally)
            result = subject
            if kind in ("replace", "both"):
                rp = []
                for _ in range(r.randint(1, 3)):
                    f = r.choice(["characters", "all", "leading", "first"])
                    if f == "characters":
                        rp.append((f, None, r.choice(ALPHA), where()))
                    else:
                        a = text(r, 1, 2)
                        rp.append((f, a, text(r, len(a), len(a)), where()))
                rp.sort(key=lambda x: x[0] == "characters")
                rx = " ".join(("characters by %s" % lit(rr) if f == "characters" else
                               "%s %s by %s" % (f, lit(l), lit(rr))) + wtxt(wh) for f, l, rr, wh in rp)
                _, result = inspect(subject, rp, True)
                phr.append("replacing " + rx)
            w("    inspect S1 " + "\n        ".join(phr))
            w('    display "%d [" S1 "] " K " " K2' % k)
            refs.append((k, "%d [%s] %04d %04d" % (k, result, k1, k2)))
        else:
            subj = text(r, 1, 14)
            w("    move %s to S2" % lit(subj))
            frm = "".join(r.sample(ALPHA, r.randint(1, 4)))
            to = "".join(r.choice("XYZ*.") for _ in frm)
            wh = (r.choice(["before", "after"]), r.choice(ALPHA)) if r.random() < 0.4 else None
            w("    inspect S2 converting %s to %s%s" % (lit(frm), lit(to),
              (" %s initial %s" % (wh[0], lit(wh[1]))) if wh else ""))
            w('    display "%d [" S2 "]"' % k)
            subject = subj[:slen[2]].ljust(slen[2])
            refs.append((k, "%d [%s]" % (k, converting(subject, frm, to, wh))))
    w("    stop run.")
    print("\n".join(out))
    for _, line in refs:                     # the reference's lines, for run-gen.sh
        print(line, file=sys.stderr)


if __name__ == "__main__":
    main()
