#!/usr/bin/env python3
"""Generate a random COBOL 85 program of MOVEs into edited items, for
differential testing against GnuCOBOL (tests/gen/run-gen.sh edit ...).

    gen-edit.py SEED [STATEMENTS] > prog.cbl

The pictures are built from the structure of a numeric-edited picture,
so each is valid by construction: an optional fixed leading sign or
currency symbol; an integer part plain, zero-suppressed (Z),
check-protected (*) or a floating insertion string (+ - $), with simple
insertion characters (B 0 , /) among its positions; an optional point
and fraction (Z there only when every digit position is Z); an optional
trailing sign, CR or DB when no sign leads or floats.  Some items are
BLANK WHEN ZERO (never with *).  Alphanumeric-edited items take X with
B 0 / insertions.  Each MOVE's result is DISPLAYed between brackets, so
leading and trailing spaces count.
"""
import os
import random
import sys
from decimal import Decimal

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import arith85                      # a MOVE into the source item
from edit85 import edit_numeric, edit_alnum   # the 85 editing rules, the reference


def insert_simple(r, chars, frac=False):
    """Scatter B 0 , / among the digit positions (never first)."""
    out = [chars[0]]
    for c in chars[1:]:
        if r.random() < 0.15:
            out.append(r.choice(["B", "0", "/"] + ([] if frac else [","])))
        out.append(c)
    return out


def numeric_edited(r):
    """(picture, digits, decimals, blank_when_zero_allowed)"""
    lead = r.choice(["", "", "", "+", "-", "$"])
    mode = r.choice(["plain", "z", "star", "float"])
    if lead == "$":
        # the chart allows $Z, $** and $++, but GnuCOBOL refuses them
        # (docs/conformance/picture.md): a whole program would be lost
        mode = "plain"
    if mode == "float":
        fl = r.choice([c for c in "+-$" if c != lead and not (c in "+-" and lead in "+-")])
    nint = r.randint(1, 9)
    ndec = r.choice([0, 0, 1, 2, 2, 3, 4])
    if mode == "plain":
        ip = ["9"] * nint
    elif mode in ("z", "star"):
        sym = "Z" if mode == "z" else "*"
        k = r.randint(1, nint)
        ip = [sym] * k + ["9"] * (nint - k)
    else:
        k = r.randint(2, nint + 1)          # a floating string: one more symbol than digits it holds
        ip = [fl] * k + ["9"] * (nint - (k - 1))
    allz = mode in ("z", "star") and all(c != "9" for c in ip)
    ip = insert_simple(r, ip)
    pic = list(lead) + ip
    digits = sum(1 for c in ip if c in "9Z*") + (sum(1 for c in ip if c == fl) - 1 if mode == "float" else 0)
    if ndec:
        pic.append(".")
        if allz and r.random() < 0.5:
            fp = ["Z" if mode == "z" else "*"] * ndec
        else:
            fp = ["9"] * ndec
        pic += insert_simple(r, fp, frac=True) if r.random() < 0.3 else fp
    signed_already = lead in "+-" or (mode == "float" and fl in "+-")
    if not signed_already and r.random() < 0.5:
        pic += list(r.choice(["+", "-", "CR", "DB"]))
    s = "".join(pic)
    return s, digits, ndec, mode != "star"


def alnum_edited(r):
    n = r.randint(2, 10)
    return "".join(insert_simple(r, ["X"] * n, frac=True)), n   # B 0 / only


def numeric_source(r, name):
    signed = r.random() < 0.6
    ints = r.randint(0, 9)
    decs = r.randint(0, 4)
    if ints + decs == 0:
        ints = 1
    pic = ("S" if signed else "") + ("9(%d)" % ints if ints else "") + ("V9(%d)" % decs if decs else "")
    return name, pic, signed, ints, decs


def value(r, signed, ints, decs):
    if r.random() < 0.1:
        return "0"
    ip = r.randint(0, 10 ** ints - 1) if ints else 0
    s = str(ip)
    if decs:
        s += "." + str(r.randint(0, 10 ** decs - 1)).zfill(decs)
    if signed and r.random() < 0.45 and s.strip("0.") != "":
        s = "-" + s
    return s


def main():
    seed = int(sys.argv[1])
    nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 60
    r = random.Random(seed * 7919 + 1)
    srcs = [numeric_source(r, "S%02d" % i) for i in range(12)]
    out = []
    w = out.append
    w("identification division.")
    w("program-id. ged%d." % seed)
    w("data division.")
    w("working-storage section.")
    for name, pic, _, _, _ in srcs:
        w("01 %s pic %s." % (name, pic))
    w("01 ANS pic x(12).")
    stmts = []
    for k in range(nstmt):
        if r.random() < 0.2:
            pic, n = alnum_edited(r)
            w("01 E%03d pic %s." % (k, pic))
            txt = "".join(r.choice("ABCXYZ 12") for _ in range(r.randint(1, 12)))
            stmts.append(('    move "%s" to E%03d' % (txt, k), k, pic, edit_alnum(pic, txt)))
        else:
            pic, digits, decs, bwz_ok = numeric_edited(r)
            bwz = " blank when zero" if bwz_ok and r.random() < 0.1 else ""
            w("01 E%03d pic %s%s." % (k, pic, bwz))
            name, _, signed, ints, sdecs = r.choice(srcs)
            if r.random() < 0.3:
                lit = value(r, True, r.randint(0, 9), r.randint(0, 4))
                stmts.append(("    move %s to E%03d" % (lit, k), k, pic,
                              edit_numeric(pic, Decimal(lit), bool(bwz))))
            else:
                lit = value(r, signed, ints, sdecs)
                sv = arith85.move((signed, ints, sdecs), Decimal(lit))
                stmts.append(("    move %s to %s\n    move %s to E%03d"
                              % (lit, name, name, k), k, pic, edit_numeric(pic, sv, bool(bwz))))
    w("procedure division.")
    for st, k, pic, want in stmts:
        w(st)
        w('    display "%d %s [" E%03d "]"' % (k, pic, k))
    w("    stop run.")
    print("\n".join(out))
    for st, k, pic, want in stmts:           # the reference's lines, for run-gen.sh
        print("%d %s [%s]" % (k, pic, want), file=sys.stderr)


if __name__ == "__main__":
    main()
