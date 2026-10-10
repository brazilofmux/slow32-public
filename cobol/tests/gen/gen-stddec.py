#!/usr/bin/env python3
"""gen-stddec.py SEED [N] -- a COBOL program whose arithmetic is ARITHMETIC
IS STANDARD-DECIMAL (2014; 2023 8.8.1.5, 11.9.5, 11.9.11), on stdout, and
the reference lines it must print, on stderr.

The witness is Python's decimal module: a context of precision 34 with the
unit's INTERMEDIATE ROUNDING mode (NEAREST-AWAY-FROM-ZERO implied,
NEAREST-EVEN, TRUNCATION, or PROHIBITED), Emax 6144 and Emin -6143, is
decimal128 arithmetic as ISO/IEC 60559 defines it and as the standard's
SDIDI is defined to behave.  Every operand is converted exactly (fixed-
point items and literals are exact decimals), every operation is rounded
once to 34 digits; exponentiation follows 8.8.1.5.4's sequences (x, x*x,
(x*x)*x, (x*x)*(x*x)); a quotient's divisor of zero, 0 ** 0, overflow,
and under PROHIBITED any inexact intermediate, are the size error
condition.  A receiver takes the SDIDI's value truncated to its scale
(ROUNDED: nearest away from zero) and a value past its integer digits is
the size error; an unsigned item takes the absolute value; a negative value
that comes to zero in the receiver keeps its sign (as the stores here do).
Comparisons compare the two SDIDIs.  No oracle: GnuCOBOL marks the clause
not implemented, gcobol takes it as NATIVE.
"""
import random
import sys
from decimal import Decimal, Context, ROUND_HALF_UP, ROUND_HALF_EVEN, ROUND_DOWN, Inexact, DivisionByZero, InvalidOperation, Overflow, Underflow, localcontext

seed = int(sys.argv[1])
nstmt = int(sys.argv[2]) if len(sys.argv) > 2 else 40
r = random.Random(seed * 1000003 + 11)

MODES = [("", ROUND_HALF_UP), ("nearest-away-from-zero", ROUND_HALF_UP), ("nearest-even", ROUND_HALF_EVEN),
         ("truncation", ROUND_DOWN), ("prohibited", ROUND_HALF_UP)]
mode_word, rounding = MODES[seed % len(MODES)]
prohibited = mode_word == "prohibited"
SD = Context(prec=34, rounding=rounding, Emax=6144, Emin=-6143, traps=[])
BIG = Context(prec=400, rounding=ROUND_DOWN, Emax=99999, Emin=-99999, traps=[])

# the items: (name, signed, integer digits, decimals, value)
items = []
def pic(signed, ni, nd):
    return ("s" if signed else "") + "9(%d)" % ni + ("v9(%d)" % nd if nd else "")
def rand_value(signed, ni, nd):
    digs = r.randint(1, ni + nd)
    s = "".join(r.choice("0123456789") for _ in range(digs))
    v = Decimal(s).scaleb(-nd) if nd else Decimal(s)
    v = BIG.quantize(v, Decimal(1).scaleb(-nd)) if nd else v
    if signed and r.random() < 0.4: v = -v
    return v
for i in range(6):
    signed = r.random() < 0.5
    ni = r.randint(1, 18); nd = r.randint(0, min(13, 31 - ni))
    items.append(("i%d" % i, signed, ni, nd, rand_value(signed, ni, nd)))

def lit_text(v):
    s = format(v, "f")
    return s
def literal():
    kind = r.random()
    if kind < 0.3: v = Decimal(r.randint(0, 999))
    elif kind < 0.6: v = Decimal(r.randint(1, 99999)).scaleb(-r.randint(1, 6))
    elif kind < 0.75: v = Decimal(10) ** r.randint(10, 22)
    elif kind < 0.9: v = Decimal(r.randint(1, 9)).scaleb(-r.randint(7, 20))
    else: v = Decimal("".join(r.choice("123456789") for _ in range(r.randint(19, 31))))
    if r.random() < 0.3: v = -v
    return v

class Err(Exception): pass

def expr(depth):
    """(text, value, bad): the statement's text is always complete; bad says
    the size error condition arose somewhere in it (a divisor of zero,
    0 ** 0); the value is then a placeholder"""
    k = r.random()
    if depth <= 0 or k < 0.3:
        if r.random() < 0.5:
            it = r.choice(items)
            return it[0], it[4], False
        v = literal()
        return ("(%s)" % lit_text(v) if v < 0 else lit_text(v)), v, False
    if k < 0.38:
        t, v, bad = expr(depth - 1)
        return "( - ( %s ) )" % t, SD.minus(v), bad      # unary minus binds before ** (8.8.1.2 rule 2): its operand parenthesized
    if k < 0.5:
        t, v, bad = expr(depth - 1)
        e = r.choice([0, 1, 2, 2, 3, 4])
        if e == 0:
            if v == 0: bad = True
            res = Decimal(1)
        elif e == 1: res = v
        elif e == 2: res = SD.multiply(v, v)
        elif e == 3: res = SD.multiply(SD.multiply(v, v), v)
        else:
            sq = SD.multiply(v, v); res = SD.multiply(sq, sq)
        return "( %s ) ** %d" % (t, e), res, bad
    op = r.choice(["+", "-", "*", "/"])
    ta, va, ba = expr(depth - 1); tb, vb, bb = expr(depth - 1)
    bad = ba or bb
    if op == "+": res = SD.add(va, vb)
    elif op == "-": res = SD.subtract(va, vb)
    elif op == "*": res = SD.multiply(va, vb)
    else:
        if vb == 0: bad = True; res = Decimal(0)
        else: res = SD.divide(va, vb)
    return "( %s %s %s )" % (ta, op, tb), res, bad

def show(v, signed, ni, nd):
    q = BIG.quantize(abs(v), Decimal(1).scaleb(-nd)) if nd else BIG.quantize(abs(v), Decimal(1))
    s = format(q, "f")
    ip, _, fp = s.partition(".")
    ip = ip.rjust(ni, "0")
    out = ip + ("." + fp.ljust(nd, "0") if nd else "")
    if signed: out = ("-" if v < 0 else "+") + out
    return out

out = []
refs = []
def w(s): out.append(s)
w("identification division.")
w("program-id. gsd%d." % seed)
w("options.")
w("    arithmetic is standard-decimal" + (("\n    intermediate rounding is " + mode_word) if mode_word else "") + ".")
w("data division.")
w("working-storage section.")
for name, signed, ni, nd, v in items:
    w("01 %s pic %s value %s." % (name, pic(signed, ni, nd), lit_text(v)))
recv = []
for i in range(4):
    signed = r.random() < 0.6
    ni = r.randint(1, 20); nd = r.randint(0, min(14, 31 - ni))
    recv.append(("r%d" % i, signed, ni, nd))
    w("01 r%d pic %s." % (i, pic(signed, ni, nd)))
w("procedure division.")
k = 0
cur = {}
for name, signed, ni, nd in recv: cur[name] = Decimal(0)
while k < nstmt:
    k += 1
    kind = r.random()
    if kind < 0.8:
        name, signed, ni, nd = r.choice(recv)
        rounded = r.random() < 0.35
        SD.clear_flags()
        t, v, bad = expr(r.randint(1, 3))
        try:
            if bad: raise Err
            if SD.flags[Overflow] or SD.flags[InvalidOperation] or SD.flags[DivisionByZero]: raise Err
            if prohibited and SD.flags[Inexact]: raise Err
            if SD.flags[Underflow] and v == 0: raise Err
            # the store
            with localcontext(BIG) as c:
                c.rounding = ROUND_HALF_UP if rounded else ROUND_DOWN
                q = abs(v).quantize(Decimal(1).scaleb(-nd)) if nd else abs(v).quantize(Decimal(1))
            if q >= Decimal(10) ** ni: raise Err
            stored = -q if v < 0 else q
            cur[name] = stored
            refs.append("%d [%s]" % (k, show(stored, signed, ni, nd)))
        except Err:
            refs.append("%d size error" % k)
        w('    compute %s%s = %s' % (name, " rounded" if rounded else "", t))
        w('        on size error display "%d size error"' % k)
        w('        not on size error display "%d [" %s "]"' % (k, name))
        w('    end-compute')
    else:
        # a comparison of two expressions without division or power (no size error possible)
        def simple(depth):
            if depth <= 0 or r.random() < 0.4:
                if r.random() < 0.6:
                    it = r.choice(items); return it[0], it[4]
                v = literal(); return ("(%s)" % lit_text(v) if v < 0 else lit_text(v)), v
            op = r.choice(["+", "-", "*"])
            ta, va = simple(depth - 1); tb, vb = simple(depth - 1)
            res = SD.add(va, vb) if op == "+" else SD.subtract(va, vb) if op == "-" else SD.multiply(va, vb)
            return "( %s %s %s )" % (ta, op, tb), res
        SD.clear_flags()
        ta, va = simple(2); tb, vb = simple(2)
        if not any(("i%d" % i) in ta or ("i%d" % i) in tb for i in range(6)):
            it = r.choice(items); ta = "( %s + %s )" % (it[0], ta); va = SD.add(it[4], va)   # a relation needs an identifier (8.8.4.1.1)
        if SD.flags[Overflow]: k -= 1; continue
        rel = r.choice(["<", ">", "=", "<=", ">=", "not ="])
        c = SD.compare(va, vb)
        truth = {"<": c < 0, ">": c > 0, "=": c == 0, "<=": c <= 0, ">=": c >= 0, "not =": c != 0}[rel]
        w('    if %s %s %s display "%d true" else display "%d false" end-if' % (ta, rel, tb, k, k))
        refs.append("%d %s" % (k, "true" if truth else "false"))
w("    stop run.")
sys.stdout.write("\n".join(out) + "\n")
sys.stderr.write("\n".join(refs) + "\n")
