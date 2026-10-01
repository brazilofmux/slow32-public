#!/usr/bin/env python3
"""Arithmetic as X3.23-1985 defines it, written out independently of
either compiler: a reference oracle for tests/gen (gen-arith.py).

An item is (signed, ints, decs).  Values are Decimal.

- Storing a result (VI-66 to VI-69: ROUNDED, SIZE ERROR, and the
  standard alignment rules of IV-16): aligned on the decimal point; with
  ROUNDED, rounded at the receiver's last digit, half away from zero;
  otherwise truncated.  A result whose magnitude does not fit the
  receiver's integer digits is a size error: with ON SIZE ERROR the
  receiver is unchanged.  An unsigned receiver keeps the magnitude.
- MOVE (VI-103): aligned on the decimal point, truncated at both ends,
  no size error; the magnitude into an unsigned item.
- DIVIDE ... REMAINDER (VI-80, VI-81, rules 6 to 8): the remainder is
  the dividend less the quotient times the divisor, the quotient being an
  intermediate field with the quotient item's digits, decimal point and
  presence or absence of a sign, truncated even when ROUNDED is
  specified; the remainder is truncated into its item.  A size error on
  the quotient leaves both receivers unchanged; one on the remainder
  leaves only the remainder unchanged.  A zero divisor is a size error.
"""
from decimal import Decimal, ROUND_DOWN, ROUND_HALF_UP, getcontext

getcontext().prec = 80


def quant(v, decs, rounded):
    q = Decimal(1).scaleb(-decs)
    return v.quantize(q, rounding=ROUND_HALF_UP if rounded else ROUND_DOWN)


def store(item, value, rounded):
    """(ok, stored): ok False on a size error"""
    signed, ints, decs = item
    v = quant(value, decs, rounded)
    if abs(v) >= Decimal(10) ** ints:
        return False, None
    return True, (v if signed else abs(v))


def move(item, value):
    signed, ints, decs = item
    v = quant(value, decs, False)
    sign = -1 if v < 0 else 1
    v = abs(v) % (Decimal(10) ** ints)          # the high-order digits beyond the item are lost
    v = quant(v, decs, False)
    return v * sign if signed else v


def divide_remainder(dividend, divisor, q_item, q_rounded, r_item):
    """(size_error, quotient or None, remainder or None); None: unchanged"""
    if divisor == 0:
        return True, None, None
    exact = dividend / divisor
    ok, q = store(q_item, exact, q_rounded)
    if not ok:
        return True, None, None
    signed, ints, decs = q_item
    qi = quant(exact, decs, False)               # truncated, even with ROUNDED
    if not signed:
        qi = abs(qi)                             # the intermediate field's sign is the quotient item's
    rem = dividend - qi * divisor
    rok, r = store(r_item, rem, False)
    if not rok:
        return True, q, None
    return False, q, r


def fmt(item, value):
    """the item as DISPLAY shows it: a sign if signed, the digits, a
    point where the PICTURE's V is"""
    signed, ints, decs = item
    v = quant(value, decs, False)
    sign = ("-" if v < 0 else "+") if signed else ""
    digits = str(abs(v).quantize(Decimal(1).scaleb(-decs))) if decs else str(int(abs(v)))
    ip, _, fp = digits.partition(".")
    ip = ip.zfill(ints)[-ints:] if ints else ""
    return sign + ip + ("." + fp.ljust(decs, "0")[:decs] if decs else "")
