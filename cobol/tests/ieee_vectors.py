#!/usr/bin/env python3
"""Reference vectors for libcob/ieee.h (tests/ieee_test.c): decimal values
and their IEEE encodings, computed with exact rational arithmetic.

    ieee_vectors.py > vectors.txt

Each line: kind digits scale neg hex, where the decimal is digits *
10^-scale (negated when neg), and hex is the 16- or 32-hex-digit
encoding (big-endian order of bytes):
  b128  binary128, round-to-nearest-even
  d64   decimal64 BID    d64d  decimal64 DPD
  d128  decimal128 BID   d128d decimal128 DPD
A line "x128 hex digits scale" gives a binary128 and its value as the
decoder should read it: 36 significant digits, nearest-even."""
import random
from fractions import Fraction
from decimal import Decimal, Context, ROUND_HALF_EVEN

def bin128(fr):
    """binary128 of a Fraction, nearest-even; returns 128-bit int"""
    neg = fr < 0
    fr = abs(fr)
    if fr == 0: return (1 << 127) if neg else 0
    # find e with 2^e <= fr < 2^(e+1)
    e = fr.numerator.bit_length() - fr.denominator.bit_length()
    while Fraction(2) ** e > fr: e -= 1
    while Fraction(2) ** (e + 1) <= fr: e += 1
    if e < -16382:          # subnormal
        q = fr / Fraction(2) ** (-16382 - 112)
        m = round_even(q)
        if m >= (1 << 112): biased = 1; m -= 1 << 112
        else: biased = 0
    else:
        q = fr / Fraction(2) ** (e - 112)
        m = round_even(q)
        if m >= (1 << 113): m >>= 1; e += 1
        biased = e + 16383
        if biased >= 0x7FFF: return None   # overflow
        m -= 1 << 112
    return (neg << 127) | (biased << 112) | m

def round_even(q):
    n, r = divmod(q.numerator, q.denominator)
    twice = 2 * r
    if twice > q.denominator or (twice == q.denominator and n & 1): n += 1
    return n

def bin128_value(bits):
    neg = bits >> 127
    e = (bits >> 112) & 0x7FFF
    m = bits & ((1 << 112) - 1)
    if e == 0x7FFF: return None
    if e == 0: fr = Fraction(m, 1 << (16382 + 112))
    else: fr = Fraction(m | (1 << 112), 1) * Fraction(2) ** (e - 16383 - 112)
    return -fr if neg else fr

def to36(fr):
    """36 significant digits, nearest-even: (digits string, scale)"""
    if fr == 0: return "0", 0
    fr = abs(fr)
    # scale s such that fr * 10^s has 36 digits
    s = 0
    v = fr
    while v >= 10 ** 36: v /= 10; s -= 1
    while v < 10 ** 35: v *= 10; s += 1
    m = round_even(v)
    if m >= 10 ** 36: m //= 10; s -= 1
    return str(m), s

DPD = {}
def dpd_decode(d):
    b = [(d >> i) & 1 for i in range(10)]
    b9,b8,b7,b6,b5,b4,b3,b2,b1,b0 = b[9],b[8],b[7],b[6],b[5],b[4],b[3],b[2],b[1],b[0]
    if not b3: return (b9*4+b8*2+b7)*100 + (b6*4+b5*2+b4)*10 + (b2*4+b1*2+b0)
    if not b2 and not b1: return (b9*4+b8*2+b7)*100 + (b6*4+b5*2+b4)*10 + 8+b0
    if not b2 and b1: return (b9*4+b8*2+b7)*100 + (8+b4)*10 + (b6*4+b5*2+b0)
    if b2 and not b1: return (8+b7)*100 + (b6*4+b5*2+b4)*10 + (b9*4+b8*2+b0)
    if not b6 and not b5: return (8+b7)*100 + (8+b4)*10 + (b9*4+b8*2+b0)
    if not b6 and b5: return (8+b7)*100 + (b9*4+b8*2+b4)*10 + 8+b0
    if b6 and not b5: return (b9*4+b8*2+b7)*100 + (8+b4)*10 + 8+b0
    return (8+b7)*100 + (8+b4)*10 + 8+b0
for d in range(1024):
    v = dpd_decode(d)
    DPD.setdefault(v, d)

def dec_encode(coef, exp, neg, size, dpd):
    """coef * 10^exp, coef < 10^digits, in range; returns the int"""
    digits, bias, ebits, cont = (16, 398, 10, 8) if size == 8 else (34, 6176, 14, 12)
    nbits = size * 8
    biased = exp + bias
    if not dpd:
        cbits = nbits - 1 - ebits
        if coef >> cbits:        # the coefficient needs its implicit 100 lead: the 11 prefix, exponent, then 2 fewer bits
            v = (3 << (nbits - 3)) | (biased << (cbits - 2)) | (coef & ((1 << (cbits - 2)) - 1))
        else:
            v = (biased << cbits) | coef
    else:
        ds = str(coef).rjust(digits, "0")
        lead = int(ds[0])
        ehi = biased >> cont
        comb = (0x18 | (ehi << 1) | (lead & 1)) if lead >= 8 else ((ehi << 3) | lead)
        v = (comb << (nbits - 6)) | ((biased & ((1 << cont) - 1)) << (nbits - 6 - cont))
        rest = ds[1:]
        n = (digits - 1) // 3
        for i in range(n):
            trip = int(rest[3 * i: 3 * i + 3])
            v |= DPD[trip] << (10 * (n - 1 - i))
    if neg: v |= 1 << (nbits - 1)
    return v

def main():
    rnd = random.Random(20261007)
    out = []
    # binary128 encodings of decimals
    vals = [("1", 0), ("1", 1), ("15", 1), ("3", 0), ("1", 3), ("123456789012345678901234567890123", 20),
            ("99999999999999999999999999999999999999", 0), ("1", 4000), ("1", 4931), ("7", 4966), ("5", 4967),
            ("1", 4985), ("17976931348623157", -292), ("1", -4932), ("12345", -4900)]
    for d, s in vals:
        for neg in (0, 1):
            fr = Fraction(int(d), 1) / Fraction(10) ** s if s >= 0 else Fraction(int(d), 1) * Fraction(10) ** (-s)
            if neg: fr = -fr
            b = bin128(fr)
            out.append("b128 %s %d %d %s" % (d, s, neg, "overflow" if b is None else "%032x" % b))
    for _ in range(300):
        nd = rnd.randint(1, 38)
        d = str(rnd.randint(10 ** (nd - 1), 10 ** nd - 1))
        s = rnd.randint(-4900, 4950) if rnd.random() < 0.3 else rnd.randint(-40, 60)
        fr = Fraction(int(d), 1) * Fraction(10) ** (-s)
        b = bin128(fr)
        out.append("b128 %s %d 0 %s" % (d, s, "overflow" if b is None else "%032x" % b))
    # binary128 decodings
    for _ in range(300):
        e = rnd.choice([rnd.randint(1, 0x7FFE), rnd.randint(16000, 16760), 0, 1])
        m = rnd.getrandbits(112)
        bits = (e << 112) | m
        fr = bin128_value(bits)
        d, s = to36(fr)
        out.append("x128 %032x %s %d" % (bits, d, s))
    for bits in (0x3FFF0000000000000000000000000000, 0x3FFB999999999999999999999999999A, 0x40000000000000000000000000000000,
                 0x7FFEFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 0x00000000000000000000000000000001, 0x00010000000000000000000000000000):
        d, s = to36(bin128_value(bits))
        out.append("x128 %032x %s %d" % (bits, d, s))
    # decimal formats: coefficient and exponent within range
    for size, digits, emin, emax in ((8, 16, -383, 384), (16, 34, -6143, 6144)):
        for _ in range(300):
            nd = rnd.randint(1, digits)
            coef = rnd.randint(10 ** (nd - 1), 10 ** nd - 1) if rnd.random() < 0.9 else rnd.randint(0, 9)
            exp = rnd.randint(emin - (digits - 1), emax - (digits - 1))
            neg = rnd.randint(0, 1)
            for dpd in (0, 1):
                v = dec_encode(coef, exp, neg, size, dpd)
                out.append("%s %d %d %d %0*x" % ("d64" if size == 8 else "d128", coef, -exp, neg, size * 2, v) if not dpd
                           else "%s %d %d %d %0*x" % ("d64d" if size == 8 else "d128d", coef, -exp, neg, size * 2, v))
    print("\n".join(out))

main()
