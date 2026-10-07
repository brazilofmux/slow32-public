/* ieee.h -- the standard floating-point formats of COBOL 2014 that the
 * hardware does not have (docs/usage.md; docs/plans/standard-queue.md
 * item 20): ISO/IEC 60559's decimal64 and decimal128, in the binary
 * integer (BID) and densely packed (DPD) encodings, and binary128.  Each
 * is read into a wide decimal value (wide.h: a sign, a scale and a
 * 128-bit magnitude) and written from one, correctly rounded, and the
 * arithmetic between is the wide stack's.  Everything static, as wide.h
 * is: libcob.c includes this, so does the compiler (a VALUE clause is
 * encoded at compile time), and tests/ieee_test.c checks it on the host.
 *
 * A value is kept as (mag, scale): mag * 10^-scale, so scale is minus
 * the decimal exponent.  Big integers beyond the 4-limb magnitude -- a
 * binary128's exact decimal expansion runs to thousands of digits -- are
 * limb arrays sized for the worst case, on the stack. */
#ifndef COB_IEEE_H
#define COB_IEEE_H

#include "wide.h"

#define IEEE_FN static __attribute__((unused))

/* a binary128 is M * 2^k with |k| <= 16494 and M < 2^113: 16607 bits;
 * a decimal scaled to meet it needs as many */
#define IEEE_BIG 540                    /* limbs: 17280 bits */

/* ---- bytes ----------------------------------------------------------- */

/* the format's bytes to limbs (least significant first), honouring the
 * byte order: little-endian in storage unless bigend */
IEEE_FN void ieee_load(const unsigned char *p, int size, int bigend, wl_t *limbs)
{
    int n = size / 4;
    for (int i = 0; i < n; i++) {
        const unsigned char *b = bigend ? p + size - 4 - 4 * i : p + 4 * i;
        limbs[i] = bigend ? ((wl_t)b[0] << 24) | ((wl_t)b[1] << 16) | ((wl_t)b[2] << 8) | b[3]
                          : ((wl_t)b[3] << 24) | ((wl_t)b[2] << 16) | ((wl_t)b[1] << 8) | b[0];
    }
}
IEEE_FN void ieee_store(unsigned char *p, int size, int bigend, const wl_t *limbs)
{
    int n = size / 4;
    for (int i = 0; i < n; i++) {
        unsigned char *b = bigend ? p + size - 4 - 4 * i : p + 4 * i;
        wl_t w = limbs[i];
        if (bigend) { b[0] = (unsigned char)(w >> 24); b[1] = (unsigned char)(w >> 16); b[2] = (unsigned char)(w >> 8); b[3] = (unsigned char)w; }
        else { b[3] = (unsigned char)(w >> 24); b[2] = (unsigned char)(w >> 16); b[1] = (unsigned char)(w >> 8); b[0] = (unsigned char)w; }
    }
}

/* bit fields of a limb array: bits [lo, lo+n) as an unsigned (n <= 32) */
IEEE_FN wl_t bits_get(const wl_t *a, int lo, int n)
{
    unsigned long long v = 0;
    int li = lo / 32, sh = lo % 32;
    v = a[li] >> sh;
    if (sh + n > 32) v |= (unsigned long long)a[li + 1] << (32 - sh);
    return n == 32 ? (wl_t)v : (wl_t)(v & ((1ull << n) - 1));
}
IEEE_FN void bits_put(wl_t *a, int lo, int n, wl_t v)
{
    for (int i = 0; i < n; i++) {
        int b = lo + i;
        if ((v >> i) & 1) a[b / 32] |= 1u << (b % 32); else a[b / 32] &= ~(1u << (b % 32));
    }
}

/* ---- DPD: declets of three digits in ten bits --------------------------- */

IEEE_FN unsigned dpd_decode(unsigned d)
{
    /* the standard's table, by its bit patterns (ISO/IEC 60559 3.5.2) */
    unsigned b9 = (d >> 9) & 1, b8 = (d >> 8) & 1, b7 = (d >> 7) & 1, b6 = (d >> 6) & 1, b5 = (d >> 5) & 1, b4 = (d >> 4) & 1,
             b3 = (d >> 3) & 1, b2 = (d >> 2) & 1, b1 = (d >> 1) & 1, b0 = d & 1;
    unsigned d1, d2, d3;
    if (!b3) { d1 = b9 * 4 + b8 * 2 + b7; d2 = b6 * 4 + b5 * 2 + b4; d3 = b2 * 4 + b1 * 2 + b0; }
    else if (!b2 && !b1) { d1 = b9 * 4 + b8 * 2 + b7; d2 = b6 * 4 + b5 * 2 + b4; d3 = 8 + b0; }
    else if (!b2 && b1) { d1 = b9 * 4 + b8 * 2 + b7; d2 = 8 + b4; d3 = b6 * 4 + b5 * 2 + b0; }
    else if (b2 && !b1) { d1 = 8 + b7; d2 = b6 * 4 + b5 * 2 + b4; d3 = b9 * 4 + b8 * 2 + b0; }
    else if (!b6 && !b5) { d1 = 8 + b7; d2 = 8 + b4; d3 = b9 * 4 + b8 * 2 + b0; }
    else if (!b6 && b5) { d1 = 8 + b7; d2 = b9 * 4 + b8 * 2 + b4; d3 = 8 + b0; }
    else if (b6 && !b5) { d1 = b9 * 4 + b8 * 2 + b7; d2 = 8 + b4; d3 = 8 + b0; }
    else { d1 = 8 + b7; d2 = 8 + b4; d3 = 8 + b0; }
    return d1 * 100 + d2 * 10 + d3;
}
IEEE_FN unsigned dpd_encode(unsigned v)
{
    static unsigned short tab[1000]; static int made;
    if (!made) {                        /* the inverse of the decoding, canonical patterns only */
        for (int i = 0; i < 1000; i++) tab[i] = 0xFFFF;
        for (unsigned d = 0; d < 1024; d++) { unsigned x = dpd_decode(d); if (tab[x] == 0xFFFF) tab[x] = (unsigned short)d; }
        made = 1;
    }
    return tab[v];
}

/* ---- the decimal formats -------------------------------------------------
 * decimal64: 1 sign, 5 combination, 8 exponent continuation, 50 coefficient
 * continuation (DPD) -- or, BID, 1 sign, then a 10-bit exponent and a
 * 53-bit coefficient (the exponent after an 11 prefix when the coefficient
 * needs its implicit 100 lead, which a canonical decimal64 never does).
 * decimal128: 1, 5, 12, 110; BID 14-bit exponent, 113-bit coefficient.
 * Bias 398 (decimal64), 6176 (decimal128); 16 or 34 digits. */
typedef struct { int size, digits, bias, ebits, cont; int emin, emax; } dec_fmt;
IEEE_FN dec_fmt dec_format(int size)
{
    dec_fmt f;
    if (size == 8) { f.size = 8; f.digits = 16; f.bias = 398; f.ebits = 10; f.cont = 8; f.emin = -383; f.emax = 384; }
    else { f.size = 16; f.digits = 34; f.bias = 6176; f.ebits = 14; f.cont = 12; f.emin = -6143; f.emax = 6144; }
    return f;
}

/* decode: 0 finite (mag, scale, neg set), 1 infinity, 2 NaN.  A
 * non-canonical BID coefficient (past 10^digits - 1) reads as zero, as the
 * standard has it */
IEEE_FN int dec_decode(const unsigned char *p, int size, int bigend, int dpd, wl_t *mag, int *scale, int *neg)
{
    dec_fmt f = dec_format(size);
    wl_t a[4] = { 0, 0, 0, 0 };
    ieee_load(p, size, bigend, a);
    int top = size * 8 - 1;
    *neg = (int)bits_get(a, top, 1);
    unsigned comb = bits_get(a, top - 5, 5);        /* the five bits after the sign */
    for (int i = 0; i < 4; i++) mag[i] = 0;
    if ((comb & 0x1E) == 0x1E) return (comb & 1) ? 2 : 1;   /* 11110 infinity, 11111 NaN */
    int exp;
    if (!dpd) {
        if ((comb & 0x18) == 0x18) {                /* the 11 prefix: exponent next, coefficient 100 + 51/111 bits */
            exp = (int)bits_get(a, top - 2 - f.ebits, f.ebits);
            int cb = size * 8 - 3 - f.ebits;        /* coefficient continuation bits */
            for (int i = 0; i < cb; i += 32) { int n = cb - i < 32 ? cb - i : 32; bits_put(mag, i, n, bits_get(a, i, n)); }
            bits_put(mag, cb, 3, 4);                /* 100 ahead of them */
        } else {
            exp = (int)bits_get(a, top - f.ebits, f.ebits);
            int cb = size * 8 - 1 - f.ebits;
            for (int i = 0; i < cb; i += 32) { int n = cb - i < 32 ? cb - i : 32; bits_put(mag, i, n, bits_get(a, i, n)); }
        }
        /* past the format's digits: not a number of the format, zero */
        wl_t lim[WL]; w_pow10(lim, f.digits);
        if (mp_cmp(mag, lim, WL) >= 0) for (int i = 0; i < 4; i++) mag[i] = 0;
    } else {
        unsigned lead, ehi;
        if ((comb & 0x18) == 0x18) { ehi = (comb >> 1) & 3; lead = 8 + (comb & 1); }
        else { ehi = comb >> 3; lead = comb & 7; }
        exp = (int)((ehi << f.cont) | bits_get(a, top - 5 - f.cont, f.cont));
        int ndeclets = (f.digits - 1) / 3;
        /* the declets, most significant first, below the continuation */
        char dig[40]; int nd = 0;
        dig[nd++] = (char)('0' + lead);
        for (int i = ndeclets - 1; i >= 0; i--) {
            unsigned v = dpd_decode(bits_get(a, i * 10, 10));
            dig[nd++] = (char)('0' + v / 100); dig[nd++] = (char)('0' + v / 10 % 10); dig[nd++] = (char)('0' + v % 10);
        }
        w_from_digits(mag, dig, nd);
    }
    *scale = f.bias - exp;              /* value = mag * 10^(exp - bias) */
    return 0;
}

/* encode a finite value: mag * 10^-scale, |mag| < 10^digits already (the
 * caller rounds); 0 done, 1 the exponent is past the format (overflow);
 * an exponent below the minimum loses low digits (subnormal), and below
 * everything the value is zero */
IEEE_FN int dec_encode(unsigned char *p, int size, int bigend, int dpd, const wl_t *mag0, int scale, int neg)
{
    dec_fmt f = dec_format(size);
    wl_t mag[WL]; memcpy(mag, mag0, sizeof mag);
    int exp = -scale;
    /* the exponent range: emin - (digits - 1) .. emax - (digits - 1) for the
     * coefficient as an integer */
    int lo = f.emin - (f.digits - 1), hi = f.emax - (f.digits - 1);
    if (mp_is_zero(mag, WL)) { if (exp < lo) exp = lo; if (exp > hi) exp = hi; }   /* a zero keeps its exponent, within the range */
    while (exp < lo) {                  /* too small: shed a digit (rounded) per step */
        if (mp_is_zero(mag, WL)) { exp = 0; break; }
        int half; w_drop_digits(mag, WL, 1, &half, 0);
        if (half > 0 || (half == 0 && (mag[0] & 1))) mp_mul_small(mag, WL, 1, 1);
        exp++;
    }
    while (exp > hi) {                  /* too large an exponent: a trailing zero absorbs one if there is room */
        int nd = w_ndigits(mag);
        if (nd >= f.digits || !mp_is_zero(mag, WL) ? nd >= f.digits : 0) return 1;
        mp_mul_small(mag, WL, 10, 0); exp--;
        if (mp_is_zero(mag, WL)) { exp = hi; break; }
    }
    wl_t a[4] = { 0, 0, 0, 0 };
    int top = size * 8 - 1;
    unsigned biased = (unsigned)(exp + f.bias);
    if (!dpd) {
        int cb = size * 8 - 1 - f.ebits;
        if (mp_bits(mag, WL) > cb) {
            /* the coefficient's top bit is past the plain field (decimal64:
             * 2^53 <= coefficient < 10^16): the 11 prefix, the exponent, and
             * the coefficient less its implicit 100 */
            for (int i = 0; i < cb - 2; i += 32) { int n = cb - 2 - i < 32 ? cb - 2 - i : 32; bits_put(a, i, n, bits_get(mag, i, n)); }
            bits_put(a, top - 2 - f.ebits, f.ebits, biased);
            bits_put(a, top - 2, 2, 3);
        } else {
            for (int i = 0; i < cb; i += 32) { int n = cb - i < 32 ? cb - i : 32; bits_put(a, i, n, bits_get(mag, i, n)); }
            bits_put(a, top - f.ebits, f.ebits, biased);
        }
    } else {
        char dig[40]; int n = f.digits;
        w_to_digits(mag, dig, n);
        unsigned lead = (unsigned)(dig[0] - '0');
        unsigned ehi = biased >> f.cont, comb;
        if (lead >= 8) comb = 0x18 | (ehi << 1) | (lead & 1); else comb = (ehi << 3) | lead;
        bits_put(a, top - 5, 5, comb);
        bits_put(a, top - 5 - f.cont, f.cont, biased & ((1u << f.cont) - 1));
        int ndeclets = (f.digits - 1) / 3;
        for (int i = 0; i < ndeclets; i++) {
            const char *d = dig + 1 + 3 * (ndeclets - 1 - i);
            unsigned v = (unsigned)((d[0] - '0') * 100 + (d[1] - '0') * 10 + (d[2] - '0'));
            bits_put(a, i * 10, 10, dpd_encode(v));
        }
    }
    bits_put(a, top, 1, (wl_t)(neg != 0));
    ieee_store(p, size, bigend, a);
    return 0;
}

/* round a wide magnitude to at most `digits` significant digits
 * (nearest, ties to even), adjusting the scale */
IEEE_FN void ieee_round_digits(wl_t *mag, int *scale, int digits)
{
    int nd = w_ndigits(mag);
    if (nd <= digits) return;
    int half; w_drop_digits(mag, WL, nd - digits, &half, 0);
    *scale -= nd - digits;
    if (half > 0 || (half == 0 && (mag[0] & 1))) {
        mp_mul_small(mag, WL, 1, 1);
        if (w_ndigits(mag) > digits) { mp_div_small(mag, WL, 10); *scale -= 1; }   /* 999.. rounded up to 1000.. */
    }
}

/* ---- big integers (IEEE_BIG limbs) -------------------------------------- */

IEEE_FN int big_top(const wl_t *a, int n) { while (n > 0 && !a[n - 1]) n--; return n; }
/* a *= 10^k */
IEEE_FN void big_mul_pow10(wl_t *a, int n, int k)
{
    for (; k >= 9; k -= 9) mp_mul_small(a, n, 1000000000u, 0);
    static const wl_t p[9] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 };
    if (k) mp_mul_small(a, n, p[k], 0);
}
/* a <<= k */
IEEE_FN void big_shl(wl_t *a, int n, int k)
{
    int words = k / 32, bits = k % 32;
    if (words) { for (int i = n - 1; i >= 0; i--) a[i] = i - words >= 0 ? a[i - words] : 0; }
    if (bits) { for (int i = n - 1; i >= 0; i--) a[i] = (a[i] << bits) | (i > 0 ? a[i - 1] >> (32 - bits) : 0); }
}
/* a >>= k; *sticky set if any dropped bit was 1, *half the bit just below the result */
IEEE_FN void big_shr(wl_t *a, int n, int k, int *half, int *sticky)
{
    *half = 0; *sticky = 0;
    if (k <= 0) return;
    int hb = k - 1;
    *half = (int)((a[hb / 32] >> (hb % 32)) & 1);
    for (int b = 0; b < hb; b++) if ((a[b / 32] >> (b % 32)) & 1) { *sticky = 1; break; }
    int words = k / 32, bits = k % 32;
    if (words) { for (int i = 0; i < n; i++) a[i] = i + words < n ? a[i + words] : 0; }
    if (bits) { for (int i = 0; i < n; i++) a[i] = (a[i] >> bits) | (i + 1 < n ? a[i + 1] << (32 - bits) : 0); }
}
/* a /= 10^k, with the half and sticky of the dropped part */
IEEE_FN void big_div_pow10(wl_t *a, int n, int k, int *half, int *sticky)
{
    *sticky = 0; *half = -1;
    for (int i = 0; i < k; i++) {
        wl_t r = mp_div_small(a, n, 10);
        if (i < k - 1) { if (r) *sticky = 1; }
        else *half = r > 5 ? 1 : r == 5 ? (*sticky ? 1 : 0) : -1;   /* the last digit dropped decides, with what went before */
    }
}
/* the decimal digits of a (most significant first), the count returned */
IEEE_FN int big_to_digits(wl_t *a, int n, char *out, int cap)
{
    char tmp[6000]; int i = 0, top = big_top(a, n);
    if (!top) { out[0] = '0'; return 1; }
    while (top > 0 && i < (int)sizeof tmp - 9) {
        wl_t r = mp_div_small(a, top, 1000000000u);
        top = big_top(a, top);
        for (int k = 0; k < 9; k++) { tmp[i++] = (char)('0' + r % 10); r /= 10; }
    }
    while (i > 1 && tmp[i - 1] == '0') i--;
    int n_out = i < cap ? i : cap;
    for (int k = 0; k < n_out; k++) out[k] = tmp[i - 1 - k];
    return i;                           /* all of them, even past cap */
}

/* ---- binary128 ------------------------------------------------------------
 * 1 sign, 15 exponent (bias 16383), 112 fraction; a leading 1 implied for
 * exponents 1..32766; 0: zero or subnormal (2^-16382 * 0.f); 32767: an
 * infinity (fraction 0) or a NaN. */

/* round-to-nearest-even's verdict: half is the first dropped unit (bit
 * or digit) relative to half the quantum -- 1 above, 0 exactly half, -1
 * below; sticky says more was dropped below it; odd the kept value's
 * last bit */
IEEE_FN int ieee_round_up(int half, int sticky, int odd)
{
    if (half > 0) return 1;
    if (half < 0) return 0;
    return sticky ? 1 : odd;
}

/* decode to a decimal of at most 36 significant digits (binary128 holds
 * 34 and a bit): 0 finite, 1 infinity, 2 NaN */
IEEE_FN int bin128_decode(const unsigned char *p, int bigend, wl_t *mag, int *scale, int *neg)
{
    wl_t a[4];
    ieee_load(p, 16, bigend, a);
    *neg = (int)(a[3] >> 31);
    int e = (int)((a[3] >> 16) & 0x7FFF);
    wl_t m[4] = { a[0], a[1], a[2], a[3] & 0xFFFF };
    for (int i = 0; i < 4; i++) mag[i] = 0;
    *scale = 0;
    if (e == 0x7FFF) return mp_is_zero(m, 4) ? 1 : 2;
    int k;
    if (e == 0) { if (mp_is_zero(m, 4)) return 0; k = -16382 - 112; }
    else { m[3] |= 1u << 16; k = e - 16383 - 112; }     /* value = m * 2^k */
    wl_t big[IEEE_BIG]; memset(big, 0, sizeof big);
    memcpy(big, m, sizeof m);
    int p10 = 0, half = -1, sticky = 0;
    if (k >= 0) big_shl(big, IEEE_BIG, k);
    else {
        /* m * 10^p / 2^-k, p chosen so the quotient has 37 or 38 digits;
         * the bits shifted out are the fraction below it */
        double lg = (mp_bits(m, 4) - 1 + k) * 0.30102999566398;      /* about log10 of the value */
        p10 = 37 - (int)lg;
        if (p10 < 0) p10 = 0;
        big_mul_pow10(big, IEEE_BIG, p10);
        big_shr(big, IEEE_BIG, -k, &half, &sticky);
        if (half) { sticky = 1; half = -1; }    /* the binary fraction counts only as "something below" the digits */
    }
    /* the integer's digits, cut to 36 with rounding */
    { char dig[6000]; wl_t t[IEEE_BIG]; memcpy(t, big, sizeof t); int nd = big_to_digits(t, IEEE_BIG, dig, 40);
      int drop = nd > 36 ? nd - 36 : 0;
      if (drop) {
          int h, s; big_div_pow10(big, IEEE_BIG, drop, &h, &s);
          if (h == 0 && sticky) h = 1;          /* exactly half of the digits dropped, but a fraction below them: above half */
          half = h; sticky = s || sticky;
      } else if (half < 0 && !sticky) half = -1;
      memcpy(mag, big, sizeof(wl_t) * 4);
      if (drop ? ieee_round_up(half, sticky, (int)(mag[0] & 1)) : 0) {
          mp_mul_small(mag, WL, 1, 1);
          if (w_ndigits(mag) > 36) { mp_div_small(mag, WL, 10); drop++; }
      }
      *scale = p10 - drop;
    }
    return 0;
}

/* encode mag * 10^-scale (|mag| < 2^128), correctly rounded to nearest
 * even: 0 done, 1 too large (overflow); too small gives a subnormal or
 * zero */
IEEE_FN int bin128_encode(unsigned char *p, int bigend, const wl_t *mag, int scale, int neg)
{
    wl_t a[4] = { 0, 0, 0, 0 };
    if (mp_is_zero(mag, WL)) { bits_put(a, 127, 1, (wl_t)(neg != 0)); ieee_store(p, 16, bigend, a); return 0; }
    wl_t big[IEEE_BIG]; memset(big, 0, sizeof big);
    memcpy(big, mag, sizeof(wl_t) * WL);
    int e2, rem = 0;                    /* value = big * 2^e2, plus a fraction below big's last bit when rem */
    if (scale <= 0) { big_mul_pow10(big, IEEE_BIG, -scale); e2 = 0; }
    else {
        /* big * 2^k / 10^scale with k chosen so the quotient keeps 116+ bits */
        int k = 117 + (int)(scale * 3.3219280948874) + 1 - mp_bits(mag, WL);
        if (k < 0) k = 0;
        big_shl(big, IEEE_BIG, k);
        int h, s; big_div_pow10(big, IEEE_BIG, scale, &h, &s);
        rem = s || h >= 0;
        e2 = -k;
    }
    /* to 113 bits: the dropped bits and rem decide the rounding */
    int nb = mp_bits(big, IEEE_BIG), half = -1, sticky = rem;
    if (nb > 113) {
        int h, s; big_shr(big, IEEE_BIG, nb - 113, &h, &s);
        half = h ? (s || rem ? 1 : 0) : -1; sticky = s || rem;
        e2 += nb - 113;
    } else if (nb < 113) { big_shl(big, IEEE_BIG, 113 - nb); e2 -= 113 - nb; }
    int e = e2 + 112 + 16383;          /* big = 1.f * 2^112 */
    if (e <= 0) {
        /* subnormal: 1 - e more bits go, everything dropped so far below them */
        int sh = 1 - e, prev = half >= 0 || sticky;
        if (sh > 114) { memset(big, 0, sizeof big); half = -1; sticky = 1; }
        else { int h, s; big_shr(big, IEEE_BIG, sh, &h, &s); half = h ? (s || prev ? 1 : 0) : -1; sticky = s || prev; }
        e = 0;
    }
    if (ieee_round_up(half, sticky, (int)(big[0] & 1))) {
        mp_mul_small(big, 4, 1, 1);
        if (e == 0) { if (mp_bits(big, 4) == 113) e = 1; }
        else if (mp_bits(big, 4) > 113) { int h, s; big_shr(big, 4, 1, &h, &s); e++; }
    }
    if (e >= 0x7FFF) return 1;
    a[0] = big[0]; a[1] = big[1]; a[2] = big[2]; a[3] = big[3] & 0xFFFF;
    a[3] |= (wl_t)e << 16;
    bits_put(a, 127, 1, (wl_t)(neg != 0));
    ieee_store(p, 16, bigend, a);
    return 0;
}

#endif
