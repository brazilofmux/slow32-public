/*
 * fma / fmaf -- fused multiply-add with a single rounding (GitHub issue 99).
 *
 * Until these existed, llvm.fma lowered to a multiply and an add, two
 * roundings, and std::fma / __builtin_fma were quietly unfused.  The
 * backend now calls fma/fmaf, and these are the only implementations:
 * the engines that intercept libm by symbol (slow32-dbt, qemu) have no
 * entry for them, so this code runs everywhere.
 *
 * Method.  The exact product of two 53-bit significands is 106 bits, and
 * the addend can sit anywhere within about 2^2100 of it, so the exact sum
 * is formed in a 192-bit integer accumulator: whichever operand has the
 * higher leading bit is placed with that bit at position 169; the other
 * is brought to the same scale, shifted left exactly, or right with every
 * bit shifted out folded into a sticky bit at position 0.  The anchor's
 * own lowest bit is at position 64 or above, so the sticky bit cannot
 * make a false tie, and the result is normalised and rounded once, to
 * nearest-even, into the destination format.  float and double share the
 * core; only the significand width and exponent range differ.
 *
 * Inputs with a NaN, an infinity or a zero are handed to the ordinary
 * operations, which are exact for them: a zero factor makes the sum a
 * single addition, a zero addend makes it a single multiplication (not
 * a multiplication and an addition: a product that underflows to -0 must
 * stay -0 when +0 is added, and does not), and the special values
 * propagate as C requires -- except an infinite addend beside a finite
 * product that overflows, which is the addend, not the NaN of inf - inf.
 *
 * Built with -fno-builtin like the rest of the soft library, so that the
 * compiler does not recognise fma and call it from itself.
 */

#include <math.h>

typedef unsigned long long u64;
typedef unsigned int u32;
typedef union { double d; u64 u; } dbl_u;
typedef union { float f; u32 u; } flt_u;

/* 192-bit unsigned integer, w[0] least significant. */
typedef struct { u64 w[3]; } u192;

static int clz64(u64 x) {
    int n = 0;
    if (x == 0) return 64;
    if (!(x >> 32)) { n += 32; x <<= 32; }
    if (!(x >> 48)) { n += 16; x <<= 16; }
    if (!(x >> 56)) { n += 8;  x <<= 8;  }
    if (!(x >> 60)) { n += 4;  x <<= 4;  }
    if (!(x >> 62)) { n += 2;  x <<= 2;  }
    if (!(x >> 63)) { n += 1; }
    return n;
}

/* Index of the highest set bit, or -1 for zero. */
static int msb192(const u192 *a) {
    if (a->w[2]) return 191 - clz64(a->w[2]);
    if (a->w[1]) return 127 - clz64(a->w[1]);
    if (a->w[0]) return 63 - clz64(a->w[0]);
    return -1;
}

static void mul64(u64 a, u64 b, u64 *hi, u64 *lo) {
    u64 a0 = (u32)a, a1 = a >> 32, b0 = (u32)b, b1 = b >> 32;
    u64 p00 = a0 * b0, p01 = a0 * b1, p10 = a1 * b0, p11 = a1 * b1;
    u64 mid = (p00 >> 32) + (u32)p01 + (u32)p10;
    *lo = (mid << 32) | (u32)p00;
    *hi = p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32);
}

static void set192(u192 *a, u64 hi, u64 lo) {
    a->w[0] = lo; a->w[1] = hi; a->w[2] = 0;
}

/* Any set bit at an index below i. */
static int any_below(const u192 *a, int i) {
    int q, r, k;
    if (i <= 0) return 0;
    if (i >= 192) return msb192(a) >= 0;
    q = i >> 6; r = i & 63;
    for (k = 0; k < q; k++) if (a->w[k]) return 1;
    return r ? (a->w[q] & ((1ULL << r) - 1)) != 0 : 0;
}

static void shl192(u192 *a, int s) {
    u64 w[3] = { 0, 0, 0 };
    int q, r, i;
    if (s <= 0) return;
    q = s >> 6; r = s & 63;
    for (i = 2; i >= 0; i--) {
        int j = i - q;
        if (j < 0) break;
        w[i] = a->w[j] << r;
        if (r && j >= 1) w[i] |= a->w[j - 1] >> (64 - r);
    }
    a->w[0] = w[0]; a->w[1] = w[1]; a->w[2] = w[2];
}

/* Shift right, returning 1 if any set bit was shifted out. */
static int shr192(u192 *a, int s) {
    u64 w[3] = { 0, 0, 0 };
    int sticky = any_below(a, s);
    int q, r, i;
    if (s <= 0) return 0;
    if (s < 192) {
        q = s >> 6; r = s & 63;
        for (i = 0; i < 3; i++) {
            int j = i + q;
            if (j > 2) break;
            w[i] = a->w[j] >> r;
            if (r && j <= 1) w[i] |= a->w[j + 1] << (64 - r);
        }
    }
    a->w[0] = w[0]; a->w[1] = w[1]; a->w[2] = w[2];
    return sticky;
}

/* (a >> s) truncated to 64 bits; the caller knows that is all there is. */
static u64 shr_u64(const u192 *a, int s) {
    int q, r;
    u64 lo, hi;
    if (s >= 192) return 0;
    q = s >> 6; r = s & 63;
    lo = a->w[q];
    hi = q < 2 ? a->w[q + 1] : 0;
    return r ? (lo >> r) | (hi << (64 - r)) : lo;
}

static void add192(u192 *a, const u192 *b) {
    u64 c = 0;
    int i;
    for (i = 0; i < 3; i++) {
        u64 s = a->w[i] + b->w[i];
        u64 c1 = s < a->w[i];
        u64 t = s + c;
        u64 c2 = t < s;
        a->w[i] = t;
        c = c1 | c2;
    }
}

/* a -= b, requiring a >= b. */
static void sub192(u192 *a, const u192 *b) {
    u64 br = 0;
    int i;
    for (i = 0; i < 3; i++) {
        u64 d = a->w[i] - b->w[i];
        u64 b1 = a->w[i] < b->w[i];
        u64 t = d - br;
        u64 b2 = d < br;
        a->w[i] = t;
        br = b1 | b2;
    }
}

static int cmp192(const u192 *a, const u192 *b) {
    int i;
    for (i = 2; i >= 0; i--) {
        if (a->w[i] != b->w[i]) return a->w[i] < b->w[i] ? -1 : 1;
    }
    return 0;
}

/*
 * Round acc * 2^e0 (acc nonzero, sticky already folded into bit 0) to a
 * format with prec significand bits, smallest normal exponent emin,
 * exponent bias, and all-ones exponent field emaxf.  Returns the exponent
 * and significand fields together; the sign is the caller's.
 */
static u64 round_pack(const u192 *acc, int e0, int prec, int emin, int bias, int emaxf) {
    int m = msb192(acc);          /* position of the leading 1 */
    int e = e0 + m;               /* its exponent */
    int ebiased, shift;
    u64 mant, r;
    if (e < emin) {               /* subnormal: the LSB is pinned at emin-(prec-1) */
        shift = (emin - (prec - 1)) - e0;
        ebiased = 0;
    } else {
        shift = m - (prec - 1);
        ebiased = e + bias;
    }
    if (shift <= 0) {
        mant = acc->w[0] << -shift;             /* exact: fewer bits than the format */
    } else {
        int guard = shift - 1 < 192 ? (int)((acc->w[(shift - 1) >> 6] >> ((shift - 1) & 63)) & 1) : 0;
        int sticky = any_below(acc, shift - 1);
        mant = shr_u64(acc, shift);
        if (guard && (sticky || (mant & 1))) mant++;
    }
    /* With the implicit bit inside mant, a round-up that carries out of
     * the significand lands in the exponent field by itself, and a
     * subnormal that rounds up to 2^(prec-1) becomes the smallest normal. */
    r = ((u64)(ebiased ? ebiased - 1 : 0) << (prec - 1)) + mant;
    if ((r >> (prec - 1)) >= (u64)emaxf) r = (u64)emaxf << (prec - 1);   /* overflow: infinity */
    return r;
}

/*
 * s1 * m1 * 2^e1 + s2 * m2 * 2^e2, exactly, then one rounding.  m1 is a
 * 128-bit product (hi:lo), m2 a significand below 2^64; both nonzero.
 * Returns the magnitude fields; *sign receives the result's sign, which
 * for an exact cancellation is + (round to nearest).
 */
static u64 fma_core(int s1, u64 m1hi, u64 m1lo, int e1, int s2, u64 m2, int e2,
                    int prec, int emin, int bias, int emaxf, int *sign) {
    int top1 = m1hi ? 127 - clz64(m1hi) : 63 - clz64(m1lo);
    int top2 = 63 - clz64(m2);
    int lead1 = e1 + top1, lead2 = e2 + top2;
    u192 a, b;
    int sa, sb, e0, d, sticky;

    if (lead1 >= lead2) {
        int sh = 169 - top1;
        set192(&a, m1hi, m1lo); shl192(&a, sh); e0 = e1 - sh; sa = s1;
        set192(&b, 0, m2);      d = e2 - e0;    sb = s2;
    } else {
        int sh = 169 - top2;
        set192(&a, 0, m2);      shl192(&a, sh); e0 = e2 - sh; sa = s2;
        set192(&b, m1hi, m1lo); d = e1 - e0;    sb = s1;
    }
    if (d >= 0) {
        shl192(&b, d);                  /* exact: b's leading bit lands at or below 169 */
    } else {
        sticky = shr192(&b, -d);
        if (sticky) b.w[0] |= 1;        /* the anchor's bits start at 64: no false tie */
    }

    if (sa == sb) {
        add192(&a, &b);
        *sign = sa;
    } else {
        int c = cmp192(&a, &b);
        if (c >= 0) {
            sub192(&a, &b);
            *sign = sa;
        } else {
            sub192(&b, &a);
            a = b;
            *sign = sb;
        }
        if (msb192(&a) < 0) { *sign = 0; return 0; }
    }
    return round_pack(&a, e0, prec, emin, bias, emaxf);
}

double fma(double x, double y, double z) {
    dbl_u ux, uy, uz;
    int ex, ey, ez, sx, sy, sz, sign;
    u64 mx, my, mz, phi, plo, bits;
    ux.d = x; uy.d = y; uz.d = z;
    ex = (int)((ux.u >> 52) & 0x7ff);
    ey = (int)((uy.u >> 52) & 0x7ff);
    ez = (int)((uz.u >> 52) & 0x7ff);
    if (ex == 0x7ff || ey == 0x7ff || x == 0.0 || y == 0.0) return x * y + z;
    if (ez == 0x7ff) return z;              /* finite product, infinite or NaN addend */
    if (z == 0.0) return x * y;
    sx = (int)(ux.u >> 63); sy = (int)(uy.u >> 63); sz = (int)(uz.u >> 63);
    mx = ux.u & 0xfffffffffffffULL; if (ex) mx |= 1ULL << 52;
    my = uy.u & 0xfffffffffffffULL; if (ey) my |= 1ULL << 52;
    mz = uz.u & 0xfffffffffffffULL; if (ez) mz |= 1ULL << 52;
    ex = ex ? ex - 1075 : -1074;    /* value = m * 2^e */
    ey = ey ? ey - 1075 : -1074;
    ez = ez ? ez - 1075 : -1074;
    mul64(mx, my, &phi, &plo);
    bits = fma_core(sx ^ sy, phi, plo, ex + ey, sz, mz, ez, 53, -1022, 1023, 0x7ff, &sign);
    ux.u = ((u64)sign << 63) | bits;
    return ux.d;
}

float fmaf(float x, float y, float z) {
    flt_u ux, uy, uz;
    int ex, ey, ez, sx, sy, sz, sign;
    u64 mx, my, mz, bits;
    ux.f = x; uy.f = y; uz.f = z;
    ex = (int)((ux.u >> 23) & 0xff);
    ey = (int)((uy.u >> 23) & 0xff);
    ez = (int)((uz.u >> 23) & 0xff);
    if (ex == 0xff || ey == 0xff || x == 0.0f || y == 0.0f) return x * y + z;
    if (ez == 0xff) return z;
    if (z == 0.0f) return x * y;
    sx = (int)(ux.u >> 31); sy = (int)(uy.u >> 31); sz = (int)(uz.u >> 31);
    mx = ux.u & 0x7fffff; if (ex) mx |= 1u << 23;
    my = uy.u & 0x7fffff; if (ey) my |= 1u << 23;
    mz = uz.u & 0x7fffff; if (ez) mz |= 1u << 23;
    ex = ex ? ex - 150 : -149;
    ey = ey ? ey - 150 : -149;
    ez = ez ? ez - 150 : -149;
    bits = fma_core(sx ^ sy, 0, mx * my, ex + ey, sz, mz, ez, 24, -126, 127, 0xff, &sign);
    ux.u = ((u32)sign << 31) | (u32)bits;
    return ux.f;
}
