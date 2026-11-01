/* strtof and strtold (C11 7.22.1.3), for both C libraries.
 *
 * strtold is strtod: long double is double on SLOW-32.
 *
 * strtof rounds the input once, to float.  It starts from strtod's double
 * d, which is the input rounded once to 53 bits, and narrows it -- which is
 * a second rounding, and goes wrong in exactly one case: d lies on the
 * midpoint between two adjacent floats (every such midpoint has 25
 * significant bits, so it is a double).  Then the input itself may lie a
 * little above or below that midpoint, and only the input says which way
 * the float rounds; "(float)d" would break the apparent tie to even.  So in
 * that case the input's digits -- decimal or hexadecimal, as many as there
 * are -- are compared with the midpoint exactly, in integers.  (It was
 * "(float)strtod", and "1.00000005960464477539062501" came back 1.0f, not
 * the float above it.)
 *
 * Range errors (7.22.1.3p10): a finite double that overflows float gives
 * +-HUGE_VALF and ERANGE; one that underflows to zero or to a subnormal
 * float gives ERANGE too, as strtod does for doubles.
 */
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern double dtoa_strtod(const char *s00, char **se);

long double strtold(const char *nptr, char **endptr) {
    return dtoa_strtod(nptr, endptr);
}

/* --- a little unsigned big-integer arithmetic, 32-bit limbs, least first --- */

typedef struct {
    uint32_t *w;
    int n;          /* limbs in use */
    int cap;
} big;

static int big_init(big *b, int cap) {
    b->w = (uint32_t *)calloc((size_t)cap, sizeof(uint32_t));
    b->n = 0;
    b->cap = cap;
    return b->w != NULL;
}

/* b = b * m + a */
static void big_muladd(big *b, uint32_t m, uint32_t a) {
    uint64_t carry = a;
    int i;
    for (i = 0; i < b->n; i++) {
        uint64_t t = (uint64_t)b->w[i] * m + carry;
        b->w[i] = (uint32_t)t;
        carry = t >> 32;
    }
    if (carry && b->n < b->cap) b->w[b->n++] = (uint32_t)carry;
}

static void big_mulpow5(big *b, long e) {
    while (e >= 13) { big_muladd(b, 1220703125u, 0); e -= 13; }    /* 5^13 */
    {
        uint32_t m = 1;
        while (e-- > 0) m *= 5;
        if (m != 1) big_muladd(b, m, 0);
    }
}

static void big_shl(big *b, long s) {
    long words = s / 32;
    int bits = (int)(s % 32);
    int i;
    if (b->n == 0) return;
    if (bits) {
        uint32_t carry = 0;
        for (i = 0; i < b->n; i++) {
            uint32_t v = b->w[i];
            b->w[i] = (v << bits) | carry;
            carry = v >> (32 - bits);
        }
        if (carry && b->n < b->cap) b->w[b->n++] = carry;
    }
    if (words) {
        for (i = b->n - 1; i >= 0; i--) b->w[i + words] = b->w[i];
        for (i = 0; i < words; i++) b->w[i] = 0;
        b->n += (int)words;
    }
}

static int big_cmp(const big *a, const big *b) {
    int i;
    if (a->n != b->n) return a->n < b->n ? -1 : 1;
    for (i = a->n - 1; i >= 0; i--)
        if (a->w[i] != b->w[i]) return a->w[i] < b->w[i] ? -1 : 1;
    return 0;
}

static int sc_space(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r';
}

static int hexval(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/* The sign of |input| - m, for m = M * 2^K (M an integer), the input being
 * the decimal or hexadecimal floating constant strtod read from s; 2 when
 * that cannot be worked out (memory), and the caller keeps (float)d. */
static int cmp_midpoint(const char *s, uint64_t M, long K) {
    const char *p = s;
    int hex = 0;
    long ndig = 0;          /* significant digits */
    long frac = 0;          /* of those, after the point */
    long ex = 0;            /* the written exponent */
    const char *dstart;
    big a, b;
    long alog2 = 0, blog2 = K;
    int r;
    int cap;
    long abits, bbits;

    while (sc_space((unsigned char)*p)) p++;
    if (*p == '+' || *p == '-') p++;
    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X') && hexval((unsigned char)p[2]) >= 0) {
        hex = 1;
        p += 2;
    } else if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X') && p[2] == '.' && hexval((unsigned char)p[3]) >= 0) {
        hex = 1;
        p += 2;
    }
    dstart = p;
    {
        int seen_point = 0;
        for (;; p++) {
            int c = (unsigned char)*p;
            if (c == '.' && !seen_point) { seen_point = 1; continue; }
            if (hex ? hexval(c) < 0 : (c < '0' || c > '9')) break;
            ndig++;
            if (seen_point) frac++;
        }
    }
    if ((hex && (*p == 'p' || *p == 'P')) || (!hex && (*p == 'e' || *p == 'E'))) {
        const char *q = p + 1;
        int neg = 0;
        if (*q == '+' || *q == '-') { neg = (*q == '-'); q++; }
        if (*q >= '0' && *q <= '9') {
            while (*q >= '0' && *q <= '9') {
                if (ex < 100000000L) ex = ex * 10 + (*q - '0');
                q++;
            }
            if (neg) ex = -ex;
        }
    }

    /* sizes, in bits, to allocate both numbers for */
    abits = 4 * ndig;                               /* 10^d < 16^d */
    bbits = 64;
    if (!hex) {
        long e10 = ex - frac;
        if (e10 >= 0) abits += 3 * e10;             /* 5^e < 8^e */
        else bbits += 3 * -e10;
    }
    {
        long sh = (hex ? 4 * -frac + ex : (ex - frac)) - K;
        if (sh < 0) sh = -sh;
        cap = (int)((abits > bbits ? abits : bbits) + sh) / 32 + 4;
    }
    if (!big_init(&a, cap)) return 2;
    if (!big_init(&b, cap)) { free(a.w); return 2; }

    /* a = the digits as an integer */
    for (p = dstart; ; p++) {
        int c = (unsigned char)*p;
        if (c == '.') continue;
        if (hex) {
            int v = hexval(c);
            if (v < 0) break;
            big_muladd(&a, 16, (uint32_t)v);
        } else {
            if (c < '0' || c > '9') break;
            big_muladd(&a, 10, (uint32_t)(c - '0'));
        }
    }
    /* b = M */
    b.w[0] = (uint32_t)M;
    b.w[1] = (uint32_t)(M >> 32);
    b.n = b.w[1] ? 2 : (b.w[0] ? 1 : 0);

    if (hex) {
        alog2 = ex - 4 * frac;                      /* input = a * 2^alog2 */
    } else {
        long e10 = ex - frac;                       /* input = a * 10^e10 */
        if (e10 >= 0) {
            big_mulpow5(&a, e10);
            alog2 = e10;
        } else {
            big_mulpow5(&b, -e10);
            blog2 = K - e10;                        /* m*5^-e10 vs a, times 2^e10 */
        }
    }
    if (alog2 > blog2) big_shl(&a, alog2 - blog2);
    else if (blog2 > alog2) big_shl(&b, blog2 - alog2);
    r = big_cmp(&a, &b);
    free(a.w);
    free(b.w);
    return r;
}

float strtof(const char *nptr, char **endptr) {
    int saved = errno;
    char *end;
    double d;
    float f;
    double ad;

    errno = 0;
    d = dtoa_strtod(nptr, &end);
    if (endptr) *endptr = end;
    if (errno == 0) errno = saved;
    if (end == nptr || d != d || d == 0 || (d - d) != 0) {
        /* nothing read, a NaN, a zero, or an infinity: as strtod had it */
        return (float)d;
    }
    f = (float)d;
    ad = d < 0 ? -d : d;
    if ((f - f) != 0) {                             /* overflow */
        errno = ERANGE;
        return d < 0 ? -HUGE_VALF : HUGE_VALF;
    }
    /* d on a midpoint between adjacent floats?  Its bits below the float's
     * last place are then exactly one half of that place. */
    {
        uint64_t bits;
        int e2;
        int drop;                                   /* bits of d below float precision */
        memcpy(&bits, &ad, sizeof bits);
        e2 = (int)(bits >> 52);                     /* biased; d is normal (|d| >= 2^-149 at least) */
        if (e2 == 0) {
            drop = -1;                              /* a double subnormal: far below any float */
        } else {
            int unb = e2 - 1023;                    /* d = 1.f * 2^unb */
            if (unb >= -126) drop = 52 - 23;        /* a normal float keeps 24 bits */
            else drop = 52 - 23 + (-126 - unb);     /* a subnormal float keeps fewer */
            if (drop > 53) drop = -1;               /* below half the least subnormal */
        }
        if (drop > 0 && drop <= 53) {
            uint64_t sig = (bits & 0xFFFFFFFFFFFFFull) | (1ull << 52);
            uint64_t rest = sig & ((1ull << drop) - 1);
            if (rest == (1ull << (drop - 1))) {
                /* the midpoint m = sig * 2^(unb-52), with sig even past
                 * the half bit; compare the input with it exactly */
                long K = (long)(e2 - 1023) - 52;
                int c = cmp_midpoint(nptr, sig, K);
                if (c != 2 && c != 0) {
                    float lo, hi;
                    uint32_t fb;
                    /* the floats on each side: lo is the truncation */
                    if (drop == 53) {
                        lo = 0.0f;                  /* d is half the least subnormal */
                    } else {
                        double trunc_d;
                        uint64_t tb = bits & ~((1ull << drop) - 1);
                        memcpy(&trunc_d, &tb, sizeof trunc_d);
                        lo = (float)trunc_d;        /* exact */
                    }
                    memcpy(&fb, &lo, sizeof fb);
                    fb++;
                    memcpy(&hi, &fb, sizeof hi);
                    f = c > 0 ? hi : lo;
                    if (d < 0) f = -f;
                    if ((f - f) != 0) { errno = ERANGE; return d < 0 ? -HUGE_VALF : HUGE_VALF; }
                }
            }
        }
    }
    if (f == 0 || (f < FLT_MIN && f > -FLT_MIN)) errno = ERANGE;    /* underflow */
    return f;
}
