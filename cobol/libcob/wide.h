/* wide.h -- 128-bit decimal-scaled numbers for the items of 19 to 31
 * digits (COBOL 2002; docs/wide.md, cobol ISSUES-117).
 *
 * A value is a sign, a scale and a magnitude of four 32-bit limbs, least
 * significant first.  10^38 < 2^127, so 38 digits hold.  SLOW-32 has no
 * 128-bit integer type, so the arithmetic is written out here on limb
 * arrays of any length (the full product of two magnitudes takes eight).
 * Everything is static: libcob.c includes this, and so does the host test
 * tests/wide_test.c, which checks it against unsigned __int128. */
#ifndef COB_WIDE_H
#define COB_WIDE_H

#include <string.h>

#define WL 4                            /* limbs in a magnitude */
typedef unsigned int wl_t;              /* one limb */

typedef struct { wl_t m[WL]; int neg; int scale; } cob_wnum;

/* ---- limb arrays ---------------------------------------------------- */

static int mp_is_zero(const wl_t *a, int n) { for (int i = 0; i < n; i++) if (a[i]) return 0; return 1; }

static int mp_cmp(const wl_t *a, const wl_t *b, int n)
{
    for (int i = n - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
    return 0;
}

/* a += b; the carry out */
static wl_t mp_add(wl_t *a, const wl_t *b, int n)
{
    unsigned long long c = 0;
    for (int i = 0; i < n; i++) { c += (unsigned long long)a[i] + b[i]; a[i] = (wl_t)c; c >>= 32; }
    return (wl_t)c;
}

/* a -= b, a >= b */
static void mp_sub(wl_t *a, const wl_t *b, int n)
{
    long long br = 0;
    for (int i = 0; i < n; i++) {
        long long d = (long long)a[i] - b[i] - br;
        br = d < 0; a[i] = (wl_t)(d + (br ? 0x100000000LL : 0));
    }
}

/* a = a * m + add; the carry out */
static wl_t mp_mul_small(wl_t *a, int n, wl_t m, wl_t add)
{
    unsigned long long c = add;
    for (int i = 0; i < n; i++) { c += (unsigned long long)a[i] * m; a[i] = (wl_t)c; c >>= 32; }
    return (wl_t)c;
}

/* a /= d; the remainder */
static wl_t mp_div_small(wl_t *a, int n, wl_t d)
{
    unsigned long long r = 0;
    for (int i = n - 1; i >= 0; i--) { r = (r << 32) | a[i]; a[i] = (wl_t)(r / d); r %= d; }
    return (wl_t)r;
}

/* r[na+nb] = a * b */
static void mp_mul(const wl_t *a, int na, const wl_t *b, int nb, wl_t *r)
{
    for (int i = 0; i < na + nb; i++) r[i] = 0;
    for (int i = 0; i < na; i++) {
        if (!a[i]) continue;
        unsigned long long c = 0;
        for (int j = 0; j < nb; j++) {
            c += (unsigned long long)a[i] * b[j] + r[i + j];
            r[i + j] = (wl_t)c; c >>= 32;
        }
        r[i + nb] = (wl_t)c;
    }
}

static int mp_bits(const wl_t *a, int n)
{
    for (int i = n - 1; i >= 0; i--)
        if (a[i]) { int b = 32; while (!(a[i] >> (b - 1))) b--; return i * 32 + b; }
    return 0;
}

/* q = a / b, r = a % b, all n limbs, b != 0: shift and subtract, a bit
 * at a time -- the wide path is rare enough that this is fine */
static void mp_divmod(const wl_t *a, const wl_t *b, int n, wl_t *q, wl_t *r)
{
    for (int i = 0; i < n; i++) { q[i] = 0; r[i] = 0; }
    for (int bit = mp_bits(a, n) - 1; bit >= 0; bit--) {
        /* r = r << 1 | the bit */
        wl_t c = (a[bit / 32] >> (bit % 32)) & 1;
        for (int i = 0; i < n; i++) { wl_t nc = r[i] >> 31; r[i] = (r[i] << 1) | c; c = nc; }
        if (mp_cmp(r, b, n) >= 0) { mp_sub(r, b, n); q[bit / 32] |= 1u << (bit % 32); }
    }
}

/* ---- decimal ---------------------------------------------------------- */

/* the magnitude 10^k, k <= 38 */
static void w_pow10(wl_t *a, int k)
{
    a[0] = 1; for (int i = 1; i < WL; i++) a[i] = 0;
    for (; k >= 9; k -= 9) mp_mul_small(a, WL, 1000000000u, 0);
    static const wl_t p[9] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 };
    if (k) mp_mul_small(a, WL, p[k], 0);
}

/* the magnitude's digits, n of them, leading zeros; the high ones are
 * lost if it has more */
static void w_to_digits(const wl_t *mag, char *out, int n)
{
    wl_t t[WL]; memcpy(t, mag, sizeof t);
    int i = n;
    while (i > 0) {
        wl_t r = mp_div_small(t, WL, 1000000000u);
        for (int k = 0; k < 9 && i > 0; k++) { out[--i] = (char)('0' + r % 10); r /= 10; }
    }
}

/* the magnitude of n digits (characters '0'..'9') */
static void w_from_digits(wl_t *mag, const char *d, int n)
{
    for (int i = 0; i < WL; i++) mag[i] = 0;
    int i = 0;
    while (i < n) {
        int len = n - i < 9 ? n - i : 9;
        wl_t w = 0, m = 1;
        for (int k = 0; k < len; k++) { w = w * 10 + (wl_t)(d[i + k] - '0'); m *= 10; }
        mp_mul_small(mag, WL, m, w);
        i += len;
    }
}

static void w_from_i64(cob_wnum *w, long long v, int scale)
{
    unsigned long long u = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
    w->m[0] = (wl_t)u; w->m[1] = (wl_t)(u >> 32); w->m[2] = w->m[3] = 0;
    w->neg = v < 0; w->scale = scale;
}

/* does the magnitude fit in 63 bits (for a narrow value)? */
static int w_fits_i64(const cob_wnum *w) { return !w->m[2] && !w->m[3] && !(w->m[1] >> 31); }
static long long w_to_i64(const cob_wnum *w)
{
    long long v = (long long)(((unsigned long long)w->m[1] << 32) | w->m[0]);
    return w->neg ? -v : v;
}

/* the number of decimal digits in the magnitude (0 for zero) */
static int w_ndigits(const wl_t *mag)
{
    if (mp_is_zero(mag, WL)) return 0;
    char d[40]; w_to_digits(mag, d, 39);
    int i = 0; while (i < 39 && d[i] == '0') i++;
    return 39 - i;
}

/* mag /= 10^k; the remainder's comparison with half of 10^k for ROUNDED:
 * *half gets -1 below half, 0 exactly half, 1 above; *nonzero whether
 * anything was dropped */
static void w_drop_digits(wl_t *mag, int n, int k, int *half, int *nonzero)
{
    int first = -1, rest = 0;
    for (int i = 0; i < k; i++) {
        wl_t r = mp_div_small(mag, n, 10);
        if (i == k - 1) first = (int)r; else if (r) rest = 1;
    }
    if (half) *half = k <= 0 ? -1 : first > 5 || (first == 5 && rest) ? 1 : first == 5 ? 0 : -1;
    if (nonzero) *nonzero = k > 0 && (first > 0 || rest);
}

/* mag *= 10^k; 0 if it no longer fits WL limbs */
static int w_scale_up(wl_t *mag, int k)
{
    for (int i = 0; i < k; i++) if (mp_mul_small(mag, WL, 10, 0)) return 0;
    return !(mag[WL - 1] >> 31);
}

#endif
