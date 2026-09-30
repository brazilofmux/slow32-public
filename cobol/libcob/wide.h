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

/* isf: a floating-point value in f instead (COMP-1/COMP-2, docs/usage.md):
 * the wide stack computes in double once any operand is one */
typedef struct { wl_t m[WL]; int neg; int scale; int isf; double f; } cob_wnum;

/* every function static, and unused ones no warning: each includer takes what it needs */
#define WFN static __attribute__((unused))

/* ---- limb arrays ---------------------------------------------------- */

WFN int mp_is_zero(const wl_t *a, int n) { for (int i = 0; i < n; i++) if (a[i]) return 0; return 1; }

WFN int mp_cmp(const wl_t *a, const wl_t *b, int n)
{
    for (int i = n - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
    return 0;
}

/* a += b; the carry out */
WFN wl_t mp_add(wl_t *a, const wl_t *b, int n)
{
    unsigned long long c = 0;
    for (int i = 0; i < n; i++) { c += (unsigned long long)a[i] + b[i]; a[i] = (wl_t)c; c >>= 32; }
    return (wl_t)c;
}

/* a -= b, a >= b */
WFN void mp_sub(wl_t *a, const wl_t *b, int n)
{
    long long br = 0;
    for (int i = 0; i < n; i++) {
        long long d = (long long)a[i] - b[i] - br;
        br = d < 0; a[i] = (wl_t)(d + (br ? 0x100000000LL : 0));
    }
}

/* a = a * m + add; the carry out */
WFN wl_t mp_mul_small(wl_t *a, int n, wl_t m, wl_t add)
{
    unsigned long long c = add;
    for (int i = 0; i < n; i++) { c += (unsigned long long)a[i] * m; a[i] = (wl_t)c; c >>= 32; }
    return (wl_t)c;
}

/* a /= d; the remainder */
WFN wl_t mp_div_small(wl_t *a, int n, wl_t d)
{
    unsigned long long r = 0;
    for (int i = n - 1; i >= 0; i--) { r = (r << 32) | a[i]; a[i] = (wl_t)(r / d); r %= d; }
    return (wl_t)r;
}

/* r[na+nb] = a * b */
WFN void mp_mul(const wl_t *a, int na, const wl_t *b, int nb, wl_t *r)
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

WFN int mp_bits(const wl_t *a, int n)
{
    for (int i = n - 1; i >= 0; i--)
        if (a[i]) { int b = 32; while (!(a[i] >> (b - 1))) b--; return i * 32 + b; }
    return 0;
}

/* q = a / b, r = a % b, all n limbs, b != 0: shift and subtract, a bit
 * at a time past 64 bits -- the wide path is rare enough that this is fine */
WFN void mp_divmod(const wl_t *a, const wl_t *b, int n, wl_t *q, wl_t *r)
{
    for (int i = 0; i < n; i++) { q[i] = 0; r[i] = 0; }
    /* the usual cases first: a divisor of one limb, and both in 64 bits */
    if (mp_bits(b, n) <= 32) {
        for (int i = 0; i < n; i++) q[i] = a[i];
        r[0] = mp_div_small(q, n, b[0]);
        return;
    }
    if (n >= 2 && mp_bits(a, n) <= 64 && mp_bits(b, n) <= 64) {
        unsigned long long x = ((unsigned long long)a[1] << 32) | a[0], y = ((unsigned long long)b[1] << 32) | b[0];
        unsigned long long qq = x / y, rr = x % y;
        q[0] = (wl_t)qq; q[1] = (wl_t)(qq >> 32); r[0] = (wl_t)rr; r[1] = (wl_t)(rr >> 32);
        return;
    }
    for (int bit = mp_bits(a, n) - 1; bit >= 0; bit--) {
        /* r = r << 1 | the bit */
        wl_t c = (a[bit / 32] >> (bit % 32)) & 1;
        for (int i = 0; i < n; i++) { wl_t nc = r[i] >> 31; r[i] = (r[i] << 1) | c; c = nc; }
        if (mp_cmp(r, b, n) >= 0) { mp_sub(r, b, n); q[bit / 32] |= 1u << (bit % 32); }
    }
}

/* ---- decimal ---------------------------------------------------------- */

/* the magnitude 10^k, k <= 38 */
WFN void w_pow10(wl_t *a, int k)
{
    a[0] = 1; for (int i = 1; i < WL; i++) a[i] = 0;
    for (; k >= 9; k -= 9) mp_mul_small(a, WL, 1000000000u, 0);
    static const wl_t p[9] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 };
    if (k) mp_mul_small(a, WL, p[k], 0);
}

/* the magnitude's digits, n of them, leading zeros; the high ones are
 * lost if it has more.  Returns how many of the n are significant (0 for
 * zero), which saves the callers a scan of the leading zeros. */
WFN int w_to_digits(const wl_t *mag, char *out, int n)
{
    wl_t t[WL]; memcpy(t, mag, sizeof t);
    int top = WL; while (top > 0 && !t[top - 1]) top--;     /* the limbs still nonzero */
    int i = n;
    while (i > 0 && top > 0) {
        wl_t r = top == 1 ? t[0] % 1000000000u : mp_div_small(t, top, 1000000000u);
        if (top == 1) t[0] /= 1000000000u;
        while (top > 0 && !t[top - 1]) top--;
        for (int k = 0; k < 9 && i > 0; k++) { out[--i] = (char)('0' + r % 10); r /= 10; }
    }
    if (i > 0) memset(out, '0', (size_t)i);                  /* the value ran out: leading zeros */
    while (i < n && out[i] == '0') i++;                     /* at most the last chunk's */
    return n - i;
}

/* the magnitude of n digits (characters '0'..'9') */
WFN void w_from_digits(wl_t *mag, const char *d, int n)
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

WFN void w_from_i64(cob_wnum *w, long long v, int scale)
{
    unsigned long long u = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
    w->m[0] = (wl_t)u; w->m[1] = (wl_t)(u >> 32); w->m[2] = w->m[3] = 0;
    w->neg = v < 0; w->scale = scale; w->isf = 0;
}

/* does the magnitude fit in 63 bits (for a narrow value)? */
WFN int w_fits_i64(const cob_wnum *w) { return !w->m[2] && !w->m[3] && !(w->m[1] >> 31); }
WFN long long w_to_i64(const cob_wnum *w)
{
    long long v = (long long)(((unsigned long long)w->m[1] << 32) | w->m[0]);
    return w->neg ? -v : v;
}

/* the number of decimal digits in the magnitude (0 for zero) */
WFN int w_ndigits(const wl_t *mag)
{
    char d[40];
    return w_to_digits(mag, d, 39);
}

/* mag /= 10^k; the remainder's comparison with half of 10^k for ROUNDED:
 * *half gets -1 below half, 0 exactly half, 1 above; *nonzero whether
 * anything was dropped */
WFN void w_drop_digits(wl_t *mag, int n, int k, int *half, int *nonzero)
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
WFN int w_scale_up(wl_t *mag, int k)
{
    for (int i = 0; i < k; i++) if (mp_mul_small(mag, WL, 10, 0)) return 0;
    return !(mag[WL - 1] >> 31);
}

#endif
