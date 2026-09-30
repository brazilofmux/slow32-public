/* wide_test.c -- libcob/wide.h against the host's unsigned __int128 on
 * random operands (docs/wide.md).  Built on the host by run-tests.sh, as
 * bt_test is: cc -I libcob tests/wide_test.c && ./wide_test. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wide.h"

typedef unsigned __int128 u128;

static unsigned long long rs = 88172645463325252ULL;
static unsigned long long rnd(void) { rs ^= rs << 13; rs ^= rs >> 7; rs ^= rs << 17; return rs; }
static u128 to128(const wl_t *m) { u128 v = 0; for (int i = WL - 1; i >= 0; i--) v = (v << 32) | m[i]; return v; }
static void from128(wl_t *m, u128 v) { for (int i = 0; i < WL; i++) { m[i] = (wl_t)v; v >>= 32; } }
/* a random magnitude below 10^digits */
static u128 rmag(int digits)
{
    u128 lim = 1; for (int i = 0; i < digits; i++) lim *= 10;
    u128 v = ((u128)rnd() << 64) | rnd();
    return v % lim;
}
static int fails;
#define CHECK(c, ...) do { if (!(c)) { if (fails++ < 10) { printf(__VA_ARGS__); printf("\n"); } } } while (0)

int main(void)
{
    for (int it = 0; it < 200000; it++) {
        int da = 1 + (int)(rnd() % 38), db = 1 + (int)(rnd() % 38);
        u128 x = rmag(da), y = rmag(db);
        wl_t a[WL], b[WL], t[WL], q[WL], r[WL];
        from128(a, x); from128(b, y);
        /* add, sub, cmp */
        memcpy(t, a, sizeof t); wl_t c = mp_add(t, b, WL);
        CHECK(!c && to128(t) == x + y, "add");
        CHECK(mp_cmp(a, b, WL) == (x < y ? -1 : x > y), "cmp");
        if (x >= y) { memcpy(t, a, sizeof t); mp_sub(t, b, WL); CHECK(to128(t) == x - y, "sub"); }
        /* small multiply and divide */
        wl_t m = (wl_t)(rnd() % 1000000000u) + 1;
        memcpy(t, a, sizeof t); wl_t rr = mp_div_small(t, WL, m);
        CHECK(to128(t) == x / m && rr == (wl_t)(x % m), "div_small");
        if (da <= 28) { memcpy(t, a, sizeof t); mp_mul_small(t, WL, m, 7); CHECK(to128(t) == x * m + 7, "mul_small"); }
        /* full multiply, checked in two halves when it fits 128 bits */
        if (da + db <= 38) { wl_t p[2 * WL]; mp_mul(a, WL, b, WL, p); CHECK(to128(p) == x * y && mp_is_zero(p + WL, WL), "mul"); }
        /* division */
        if (y) { mp_divmod(a, b, WL, q, r); CHECK(to128(q) == x / y && to128(r) == x % y, "divmod"); }
        /* digits both ways */
        char d[40], e[40]; w_to_digits(a, d, 38);
        u128 v = x; for (int i = 37; i >= 0; i--) { e[i] = (char)('0' + (int)(v % 10)); v /= 10; }
        CHECK(!memcmp(d, e, 38), "to_digits");
        wl_t f[WL]; w_from_digits(f, d, 38); CHECK(to128(f) == x, "from_digits");
        CHECK(w_ndigits(a) == (x ? (int)strspn(d, "0") * -1 + 38 : 0), "ndigits");
        /* dropping and scaling digits */
        int k = (int)(rnd() % 10), half, nz;
        memcpy(t, a, sizeof t); w_drop_digits(t, WL, k, &half, &nz);
        u128 p10 = 1; for (int i = 0; i < k; i++) p10 *= 10;
        u128 rem = x % p10;
        CHECK(to128(t) == x / p10, "drop");
        CHECK(nz == (k > 0 && rem != 0), "drop nonzero");
        if (k) CHECK(half == (rem * 2 > p10 ? 1 : rem * 2 == p10 ? 0 : -1), "drop half");
        if (da + k <= 38) { memcpy(t, a, sizeof t); CHECK(w_scale_up(t, k) && to128(t) == x * p10, "scale_up"); }
    }
    /* the edges random operands never reach: limb boundaries, where a
     * carry or a borrow runs the length of the number, and powers of ten */
    u128 edge[64]; int ne = 0;
    edge[ne++] = 0; edge[ne++] = 1;
    for (int k = 32; k <= 96; k += 32) { u128 v = (u128)1 << k; edge[ne++] = v - 1; edge[ne++] = v; edge[ne++] = v + 1; }
    { u128 v = 1; for (int k = 0; k <= 38; k += 3) { edge[ne++] = v; edge[ne++] = v - 1 + (v == 0); for (int j = 0; j < 3; j++) v *= 10; } }
    for (int i = 0; i < ne; i++) for (int j = 0; j < ne; j++) {
        u128 x = edge[i], y = edge[j];
        wl_t a[WL], b[WL], t[WL], q[WL], r[WL];
        from128(a, x); from128(b, y);
        if (x >= y) { memcpy(t, a, sizeof t); mp_sub(t, b, WL); CHECK(to128(t) == x - y, "edge sub"); }
        memcpy(t, a, sizeof t); mp_add(t, b, WL); CHECK(to128(t) == x + y, "edge add");
        if (y) { mp_divmod(a, b, WL, q, r); CHECK(to128(q) == x / y && to128(r) == x % y, "edge divmod"); }
        CHECK(mp_cmp(a, b, WL) == (x < y ? -1 : x > y), "edge cmp");
    }
    wl_t p[WL]; w_pow10(p, 38); u128 e38 = 1; for (int i = 0; i < 38; i++) e38 *= 10;
    CHECK(to128(p) == e38, "pow10");
    if (fails) { printf("wide_test: %d failures\n", fails); return 1; }
    printf("wide_test: 200000 random operand pairs and %d edge pairs agree with __int128\n", ne * ne);
    return 0;
}
