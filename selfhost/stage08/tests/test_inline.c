/* Inliner semantics: early return, loops+break in callee, side-effect
 * ordering, short-circuit conditionality, nesting, recursion refusal,
 * locals with distinct instances, doubles, address-taken locals. */
int g_seq;
int tick(int v) { g_seq = g_seq * 10 + v; return v; }

int early(int x) { if (x > 3) return 100; return x; }

int sumto(int n) { int i, s; s = 0; for (i = 0; i < n; i++) { if (i == 7) break; s = s + i; } return s; }

int viaptr(int x) { int loc; int *p; loc = x; p = &loc; *p = *p + 5; return loc; }

double dscale(double v, double k) { return v * k + 1.0; }

int fact(int n) { if (n <= 1) return 1; return n * fact(n - 1); }

int arr7(int k) { int a[8]; int i; for (i = 0; i < 8; i++) a[i] = i * k; return a[7]; }

int main(void) {
    int fails = 0;
    double d;

    if (early(2) != 2) fails |= 1;
    if (early(9) != 100) fails |= 2;
    if (sumto(5) != 10) fails |= 4;
    if (sumto(20) != 21) fails |= 8;      /* breaks at i==7 */
    if (viaptr(10) != 15) fails |= 16;
    if (fact(5) != 120) fails |= 32;      /* recursive: must not inline */
    if (arr7(3) != 21) fails |= 64;

    d = dscale(2.5, 4.0);
    if (d != 11.0) fails |= 128;

    /* side-effect ordering across an inlined call */
    g_seq = 0;
    if (tick(1) + tick(2) * 0 + tick(3) != 4) fails |= 256;
    if (g_seq != 123) fails |= 512;

    /* short circuit: the inlined call must NOT run */
    g_seq = 0;
    if (0 && tick(9)) fails |= 1024;
    if (g_seq != 0) fails |= 2048;

    /* nested inlining */
    if (early(sumto(5)) != 100) fails |= 4096;

    /* two instances of the same callee keep separate locals */
    if (viaptr(1) + viaptr(2) != 13) fails |= 8192;

    return fails;
}
