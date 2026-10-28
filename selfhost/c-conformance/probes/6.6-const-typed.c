// 6.6p6,8  constant expressions in their C types: casts, unsigned and 64-bit arithmetic, float rounding
#include <stdio.h>
enum { E1 = (int)(sizeof(long long) * 3), E2 = -7 / 2, E3 = (unsigned char)300 };
static float f1 = 1.0f / 3;
static double d1 = (float)0.1 + 0.2;
static double d2 = 18446744073709551615ULL;
static double d3 = -9223372036854775807LL - 1;
static double d4 = 1 ? 2.5 * 2 : 1.0;
static unsigned char uc = (unsigned char)300;
static short sh = (short)70000;
static unsigned u1 = -1u >> 1;
static long long ll = (long long)0xFFFFFFFFu * 0xFFFFFFFFu;
static unsigned long long ull = -1ULL / 3;
static int iv = (int)2.75 + 'a' % 7;
static double tab[] = { 1.0 / 8, 3 * 0.5, -(1 + 0.25) };
static struct { int i; double d; float f; } st = { 1 << 4, 1.0 / 3, 2.0f / 3 };
static char arr[E1 + (int)sizeof(double)];
static float fl = 2.5f;
static double fd = -0.75f;
static float ftab[] = { 1.5f, -2.0f, 3 };
int main(void) {
    long long k = 1;
    printf("%d %d %d %.9g %.17g\n", E1, E2, E3, f1, d1);
    printf("%.17g %.17g %g\n", d2, d3, d4);
    printf("%u %d %u %lld %llu %d\n", uc, sh, u1, ll, ull, iv);
    printf("%g %g %g %d %.17g %.9g %u\n", tab[0], tab[1], tab[2], st.i, st.d, st.f, (unsigned)sizeof arr);
    printf("%g %g %g %g %g\n", fl, fd, ftab[0], ftab[1], ftab[2]);
    switch (k << 33) { case 1LL << 33: printf("case\n"); break; default: printf("default\n"); }
    return 0;
}
