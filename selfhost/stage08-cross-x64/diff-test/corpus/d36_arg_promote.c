/* Implicit argument conversions at call sites (issue #6): an int
 * argument bound to a long long parameter must be widened (sign- or
 * zero-extended) to a full 64-bit value, an int bound to a double
 * parameter must be converted, and a long long bound to an int
 * parameter must be truncated without shifting the later argument
 * slots.  Before the fix the high half was whatever the register or
 * slot happened to hold (silent miscompile on slow32 pairs; stale
 * upper bits on 64-bit hosts).  */
int printf(char *fmt, ...);

long long mul(long long a, long long b) { return a * b; }
long long ident64(long long v) { return v; }
double dsum(double a, double b) { return a + b; }
int isink(int a, int b, int c) { return a * 100 + b * 10 + c; }
long long lsink(int w, long long x, int y, long long z)
{
    return (long long)w + x * 100 + (long long)y * 10000 + z * 1000000;
}

static void show64(char *label, long long v)
{
    printf("%s %08x%08x\n", label,
           (unsigned)((unsigned long long)v >> 32), (unsigned)v);
}

static void showd(char *label, double v)
{
    unsigned long long bits = *(unsigned long long *)&v;
    printf("%s %08x%08x\n", label, (unsigned)(bits >> 32), (unsigned)bits);
}

int main(void)
{
    int k = 7;
    int neg = -5;
    unsigned int u = 3000000000u;
    long long big = 0x500000004LL;

    show64("mul-var", mul(k, 7));        /* int,int -> ll,ll: 49 */
    show64("mul-lit", mul(6, 8));        /* literals: 48 */
    show64("sext", ident64(neg));        /* sign-extend: ffff...fffb */
    show64("zext", ident64(u));          /* unsigned zero-extend */
    showd("i2d", dsum(1, 2.5));          /* int -> double: 3.5 */
    showd("i2d-var", dsum(k, 0.5));      /* variable int -> double: 7.5 */
    printf("trunc %d\n", isink(big, 2, 3));      /* ll -> int, no slot shift */
    show64("mix", lsink(1, 2, 3, 4));    /* 4030201 */
    return 0;
}
