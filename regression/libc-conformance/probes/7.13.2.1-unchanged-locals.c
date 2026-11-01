/* 7.13.2.1p3: locals not changed between setjmp and longjmp keep their values even when many are live and the jump comes from deep calls */
#include <setjmp.h>
#include <stdio.h>

static jmp_buf env;
static volatile int seed = 3;

static unsigned churn(int n, unsigned x) {
    unsigned a = x + 1, b = x * 3, c = x ^ 0x55, d = x - 9, e = x * x, f = a + b, g = c + d, h = e - f;
    if (n == 0) longjmp(env, 1);
    return churn(n - 1, a + b + c + d + e + f + g + h) + a + b + c + d + e + f + g + h;
}

int main(void) {
    int s = seed;
    int a = s * 2, b = s * 3, c = s * 5, d = s * 7, e = s * 11, f = s * 13, g = s * 17, h = s * 19;
    int i = s * 23, j = s * 29, k = s * 31, l = s * 37, m = s * 41, n = s * 43, o = s * 47, p = s * 53;
    long long q = (long long)s << 33;
    double r = s * 0.125;
    if (setjmp(env) == 0) {
        churn(30, s);
        return 1;
    }
    printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
    printf("%d %d %d %d %d %d %d %d\n", i, j, k, l, m, n, o, p);
    printf("%lld %g\n", q, r);
    return 0;
}
