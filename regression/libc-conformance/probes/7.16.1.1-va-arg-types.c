/* 7.16.1.1: va_arg fetches int, unsigned, long, long long, double, pointers and promoted char, short and float in order */
#include <stdarg.h>
#include <stdio.h>

static void show(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int i = va_arg(ap, int);
    unsigned u = va_arg(ap, unsigned);
    long l = va_arg(ap, long);
    long long ll = va_arg(ap, long long);
    double d = va_arg(ap, double);
    const char *s = va_arg(ap, const char *);
    int c = va_arg(ap, int);
    int sh = va_arg(ap, int);
    double f = va_arg(ap, double);
    unsigned long long ull = va_arg(ap, unsigned long long);
    int last = va_arg(ap, int);
    va_end(ap);
    printf("%d %d %u %ld %lld %g %s %c %d %g %llu %d\n", n, i, u, l, ll, d, s, c, sh, f, ull, last);
}

int main(void) {
    char c = 'q';
    short sh = -12;
    float f = 1.5f;
    show(1, -3, 4000000000u, -70000L, -5000000000LL, 3.25, "str", c, sh, f, 18000000000000000000ull, 42);
    return 0;
}
