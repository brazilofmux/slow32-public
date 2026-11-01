/* Variadic arguments the va_list walk got wrong (libc-conformance
 * 7.16.1.1-va-arg-struct, 7.16.1.4-va-start-param-types):
 *  - a structure through the ellipsis travels as the address of the
 *    caller's copy, in the next word slot (clang's convention too);
 *  - va_start after a long long or double last parameter, and after more
 *    than eight words of fixed parameters, lands on the first variadic
 *    argument. */
#include <stdarg.h>
typedef struct { int a, b, c; } Trio;
typedef struct { char tag; double d; } Mixed;

static int trio_sum(int n, ...) {
    va_list ap;
    Trio t;
    int after;
    va_start(ap, n);
    t = va_arg(ap, Trio);
    after = va_arg(ap, int);
    va_end(ap);
    return n * 1000 + t.a * 100 + t.b * 10 + t.c + after;
}

static double mixed(int k, ...) {
    va_list ap;
    Mixed m;
    double tail;
    va_start(ap, k);
    m = va_arg(ap, Mixed);
    tail = va_arg(ap, double);
    va_end(ap);
    return k + m.tag + m.d + tail;
}

static int after_ll(long long x, ...) {
    va_list ap;
    int a, b;
    va_start(ap, x);
    a = va_arg(ap, int);
    b = va_arg(ap, int);
    va_end(ap);
    return (int)x * 100 + a * 10 + b;
}

static int after_double(double d, ...) {
    va_list ap;
    int a;
    va_start(ap, d);
    a = va_arg(ap, int);
    va_end(ap);
    return (int)(d * 2) * 10 + a;
}

static int after_nine(int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8, int p9, ...) {
    va_list ap;
    int a;
    va_start(ap, p9);
    a = va_arg(ap, int);
    va_end(ap);
    return p1 + p9 + a;
}

int main(void) {
    Trio t = { 7, 8, 9 };
    Mixed m = { 3, 0.5 };
    if (trio_sum(1, t, 42) != 1000 + 789 + 42) return 1;
    t.a = 1;
    if (trio_sum(2, t, 5) != 2000 + 189 + 5) return 2;
    if (mixed(1, m, 0.25) != 1 + 3 + 0.5 + 0.25) return 3;
    if (after_ll(1LL, 4, 5) != 145) return 4;
    if (after_double(1.5, 6) != 36) return 5;
    if (after_nine(1, 2, 3, 4, 5, 6, 7, 8, 9, 40) != 50) return 6;
    return 0;
}
