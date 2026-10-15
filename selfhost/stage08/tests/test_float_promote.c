/* A float in a variadic call's `...` is passed as a double (the
 * default argument promotions, C90 6.3.2.2).
 *
 * sema converted each argument to its declared parameter's type and
 * left one with no declared parameter as it was, so a float reached the
 * callee as its 32-bit pattern where va_arg(ap, double) reads a register
 * pair: printf("%f", f) printed 0.000000 or a huge number.  DOOM's
 * -timedemo summary showed it -- "(0.000000 fps)" from the stage08
 * build, the right figure from clang's.  Returns the first failing
 * check, 0 if all pass. */

#include <stdarg.h>

static double nth(int n, ...) {
    va_list ap;
    double d;
    int i;
    d = 0.0;
    va_start(ap, n);
    i = 0;
    while (i <= n) {
        d = va_arg(ap, double);
        i = i + 1;
    }
    va_end(ap);
    return d;
}

/* Mixed: an int and a pointer either side of the promoted float. */
static int mixed(int k, ...) {
    va_list ap;
    int a;
    double d;
    int *p;
    va_start(ap, k);
    a = va_arg(ap, int);
    d = va_arg(ap, double);
    p = va_arg(ap, int *);
    va_end(ap);
    if (a != 5026) return 1;
    if (d != 2.5) return 2;
    if (*p != k) return 3;
    return 0;
}

static double (*nth_ptr)(int, ...) = nth;

int main(void) {
    volatile int g = 5026;
    volatile int h = 74;
    float fps;
    float lit;
    int k;
    fps = ((float) g * 35) / h;
    lit = 2.5f;
    k = 77;
    if (nth(0, fps) != (double) fps) return 10;
    if (nth(0, lit) != 2.5) return 11;
    if (nth(1, lit, fps) != (double) fps) return 12;
    if (nth(2, 1.0, lit, (float) h) != 74.0) return 13;
    if (mixed(k, g, lit, &k) != 0) return 14;
    if (nth_ptr(0, lit) != 2.5) return 15;
    if (nth_ptr(1, fps, lit) != 2.5) return 16;
    return 0;
}
