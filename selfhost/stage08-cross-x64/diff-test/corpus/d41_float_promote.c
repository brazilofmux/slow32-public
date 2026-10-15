/* A float in a variadic call's `...` is promoted to double (C90
 * 6.3.2.2).  The shared sema converted an argument only to a declared
 * parameter's type, so a float past the prototype went over as a float
 * and va_arg(ap, double) read something else: printf("%f", f) printed
 * 0.000000 on SLOW-32.  Floats alone, among ints and pointers, through
 * printf, and through a pointer to a variadic function. */
#include <stdarg.h>
#include <stdio.h>

static double nth(int n, ...) {
    va_list ap;
    double d = 0.0;
    int i;
    va_start(ap, n);
    for (i = 0; i <= n; i = i + 1) d = va_arg(ap, double);
    va_end(ap);
    return d;
}

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
    return a + (int)(d * 4.0) + *p;
}

static double (*nth_ptr)(int, ...) = nth;

int main(void) {
    volatile int g = 5026;
    volatile int h = 74;
    float rate = ((float) g * 35) / h;
    float half = 2.5f;
    int k = 77;
    printf("%.3f %.1f\n", rate, half);
    printf("%d %.3f %d %.1f\n", g, rate, h, half);
    printf("%.3f\n", nth(0, rate));
    printf("%.3f\n", nth(2, 1.0, half, (float) h));
    printf("%d\n", mixed(k, g, half, &k));
    printf("%.3f %.3f\n", nth_ptr(0, half), nth_ptr(1, rate, half));
    return 0;
}
