/* 7.16.1.4: va_start after a last named parameter of type long long, double or pointer, or after many named parameters */
#include <stdarg.h>
#include <stdio.h>

static int after_ll(long long x, ...) {
    va_list ap; int r;
    va_start(ap, x); r = va_arg(ap, int); va_end(ap);
    return r + (int)(x >> 40);
}
static double after_d(double x, ...) {
    va_list ap; double r;
    va_start(ap, x); r = va_arg(ap, double); va_end(ap);
    return r + x;
}
static int after_p(const char *p, ...) {
    va_list ap; int r;
    va_start(ap, p); r = va_arg(ap, int); va_end(ap);
    return r + p[0];
}
static int after_nine(int a, int b, int c, int d, int e, int f, int g, int h, int i, ...) {
    va_list ap; int r;
    va_start(ap, i); r = va_arg(ap, int); r += va_arg(ap, int); va_end(ap);
    return r + a + b + c + d + e + f + g + h + i;
}

int main(void) {
    printf("%d %g %d %d\n", after_ll(3LL << 40, 4), after_d(1.5, 2.25), after_p("A", 1),
           after_nine(1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 1000));
    return 0;
}
