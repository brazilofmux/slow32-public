/* 7.12.10.2-7.12.10.3 remainder, remquo: IEC 60559 remainder (quotient rounded to nearest, ties even) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    int q = 0;
    printf("%g %g\n", remainder(v(5.0), v(2.0)), remainder(v(7.0), v(2.0)));
    double r = remquo(v(10.0), v(3.0), &q);
    printf("%g %d\n", r, q & 7);
    printf("%g\n", (double)remainderf(5.5f, 2.0f));
    return 0;
}
