/* 7.12.13.1 fma: x*y+z computed as if exactly and rounded once */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double a = 1.0 + 2.220446049250313e-16, b = 1.0 - 2.220446049250313e-16;
    printf("%.17g\n", fma(v(a), v(b), v(-1.0)));
    printf("%.17g\n", fma(v(0.1), v(10.0), v(-1.0)));
    printf("%.17g\n", fma(v(2.0), v(3.0), v(4.0)));
    printf("%.9g\n", (double)fmaf(1.0f + 1.1920929e-7f, 1.0f - 1.1920929e-7f, -1.0f));
    return 0;
}
