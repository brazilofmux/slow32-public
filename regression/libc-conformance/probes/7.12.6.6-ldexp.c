/* 7.12.6.6 ldexp: x times 2 to the power exp, exact whenever the result is representable */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.17g\n", ldexp(v(1.0), -1074));
    printf("%.17g\n", ldexp(v(4.9406564584124654e-324), 1074));
    printf("%.17g\n", ldexp(v(0.75), 1024));
    printf("%.17g\n", ldexp(v(1e-310), 100));
    printf("%.17g\n", ldexp(v(1.0715086071862673e301), -2000));
    printf("%.17g\n", ldexp(v(-3.0), 5));
    printf("%.17g\n", ldexp(v(1.5), -1));
    printf("%.9g\n", (double)ldexpf(1.5f, 10));
    return 0;
}
