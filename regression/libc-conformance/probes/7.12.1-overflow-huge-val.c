/* 7.12.1p5 range errors: a result that overflows comes back as HUGE_VAL with the sign of the true value */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%d %d\n", ldexp(v(1.0), 2000) == HUGE_VAL, ldexp(v(-1.0), 2000) == -HUGE_VAL);
    printf("%d %d\n", exp(v(1000.0)) == HUGE_VAL, pow(v(10.0), v(400.0)) == HUGE_VAL);
    printf("%d %d\n", cosh(v(1000.0)) == HUGE_VAL, sinh(v(-1000.0)) == -HUGE_VAL);
    return 0;
}
