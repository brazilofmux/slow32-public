/* 7.12.7.2 fabs: absolute value, with a positive zero for a negative zero */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double nz = copysign(0.0, -1.0);
    printf("%g %g %g\n", fabs(v(-2.5)), fabs(v(2.5)), fabs(v(-1e-310)));
    printf("%d %d\n", copysign(1.0, fabs(v(nz))) > 0, copysign(1.0, (double)fabsf((float)v(nz))) > 0);
    printf("%g\n", (double)fabsf(-7.25f));
    return 0;
}
