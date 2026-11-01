/* 7.12.11.1 copysign: magnitude of x with the sign of y, zeros included */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double nz = copysign(v(0.0), v(-1.0));
    printf("%g %g %g %g\n", copysign(v(3.0), v(-1.0)), copysign(v(-3.0), v(2.0)),
           copysign(v(3.0), nz), copysign(v(-3.0), v(0.0)));
    printf("%d %g\n", 1.0 / copysign(1e300, nz) < 0, (double)copysignf(2.5f, -0.0f));
    return 0;
}
