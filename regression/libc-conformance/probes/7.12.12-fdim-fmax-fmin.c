/* 7.12.12 fdim, fmax, fmin: positive difference, maximum, minimum, a NaN argument ignored beside a number */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double n = strtod("nan", 0);
    printf("%g %g %g\n", fdim(v(5.0), v(3.0)), fdim(v(3.0), v(5.0)), fdim(v(-1.0), v(-4.5)));
    printf("%g %g %g %g\n", fmax(v(2.0), v(-3.0)), fmax(n, v(1.5)), fmax(v(1.5), n), fmax(v(-1e300), v(-1e301)));
    printf("%g %g %g %d\n", fmin(v(2.0), v(-3.0)), fmin(n, v(1.5)), fmin(v(1.5), n), fmin(n, n) != fmin(n, n));
    printf("%g %g %g\n", (double)fdimf(1.0f, 4.0f), (double)fmaxf(1.0f, 4.0f), (double)fminf(1.0f, 4.0f));
    return 0;
}
