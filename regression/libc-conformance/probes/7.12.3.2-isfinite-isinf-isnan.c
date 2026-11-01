/* 7.12.3.2-7.12.3.4 isfinite, isinf, isnan: true for exactly the finite, infinite and NaN arguments, double and float */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    double v[5];
    v[0] = 0.0; v[1] = -2.5; v[2] = 1e-310;
    v[3] = -strtod("inf", 0); v[4] = strtod("nan", 0);
    for (int i = 0; i < 5; i++) {
        float f = (float)v[i];
        printf("%d%d%d %d%d%d\n", isfinite(v[i]) != 0, isinf(v[i]) != 0, isnan(v[i]) != 0,
               isfinite(f) != 0, isinf(f) != 0, isnan(f) != 0);
    }
    return 0;
}
