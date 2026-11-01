/* 7.12.11.2-7.12.11.4 nan, nextafter, nexttoward: quiet NaN from a string and the next representable value */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double n = nan("");
    printf("%d\n", n != n);
    printf("%.17g %.17g\n", nextafter(v(1.0), v(2.0)), nextafter(v(0.0), v(-1.0)));
    printf("%.17g\n", nexttoward(v(1.0), 0.0L));
    printf("%.9g\n", (double)nextafterf(1.0f, 2.0f));
    return 0;
}
