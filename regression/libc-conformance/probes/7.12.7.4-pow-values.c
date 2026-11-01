/* 7.12.7.4 pow: values for integral, fractional and negative exponents, and negative bases with integral exponents */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.17g %.17g %.17g\n", pow(v(2.0), v(10.0)), pow(v(-2.0), v(3.0)), pow(v(4.0), v(-0.5)));
    printf("%.12g %.12g %.12g\n", pow(v(2.0), v(0.5)), pow(v(10.0), v(-2.5)), pow(v(10.0), v(300.5)));
    printf("%.12g %.12g\n", pow(v(1.1), v(1000.0)), pow(v(-1.5), v(-3.0)));
    printf("%g %g %g\n", pow(v(7.0), v(0.0)), pow(v(0.0), v(2.0)), pow(v(1.0), v(12345.6)));
    printf("%.5g\n", (double)powf(2.0f, 0.5f));
    return 0;
}
