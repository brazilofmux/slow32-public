/* 7.12.6 exp, log, log10, log2 and float forms: values at ordinary arguments, to 12 significant digits */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g %.12g\n", exp(v(1.0)), exp(v(-1.0)), exp(v(0.0)), exp(v(100.0)));
    printf("%.12g %.12g %.12g %.12g\n", log(v(10.0)), log(v(1.0)), log(v(1e-300)), log(v(1e300)));
    printf("%.12g %.12g %.12g\n", log10(v(1000.0)), log10(v(2.0)), log10(v(1e-300)));
    printf("%.12g %.12g %.12g\n", log2(v(8.0)), log2(v(3.0)), log2(v(1e-310)));
    printf("%.5g %.5g %.5g %.5g\n", expf((float)v(2.0)), logf((float)v(2.0)),
           log10f((float)v(50.0)), log2f((float)v(10.0)));
    return 0;
}
