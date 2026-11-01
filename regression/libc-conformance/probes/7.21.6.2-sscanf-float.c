/* 7.21.6.2 fscanf: %f %e %g %a and %F match strtod subject sequences, into float, double (l) and long double (L) */
#include <stdio.h>

int main(void) {
    float f = 0, g = 0;
    double d = 0, e = 0, h = 0;
    long double ld = 0;
    int r = sscanf("1.5 -2.5e3 .25 7.", "%f %lf %le %lg", &f, &d, &e, &h);
    printf("%d: %g %g %g %g\n", r, f, d, e, h);
    r = sscanf("0x1.8p1", "%la", &d);
    printf("%d: %g\n", r, d);
    r = sscanf("1e-2", "%Lf", &ld);
    printf("%d: %Lg\n", r, ld);
    r = sscanf("+3E+2", "%G", &g);
    printf("%d: %g\n", r, g);
    r = sscanf("-0x10", "%lf", &d);
    printf("%d: %g\n", r, d);
    r = sscanf("4.5", "%E", &g);
    printf("%d: %g\n", r, g);
    r = sscanf("inf -INFINITY nan", "%lf %lf %lf", &d, &e, &h);
    printf("%d: %g %g %d\n", r, d, e, h != h);
    r = sscanf("1e400 1e-400", "%lf %lf", &d, &e);
    printf("%d: %g %d\n", r, d, e >= 0 && e < 1e-300);
    r = sscanf("0.1x", "%lf", &d);
    printf("%d: %.17g\n", r, d);
    r = sscanf("123456789012345678901234567890", "%lf", &d);
    printf("%d: %.17g\n", r, d);
    r = sscanf("2.5", "%F", &f);
    printf("%d: %g\n", r, f);
    return 0;
}
