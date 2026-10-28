// 6.4.4.2p1-p4  decimal floating constants: forms, exponents, f/l suffixes, types
#include <stdio.h>
int main(void) {
    double a = 1., b = .5, c = 1e3, d = 2.5E-2, e = 1e+2;
    float f = 3.25f; long double l = 4.5L; float g = 1.5F;
    printf("%.3f %.3f %.3f %.4f %.1f %.2f %.1f %.1f\n", a, b, c, d, e, f, (double)l, g);
    printf("%d %d %d\n", (int)sizeof 1.0f, (int)sizeof 1.0, (int)sizeof 1.0L);
    printf("%.17g %.9g\n", 0.1, (double)0.1f);
    return 0;
}
