/* 7.12.10.1 fmod: x - n*y exactly, with x's sign and magnitude below |y| */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.17g %.17g %.17g %.17g\n", fmod(v(5.5), v(2.0)), fmod(v(-5.5), v(2.0)),
           fmod(v(5.5), v(-2.0)), fmod(v(-5.5), v(-2.0)));
    printf("%.17g %.17g\n", fmod(v(1e17), v(0.3)), fmod(v(1e300), v(3.0)));
    printf("%.17g %.17g\n", fmod(v(0.3), v(0.1)), fmod(v(1.0), v(1e-310)));
    printf("%.17g %.17g\n", fmod(v(1.5), v(4.0)), fmod(v(-6.0), v(3.0)));
    printf("%d\n", copysign(1.0, fmod(v(-6.0), v(3.0))) < 0);
    printf("%.9g %.9g\n", (double)fmodf(7.5f, 2.0f), (double)fmodf(-1e10f, 3.0f));
    return 0;
}
