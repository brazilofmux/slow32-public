/* <math.h> declares what the library defines: a function called
 * through a missing declaration returns int, and its double comes back
 * as garbage.  The two libraries build these from one source; this is a
 * test of the header, and of the two compilers' arithmetic. */
#include <stdio.h>
#include <math.h>

int main(void) {
    double x = 0.5, y = 2.0;
    float fx = 0.5f;
    int e;
    double ip;
    printf("acos %.6f asin %.6f atan %.6f atan2 %.6f\n", acos(x), asin(x), atan(x), atan2(x, y));
    printf("sinh %.6f cosh %.6f tanh %.6f\n", sinh(x), cosh(x), tanh(x));
    printf("log10 %.6f log2 %.6f cbrt %.6f\n", log10(1000.0), log2(8.0), cbrt(27.0));
    printf("round %.1f %.1f %.1f trunc %.1f %.1f\n", round(2.5), round(-2.5), round(2.4), trunc(2.9), trunc(-2.9));
    printf("copysign %.1f %.1f\n", copysign(3.0, -1.0), copysign(-3.0, 1.0));
    printf("fabs %.2f fabsf %.2f\n", fabs(-1.25), (double)fabsf(-2.5f));
    printf("floor %.1f ceil %.1f fmod %.2f\n", floor(-1.5), ceil(-1.5), fmod(7.5, 2.0));
    printf("sqrt %.6f pow %.3f exp %.6f log %.6f\n", sqrt(2.0), pow(2.0, 10.0), exp(1.0), log(10.0));
    printf("sin %.6f cos %.6f tan %.6f\n", sin(1.0), cos(1.0), tan(1.0));
    printf("frexp %.4f %d", frexp(48.0, &e), e);
    printf(" ldexp %.1f modf %.2f", ldexp(0.75, 6), modf(3.25, &ip));
    printf(" %.1f\n", ip);
    printf("float: sinf %.4f cosf %.4f sqrtf %.4f powf %.2f floorf %.1f\n", (double)sinf(fx), (double)cosf(fx),
           (double)sqrtf(2.0f), (double)powf(2.0f, 0.5f), (double)floorf(-0.5f));
    printf("float: acosf %.4f atan2f %.4f tanhf %.4f roundf %.1f truncf %.1f\n", (double)acosf(fx),
           (double)atan2f(fx, 2.0f), (double)tanhf(fx), (double)roundf(2.5f), (double)truncf(-2.7f));
    printf("isnan %d isinf %d isfinite %d\n", isnan(x) != 0, isinf(x) != 0, isfinite(x) != 0);
    return 0;
}
