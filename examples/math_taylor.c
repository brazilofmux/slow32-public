/*
 * Taylor-series math implementations under distinct names for comparison.
 * Extracted from math_soft.c #else branches.
 */

#define PI      3.14159265358979323846
#define TWO_PI  6.28318530717958647692
#define HALF_PI 1.57079632679489661923
#define LN2     0.69314718055994530942

extern double fabs(double);
extern double fmod(double, double);
extern double ldexp(double, int);

double taylor_sin(double x) {
    /* Range reduce to [-pi, pi] */
    x = fmod(x, TWO_PI);
    if (x > PI) x -= TWO_PI;
    if (x < -PI) x += TWO_PI;

    /* Taylor: sin(x) = x - x^3/3! + x^5/5! - ... */
    double x2 = x * x;
    double term = x, sum = x;
    for (int i = 1; i <= 15; i++) {
        term *= -x2 / (double)(2 * i * (2 * i + 1));
        sum += term;
    }
    return sum;
}

double taylor_cos(double x) {
    return taylor_sin(x + HALF_PI);
}

static double taylor_atan_core(double x) {
    int neg = 0, recip = 0;
    if (x < 0.0) { neg = 1; x = -x; }
    if (x > 1.0) { recip = 1; x = 1.0 / x; }

    double result;
    if (x > 0.2679) {
        double sqrt3 = 1.7320508075688772;
        double t = (x * sqrt3 - 1.0) / (sqrt3 + x);
        double t2 = t * t, sum = t, term = t;
        for (int i = 1; i <= 20; i++) {
            term *= -t2;
            sum += term / (double)(2 * i + 1);
        }
        result = 0.5235987755982988 + sum;  /* pi/6 */
    } else {
        double x2 = x * x, sum = x, term = x;
        for (int i = 1; i <= 20; i++) {
            term *= -x2;
            sum += term / (double)(2 * i + 1);
        }
        result = sum;
    }

    if (recip) result = HALF_PI - result;
    if (neg) result = -result;
    return result;
}

double taylor_atan2(double y, double x) {
    if (x > 0.0) return taylor_atan_core(y / x);
    if (x < 0.0)
        return (y >= 0.0) ? taylor_atan_core(y / x) + PI
                          : taylor_atan_core(y / x) - PI;
    if (y > 0.0) return HALF_PI;
    if (y < 0.0) return -HALF_PI;
    return 0.0;
}

double taylor_exp(double x) {
    if (x > 709.0) return 1e308;
    if (x < -709.0) return 0.0;

    int k = (int)(x / LN2 + (x >= 0.0 ? 0.5 : -0.5));
    double r = x - (double)k * LN2;

    /* Taylor series for exp(r), |r| < 0.35 */
    double term = 1.0, sum = 1.0;
    for (int i = 1; i <= 25; i++) {
        term *= r / (double)i;
        sum += term;
        if (fabs(term) < 1e-16 * fabs(sum)) break;
    }
    return ldexp(sum, k);
}
