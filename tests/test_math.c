/* Math library validation test for SLOW-32 runtime */
#include <stdio.h>
#include <math.h>

static int pass = 0, fail = 0;

static void check(const char *name, double got, double expected, double tol) {
    double err = (got - expected);
    if (err < 0) err = -err;
    if (expected != 0.0) {
        double rel = err / (expected < 0 ? -expected : expected);
        if (rel < tol) { pass++; return; }
    } else {
        if (err < tol) { pass++; return; }
    }
    fail++;
    printf("FAIL: %s = %.10f, expected %.10f (err=%.2e)\n", name, got, expected, err);
}

static void check_int(const char *name, int got, int expected) {
    if (got == expected) { pass++; return; }
    fail++;
    printf("FAIL: %s = %d, expected %d\n", name, got, expected);
}

int main(void) {
    double tol = 1e-8;  /* generous tolerance for soft-float */

    printf("=== SLOW-32 Math Library Tests ===\n\n");

    /* --- fabs --- */
    check("fabs(3.14)", fabs(3.14), 3.14, tol);
    check("fabs(-2.7)", fabs(-2.7), 2.7, tol);
    check("fabs(0.0)", fabs(0.0), 0.0, tol);

    /* --- floor/ceil/trunc/round --- */
    check("floor(2.7)", floor(2.7), 2.0, tol);
    check("floor(-2.3)", floor(-2.3), -3.0, tol);
    check("floor(5.0)", floor(5.0), 5.0, tol);
    check("ceil(2.3)", ceil(2.3), 3.0, tol);
    check("ceil(-2.7)", ceil(-2.7), -2.0, tol);
    check("trunc(2.9)", trunc(2.9), 2.0, tol);
    check("trunc(-2.9)", trunc(-2.9), -2.0, tol);
    check("round(2.5)", round(2.5), 3.0, tol);
    check("round(-2.5)", round(-2.5), -3.0, tol);
    check("round(2.3)", round(2.3), 2.0, tol);

    /* --- sqrt --- */
    check("sqrt(4.0)", sqrt(4.0), 2.0, tol);
    check("sqrt(2.0)", sqrt(2.0), 1.41421356237, tol);
    check("sqrt(100.0)", sqrt(100.0), 10.0, tol);
    check("sqrt(0.25)", sqrt(0.25), 0.5, tol);

    /* --- fmod --- */
    check("fmod(10.0, 3.0)", fmod(10.0, 3.0), 1.0, tol);
    check("fmod(-10.0, 3.0)", fmod(-10.0, 3.0), -1.0, tol);
    check("fmod(10.5, 3.0)", fmod(10.5, 3.0), 1.5, tol);

    /* --- exp/log --- */
    check("exp(0.0)", exp(0.0), 1.0, tol);
    check("exp(1.0)", exp(1.0), 2.71828182846, tol);
    check("exp(-1.0)", exp(-1.0), 0.36787944117, tol);
    check("exp(2.0)", exp(2.0), 7.38905609893, tol);
    check("log(1.0)", log(1.0), 0.0, tol);
    check("log(M_E)", log(2.71828182846), 1.0, tol);
    check("log(10.0)", log(10.0), 2.30258509299, tol);
    check("log10(100.0)", log10(100.0), 2.0, tol);
    check("log10(1000.0)", log10(1000.0), 3.0, tol);

    /* --- pow --- */
    check("pow(2.0, 10.0)", pow(2.0, 10.0), 1024.0, tol);
    check("pow(3.0, 3.0)", pow(3.0, 3.0), 27.0, tol);
    check("pow(2.0, -1.0)", pow(2.0, -1.0), 0.5, tol);
    check("pow(4.0, 0.5)", pow(4.0, 0.5), 2.0, tol);
    check("pow(27.0, 1.0/3.0)", pow(27.0, 1.0/3.0), 3.0, tol);

    /* --- sin/cos/tan --- */
    check("sin(0.0)", sin(0.0), 0.0, tol);
    check("sin(pi/2)", sin(M_PI_2), 1.0, tol);
    check("sin(pi)", sin(M_PI), 0.0, tol);
    check("sin(pi/6)", sin(M_PI/6.0), 0.5, tol);
    check("cos(0.0)", cos(0.0), 1.0, tol);
    check("cos(pi)", cos(M_PI), -1.0, tol);
    check("cos(pi/3)", cos(M_PI/3.0), 0.5, tol);
    check("tan(0.0)", tan(0.0), 0.0, tol);
    check("tan(pi/4)", tan(M_PI/4.0), 1.0, tol);

    /* --- sin/cos at larger angles --- */
    check("sin(10.0)", sin(10.0), -0.54402111088, tol);
    check("cos(10.0)", cos(10.0), -0.83907152907, tol);
    check("sin(-3.0)", sin(-3.0), -0.14112000806, tol);

    /* --- atan/atan2 --- */
    check("atan(0.0)", atan(0.0), 0.0, tol);
    check("atan(1.0)", atan(1.0), M_PI_4, tol);
    check("atan(-1.0)", atan(-1.0), -M_PI_4, tol);
    check("atan2(1.0,1.0)", atan2(1.0, 1.0), M_PI_4, tol);
    check("atan2(1.0,-1.0)", atan2(1.0, -1.0), 3.0*M_PI_4, tol);
    check("atan2(0.0, -1.0)", atan2(0.0, -1.0), M_PI, tol);

    /* --- asin/acos --- */
    check("asin(0.0)", asin(0.0), 0.0, tol);
    check("asin(0.5)", asin(0.5), M_PI/6.0, tol);
    check("asin(1.0)", asin(1.0), M_PI_2, tol);
    check("acos(1.0)", acos(1.0), 0.0, tol);
    check("acos(0.0)", acos(0.0), M_PI_2, tol);
    check("acos(0.5)", acos(0.5), M_PI/3.0, tol);

    /* --- sinh/cosh/tanh --- */
    check("sinh(0.0)", sinh(0.0), 0.0, tol);
    check("sinh(1.0)", sinh(1.0), 1.17520119364, tol);
    check("cosh(0.0)", cosh(0.0), 1.0, tol);
    check("cosh(1.0)", cosh(1.0), 1.54308063482, tol);
    check("tanh(0.0)", tanh(0.0), 0.0, tol);
    check("tanh(1.0)", tanh(1.0), 0.76159415596, tol);
    check("tanh(100.0)", tanh(100.0), 1.0, tol);

    /* --- ldexp/frexp --- */
    check("ldexp(1.0, 10)", ldexp(1.0, 10), 1024.0, tol);
    check("ldexp(1.5, 3)", ldexp(1.5, 3), 12.0, tol);
    check("ldexp(1.0, -1)", ldexp(1.0, -1), 0.5, tol);
    {
        int e;
        double m = frexp(8.0, &e);
        check("frexp(8.0) mantissa", m, 0.5, tol);
        check_int("frexp(8.0) exponent", e, 4);
        m = frexp(0.75, &e);
        check("frexp(0.75) mantissa", m, 0.75, tol);
        check_int("frexp(0.75) exponent", e, 0);
    }

    /* --- copysign --- */
    check("copysign(3.0, -1.0)", copysign(3.0, -1.0), -3.0, tol);
    check("copysign(-3.0, 1.0)", copysign(-3.0, 1.0), 3.0, tol);

    /* --- modf --- */
    {
        double ipart;
        double fpart = modf(3.75, &ipart);
        check("modf(3.75) int", ipart, 3.0, tol);
        check("modf(3.75) frac", fpart, 0.75, tol);
    }

    /* --- classification --- */
    check_int("isfinite(1.0)", isfinite(1.0), 1);
    check_int("isnan(1.0)", isnan(1.0), 0);

    /* --- float versions (spot check) --- */
    {
        float sf = sinf(1.0f);
        check("sinf(1.0)", (double)sf, 0.84147098481, 1e-5);
        float cf = cosf(0.0f);
        check("cosf(0.0)", (double)cf, 1.0, 1e-5);
        float sq = sqrtf(9.0f);
        check("sqrtf(9.0)", (double)sq, 3.0, 1e-5);
    }

    printf("\n%d passed, %d failed\n", pass, fail);
    if (fail == 0) printf("ALL TESTS PASSED\n");
    return fail;
}
