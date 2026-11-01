/* 7.3.5-7.3.9 the complex functions at ordinary arguments and on the branch cuts' signed zeros, printed to ten significant digits (and float ones to six), where two correct libraries agree */
#include <complex.h>
#include <math.h>
#include <stdio.h>

static void show(const char *what, double complex z) {
    printf("%-8s %.10g %.10g\n", what, creal(z), cimag(z));
}

static void showf(const char *what, float complex z) {
    printf("%-8s %.6g %.6g\n", what, crealf(z), cimagf(z));
}

int main(void) {
    double complex a = CMPLX(1.5, -2.25), b = CMPLX(-0.75, 3.0);
    float complex fa = CMPLXF(0.5f, 1.25f);
    show("cexp", cexp(a));
    show("clog", clog(a));
    show("csqrt", csqrt(a));
    show("cpow", cpow(a, b));
    show("csin", csin(a));
    show("ccos", ccos(a));
    show("ctan", ctan(a));
    show("casin", casin(a));
    show("cacos", cacos(a));
    show("catan", catan(a));
    show("csinh", csinh(a));
    show("ccosh", ccosh(a));
    show("ctanh", ctanh(a));
    show("casinh", casinh(a));
    show("cacosh", cacosh(a));
    show("catanh", catanh(a));
    printf("cabs %.10g carg %.10g\n", cabs(a), carg(a));
    show("conj", conj(a));
    show("cproj", cproj(CMPLX(INFINITY, -2.0)));
    show("sqrt-4", csqrt(CMPLX(-4.0, 0.0)));
    show("sqrt-4-0", csqrt(CMPLX(-4.0, -0.0)));
    show("log-1", clog(CMPLX(-1.0, 0.0)));
    show("log-1-0", clog(CMPLX(-1.0, -0.0)));
    showf("cexpf", cexpf(fa));
    showf("csqrtf", csqrtf(fa));
    showf("clogf", clogf(fa));
    printf("cabsf %.6g\n", cabsf(fa));
    return 0;
}
