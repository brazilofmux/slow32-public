/* 7.3 and 6.5 complex arithmetic: + - * / and unary - on double and float complex, real and complex operands mixed, I, CMPLX, creal/cimag, and the infinite and NaN operands of G.5.1 that the plain formulas get wrong; printed exactly */
#include <complex.h>
#include <math.h>
#include <stdio.h>

static void show(const char *what, double complex z) {
    printf("%-10s %a %a\n", what, creal(z), cimag(z));
}

int main(void) {
    volatile double inf = INFINITY, qnan = NAN, zero = 0.0;
    double complex a = CMPLX(1.5, -2.25), b = CMPLX(-0.75, 3.0);
    float complex fa = CMPLXF(1.5f, -2.25f), fb = CMPLXF(-0.75f, 3.0f);
    show("a*b", a * b);
    show("a/b", a / b);
    show("a+b", a + b);
    show("a-b", a - b);
    show("-a", -a);
    show("fa*fb", fa * fb);
    show("fa/fb", fa / fb);
    show("I*I", I * I);
    show("2a+1", 2.0 * a + 1.0);
    show("3/a", 3.0 / a);
    show("a/2", a / 2.0);
    show("inf*1", CMPLX(inf, 0) * CMPLX(1, 0));
    show("inf*i", CMPLX(inf, inf) * CMPLX(0, 1));
    show("nan-inf*2", CMPLX(qnan, inf) * CMPLX(2, 0));
    show("big*big", CMPLX(1e300, 1e300) * CMPLX(1e300, -1e300));
    show("z/0", CMPLX(1, 1) / CMPLX(zero, zero));
    show("inf/z", CMPLX(inf, 1) / CMPLX(2, 3));
    show("z/inf", CMPLX(1, 2) / CMPLX(inf, inf));
    show("big/big", CMPLX(1e300, 1e300) / CMPLX(1e300, 1e-300));
    show("tiny/tiny", CMPLX(1e-300, 2e-300) / CMPLX(3e-300, 4e-300));
    printf("a==a %d a==b %d a!=b %d real==cplx %d\n", a == a, a == b, a != b, 2.0 == CMPLX(2, 0));
    return 0;
}
