/* 6.2.5p11-13, 6.3.1.6-6.3.1.8, 7.3.1: complex types' size and layout (an array of two), conversions between real, integer, complex and _Bool, the usual arithmetic conversions with a complex operand, complex in a structure and through a function, and the header's macros */
#include <complex.h>
#include <stdio.h>

struct holder { char tag; double complex z; float complex f; };

static double complex twice(double complex z) { return z + z; }
static float complex fconj(float complex z) { return conj(z); }

int main(void) {
    double complex z = CMPLX(2.5, -1.0);
    float complex f = CMPLXF(0.5f, 4.0f);
    double parts[2];
    struct holder h;
    double complex fromint = 7;
    double complex widened = f;
    float complex narrowed = z;
    int back = (int)CMPLX(9.75, 3.0);
    _Bool truth1 = CMPLX(0.0, 1.0), truth0 = CMPLX(0.0, 0.0);
    printf("defined: %d %d\n",
#ifdef __STDC_NO_COMPLEX__
           0,
#else
           1,
#endif
#ifdef complex
           1
#else
           0
#endif
    );
    printf("sizes %d %d\n", (int)sizeof(double complex), (int)sizeof(float complex));
    *(double complex *)parts = z;
    printf("layout %g %g\n", parts[0], parts[1]);
    printf("int->c %g %g\n", creal(fromint), cimag(fromint));
    printf("f->d %g %g d->f %g %g\n", creal(widened), cimag(widened), crealf(narrowed), cimagf(narrowed));
    printf("c->int %d bool %d %d\n", back, truth1, truth0);
    printf("usual %d %d\n", (int)sizeof(f + 1.0), (int)sizeof(f * 2));
    h.tag = 'h';
    h.z = twice(z);
    h.f = fconj(f);
    printf("struct %c %g %g %g %g\n", h.tag, creal(h.z), cimag(h.z), crealf(h.f), cimagf(h.f));
    z += 1.0;
    z *= I;
    z -= CMPLX(1, 1);
    z /= 2;
    printf("compound %g %g\n", creal(z), cimag(z));
    printf("I %g %g\n", creal(I), cimag(I));
    return 0;
}
