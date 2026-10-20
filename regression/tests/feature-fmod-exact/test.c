/* fmod and fmodf are exact (C17 7.12.10.1: the result has x's sign and is
 * less than y in magnitude, and IEEE makes it exact), so every result is
 * one particular double, and the host's libm prints the same bits.  Pairs
 * across the whole exponent range, by a fixed generator, then the edge
 * cases: subnormals, x a multiple of y, |x| < |y|, signed zeros,
 * infinities, NaN.  Checked against glibc and macOS. */
#include <math.h>
#include <stdio.h>
#include <string.h>

static unsigned long long state = 0x2545F4914F6CDD1Dull;
static unsigned long long next(void) {
    state ^= state << 13;
    state ^= state >> 7;
    state ^= state << 17;
    return state;
}

static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, 8); return u; }
static unsigned fbits(float f) { unsigned u; memcpy(&u, &f, 4); return u; }

static void show(double r) {
    if (r != r) printf(" nan");
    else printf(" %016llx", dbits(r));
}

int main(void) {
    unsigned long long sum = 0, fsum = 0;
    for (int i = 0; i < 4000; i++) {
        unsigned long long a = next(), b = next();
        /* finite doubles of any exponent, either sign */
        double x, y;
        a &= 0xFFEFFFFFFFFFFFFFull;
        b &= 0xFFEFFFFFFFFFFFFFull;
        memcpy(&x, &a, 8);
        memcpy(&y, &b, 8);
        if (y == 0.0) continue;
        double r = fmod(x, y);
        sum = sum * 1000003u + dbits(r);
        float fx = (float)ldexp((double)(a >> 40), (int)(a % 200) - 100);
        float fy = (float)ldexp((double)(b >> 40) + 1.0, (int)(b % 200) - 120);
        if (a & 1) fx = -fx;
        fsum = fsum * 1000003u + fbits(fmodf(fx, fy));
        if (i < 6) {
            printf("pair %d:", i);
            show(r);
            printf(" %08x\n", fbits(fmodf(fx, fy)));
        }
    }
    printf("4000 double pairs: %016llx\n", sum);
    printf("4000 float pairs:  %016llx\n", fsum);

    static const double xs[] = { 10.0, -10.0, 9.0, 0.0, -0.0, 1e-310, 3e-308, 5.0, 1.0, 7.0, INFINITY, 2.0, 1e308 };
    static const double ys[] = { 3.0, 3.0, 3.0, 2.0, 2.0, 3e-311, 1e-310, INFINITY, 1e-310, -2.0, 1.0, NAN, 1e-308 };
    for (int i = 0; i < 13; i++) {
        printf("fmod(%g, %g) =", xs[i], ys[i]);
        show(fmod(xs[i], ys[i]));
        printf("\n");
    }
    printf("fmod(1, 0) nan %d, fmod(nan, 1) nan %d\n", fmod(1.0, 0.0) != fmod(1.0, 0.0), fmod(NAN, 1.0) != fmod(NAN, 1.0));
    printf("fmodf(7.5, 2) = %08x, fmodf(-0.0, 1) = %08x\n", fbits(fmodf(7.5f, 2.0f)), fbits(fmodf(-0.0f, 1.0f)));
    return 0;
}
