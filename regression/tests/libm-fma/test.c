/* fma and fmaf round once (GitHub issue 99).  llvm.fma used to lower to
 * a multiply and an add, two roundings; it is now a call into the
 * runtime.  Each line is the bit pattern of one result.  Spelled as
 * x*y+z, eight of the fourteen lines come out differently. */
#include <stdio.h>
#include <math.h>
#include <string.h>

static void pd(const char *name, double x, double y, double z) {
    volatile double vx = x, vy = y, vz = z;
    double r = fma(vx, vy, vz);
    unsigned long long b;
    memcpy(&b, &r, 8);
    printf("%-9s %08lx%08lx\n", name, (unsigned long)(b >> 32), (unsigned long)(b & 0xffffffffu));
}

static void pf(const char *name, float x, float y, float z) {
    volatile float vx = x, vy = y, vz = z;
    float r = fmaf(vx, vy, vz);
    unsigned int b;
    memcpy(&b, &r, 4);
    printf("%-9s %08lx\n", name, (unsigned long)b);
}

int main(void) {
    /* the product (1+2^-23)(1+2^-30) = 1+2^-23+2^-30+2^-53 is a tie that
     * rounds down to even; the addend breaks the tie upward */
    pd("tiebreak", 1.0 + 0x1p-23, 1.0 + 0x1p-30, 0x1p-60);
    /* same, with an addend far below: only its sticky bit survives */
    pd("sticky", 1.0 + 0x1p-23, 1.0 + 0x1p-30, 0x1p-100);
    /* the rounding error of the product: 2^-53, not 0 */
    pd("residual", 1.0 + 0x1p-23, 1.0 + 0x1p-30, -(1.0 + 0x1p-23 + 0x1p-30));
    /* a product that underflows to -0 stays -0 when the addend is +0 */
    pd("negzero", 0x1p-1074, -0.5, 0.0);
    /* an infinite addend beside an overflowing product: the addend */
    pd("overinf", 2.0, 0x1.fffffffffffffp1023, -INFINITY);
    /* exact cancellation is +0 */
    pd("cancel", 1.0, -1.0, 1.0);
    /* two subnormal halves make the next subnormal, exactly */
    pd("subnorm", 0x1p-537, 0x1p-537, 0x1p-1074);
    /* the product's exponent overflows but the sum does not */
    pd("bigsmall", 0x1p1000, 0x1p1000, -0x1p1023);
    pd("plain", 3.0, 4.0, 5.0);
    pd("ulp", 1.0 + 0x1p-52, 1.0 + 0x1p-52, -1.0);

    pf("ftie", 1.0f + 0x1p-12f, 1.0f + 0x1p-12f, 0x1p-30f);
    pf("fresid", 1.0f + 0x1p-12f, 1.0f + 0x1p-12f, -(1.0f + 0x1p-11f));
    pf("fnegzero", 0x1p-149f, -0.5f, 0.0f);
    pf("fplain", 3.0f, 4.0f, 5.0f);
    return 0;
}
