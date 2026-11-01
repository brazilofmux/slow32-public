/* 7.12.3.6 signbit: nonzero exactly when the sign is negative, zeros and infinities included */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    volatile double pz = 0.0, nz = -0.0, n = -3.0, p = 3.0;
    volatile float fz = -0.0f, fs = -1e-40f;
    double ninf = -strtod("inf", 0);
    printf("%d %d %d %d %d %d %d\n", signbit(pz) != 0, signbit(nz) != 0, signbit(n) != 0,
           signbit(p) != 0, signbit(fz) != 0, signbit(fs) != 0, signbit(ninf) != 0);
    return 0;
}
