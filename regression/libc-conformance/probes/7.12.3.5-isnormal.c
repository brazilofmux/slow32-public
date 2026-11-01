/* 7.12.3.5 isnormal: true only for normal values, judged in the argument's own type */
#include <math.h>
#include <stdio.h>

int main(void) {
    volatile double a = 1.0, b = 0.0, c = 1e-310, d = 1e-40;
    volatile float e = 1e-40f, f = 2.0f;
    printf("%d %d %d %d %d %d\n", isnormal(a) != 0, isnormal(b) != 0, isnormal(c) != 0,
           isnormal(d) != 0, isnormal(e) != 0, isnormal(f) != 0);
    return 0;
}
