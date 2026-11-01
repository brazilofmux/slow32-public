/* 7.12.7.3 hypot: the square root of the sum of squares, without undue overflow */
#include <math.h>
#include <stdio.h>

int main(void) {
    printf("%.6g %.6g %d\n", hypot(3.0, 4.0), hypot(1e200, 1e200), isinf(hypot(1e308, 1e308)));
    return 0;
}
