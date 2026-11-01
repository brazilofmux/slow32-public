/* 7.21.6.1 fprintf: infinity prints as inf or infinity and NaN as nan, upper case for F E G */
#include <stdio.h>
#include <math.h>

int main(void) {
    volatile double big = 1e308;
    double inf = big * 10.0, nan_ = fabs(inf - inf);
    char b[64];
    printf("[%f][%F][%e][%E][%g][%G]\n", inf, inf, -inf, -inf, inf, inf);
    printf("[%8f][%-8f|][%+f][% f][%.3f]\n", inf, inf, inf, inf, -inf);
    snprintf(b, sizeof b, "%f %F %e %G %g %E", nan_, nan_, nan_, nan_, nan_, nan_);
    printf("[%s]\n", b);
    snprintf(b, sizeof b, "%5f|%-5f|", nan_, nan_);
    printf("[%s]\n", b);
    return 0;
}
