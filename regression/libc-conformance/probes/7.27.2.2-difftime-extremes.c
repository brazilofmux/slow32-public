/* 7.27.2.2 difftime: the difference of two far-apart time_t values, which need not fit in time_t itself */
#include <stdio.h>
#include <time.h>

int main(void) {
    /* every library measured here has a signed 64-bit time_t; the
     * probe says so rather than assume it */
    time_t hi, lo;
    if (sizeof(time_t) != 8 || (time_t)-1 > 0) {
        printf("time_t is not signed 64-bit: nothing to test\n");
        return 0;
    }
    hi = (time_t)((((unsigned long long)1 << 62) - 1) * 2 + 1);   /* 2^63 - 1 */
    lo = -hi - 1;
    printf("%.0f\n", difftime(hi, lo));
    printf("%.0f\n", difftime(lo, hi));
    printf("%.0f\n", difftime(hi, (time_t)-1));
    printf("%.0f\n", difftime((time_t)-2, hi));
    printf("%.0f\n", difftime(hi, (time_t)0));
    printf("%.0f\n", difftime((time_t)9007199254740993LL, (time_t)1));
    return 0;
}
