/* 7.8.2.1, 7.8.2.2: imaxabs and imaxdiv (with imaxdiv_t) on intmax_t */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    imaxdiv_t q = imaxdiv(-7000000000000, 3);
    printf("%lld %lld\n", (long long)imaxabs(-9000000000000), (long long)imaxabs(INTMAX_MAX) - INTMAX_MAX);
    printf("%lld %lld\n", (long long)q.quot, (long long)q.rem);
    return 0;
}
