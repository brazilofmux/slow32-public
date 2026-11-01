/* 7.27.3.4 localtime: local broken-down time in the zone (America/Chicago) across DST changes, before 1970, after 2038 */
#include <stdio.h>
#include <time.h>

int main(void) {
    static const long long when[] = {
        0,
        1741507199LL,           /* 2025-03-09 01:59:59 CST */
        1741507200LL,           /* 03:00:00 CDT */
        1762066799LL,           /* 2025-11-02 01:59:59 CDT */
        1762066800LL,           /* 01:00:00 CST, the second time */
        -1000000000LL,          /* 1938 */
        -2208988800LL,          /* 1900-01-01 00:00 UTC */
        2531347200LL,           /* 2050-03-20 */
        2541801600LL,           /* 2050-07-19: DST by the zone's rule */
        4118054400LL,           /* 2100-06-30 16:00 UTC */
    };
    int i;
    for (i = 0; i < (int)(sizeof when / sizeof when[0]); i++) {
        time_t t = (time_t)when[i];
        struct tm *g = localtime(&t);
        if (!g) {
            printf("%lld: null\n", when[i]);
            continue;
        }
        printf("%lld: %lld-%02d-%02d %02d:%02d:%02d wday=%d yday=%d isdst=%d\n", when[i],
               (long long)g->tm_year + 1900, g->tm_mon + 1, g->tm_mday, g->tm_hour, g->tm_min, g->tm_sec, g->tm_wday,
               g->tm_yday, g->tm_isdst > 0);
    }
    return 0;
}
