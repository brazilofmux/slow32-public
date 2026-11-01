/* 7.27.3.4 localtime: before the zone's first standard time (1883-11-18 noon) local time is local mean time, -5:50:36 */
#include <stdio.h>
#include <time.h>

int main(void) {
    static const long long when[] = {
        -2717647201LL,          /* the last second of local mean time */
        -2717647200LL,          /* the first of CST */
        -3000000000LL,          /* 1874 */
        -62135596800LL,         /* 0001-01-01 00:00 UTC */
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
