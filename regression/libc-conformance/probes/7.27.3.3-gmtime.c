/* 7.27.3.3 gmtime: UTC broken-down time for times before 1970, after 2038, at leap days and century years */
#include <stdio.h>
#include <time.h>

int main(void) {
    static const long long when[] = {
        0, -1, 86399, 86400, -86400, -86401,
        951782400LL,            /* 2000-02-29 */
        951868800LL,            /* 2000-03-01 */
        -2203891200LL,          /* 1900-03-01: 1900 not a leap year */
        -2208988800LL,          /* 1900-01-01 */
        -12219292800LL,         /* 1582-10-15, proleptic Gregorian */
        -62135596800LL,         /* 0001-01-01 */
        -62135596801LL,         /* the second before: year 0 */
        2147483647LL, 2147483648LL, 4294967296LL,
        4107542400LL,           /* 2100-03-01 */
        253402300799LL,         /* 9999-12-31 23:59:59 */
        253402300800LL,         /* 10000-01-01 */
    };
    int i;
    for (i = 0; i < (int)(sizeof when / sizeof when[0]); i++) {
        time_t t = (time_t)when[i];
        struct tm *g = gmtime(&t);
        if (!g) {
            printf("%lld: null\n", when[i]);
            continue;
        }
        printf("%lld: %lld-%02d-%02d %02d:%02d:%02d wday=%d yday=%d isdst=%d\n", when[i],
               (long long)g->tm_year + 1900, g->tm_mon + 1, g->tm_mday, g->tm_hour, g->tm_min, g->tm_sec, g->tm_wday,
               g->tm_yday, g->tm_isdst);
    }
    return 0;
}
