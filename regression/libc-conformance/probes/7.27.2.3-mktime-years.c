/* 7.27.2.3 mktime: years far from 1970 -- before 1901, after 2038, beyond 9999 -- convert and round-trip through localtime */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void one(long long y, int mo, int d, int h) {
    struct tm t, *b;
    time_t r;
    memset(&t, 0, sizeof t);
    t.tm_year = (int)(y - 1900); t.tm_mon = mo - 1; t.tm_mday = d; t.tm_hour = h; t.tm_isdst = -1;
    r = mktime(&t);
    printf("%lld-%02d-%02d %02d -> %lld wday=%d yday=%d isdst=%d", y, mo, d, h, (long long)r, t.tm_wday, t.tm_yday,
           t.tm_isdst);
    b = localtime(&r);
    if (r != (time_t)-1 && b)
        printf(" back=%lld-%02d-%02d %02d:%02d:%02d", (long long)b->tm_year + 1900, b->tm_mon + 1, b->tm_mday,
               b->tm_hour, b->tm_min, b->tm_sec);
    printf("\n");
}

int main(void) {
    one(1, 1, 1, 0);
    one(1600, 3, 1, 12);
    one(1850, 6, 1, 12);       /* local mean time in this zone */
    one(1900, 3, 1, 12);
    one(1901, 12, 14, 12);
    one(1969, 7, 20, 20);
    one(2038, 1, 18, 21);      /* 2038-01-19 03:00 UTC */
    one(2038, 1, 19, 12);
    one(2100, 2, 29, 12);      /* not a leap year: March 1 */
    one(2400, 2, 29, 12);      /* a leap year */
    one(9999, 12, 31, 23);
    one(10000, 7, 4, 12);
    one(292277026, 1, 1, 0);
    return 0;
}
