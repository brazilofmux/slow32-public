/* 7.27.2.3 mktime: each member out of range on its own (and wday/yday ignored) comes back in range */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void one(const char *what, int y, int mo, int d, int h, int mi, int s) {
    struct tm t;
    time_t r;
    memset(&t, 0, sizeof t);
    t.tm_year = y; t.tm_mon = mo; t.tm_mday = d; t.tm_hour = h; t.tm_min = mi; t.tm_sec = s;
    t.tm_isdst = -1;
    t.tm_wday = 123; t.tm_yday = -400;           /* to be ignored */
    r = mktime(&t);
    printf("%-14s %lld  %04d-%02d-%02d %02d:%02d:%02d wday=%d yday=%d isdst=%d\n", what, (long long)r,
           t.tm_year + 1900, t.tm_mon + 1, t.tm_mday, t.tm_hour, t.tm_min, t.tm_sec, t.tm_wday, t.tm_yday,
           t.tm_isdst);
}

int main(void) {
    one("plain", 125, 0, 15, 12, 0, 0);
    one("sec 60", 125, 0, 15, 12, 0, 60);
    one("sec 7200", 125, 0, 15, 12, 0, 7200);
    one("sec -1", 125, 0, 1, 0, 0, -1);
    one("min 1500", 125, 0, 15, 12, 1500, 0);
    one("min -721", 125, 0, 15, 12, -721, 0);
    one("hour 49", 125, 0, 15, 49, 0, 0);
    one("hour -13", 125, 0, 15, -13, 0, 0);
    one("mday 0", 125, 2, 0, 12, 0, 0);
    one("mday 0 leap", 124, 2, 0, 12, 0, 0);
    one("mday 400", 125, 0, 400, 12, 0, 0);
    one("mday -400", 125, 0, -400, 12, 0, 0);
    one("mon 12", 125, 12, 15, 12, 0, 0);
    one("mon 25", 125, 25, 15, 12, 0, 0);
    one("mon -1", 125, -1, 15, 12, 0, 0);
    one("mon -25", 125, -25, 15, 12, 0, 0);
    one("mon 1 mday 31", 125, 1, 31, 12, 0, 0);  /* February 31: March 3 */
    one("mon 13 mday 29", 123, 13, 29, 12, 0, 0); /* month first: Feb 2024, a leap year */
    one("all negative", 125, -1, -1, -1, -1, -1);
    one("all large", 125, 30, 70, 30, 70, 70);
    one("into DST", 125, 2, 8, 27, 0, 0);       /* hour 27 of March 8: 2025-03-09 03:00 CDT */
    one("out of DST", 125, 10, 1, 27, 0, 0);     /* 2025-11-02 03:00 CST */
    return 0;
}
