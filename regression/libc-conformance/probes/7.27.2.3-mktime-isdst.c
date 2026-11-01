/* 7.27.2.3 mktime: tm_isdst positive/zero presumes DST is/is not in effect, negative determines it (fn 320) */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void one(int y, int mo, int d, int h, int mi, int isdst) {
    struct tm t;
    time_t r;
    memset(&t, 0, sizeof t);
    t.tm_year = y - 1900; t.tm_mon = mo - 1; t.tm_mday = d; t.tm_hour = h; t.tm_min = mi;
    t.tm_isdst = isdst;
    r = mktime(&t);
    printf("%04d-%02d-%02d %02d:%02d isdst=%2d -> %lld  %04d-%02d-%02d %02d:%02d isdst=%d\n", y, mo, d, h, mi,
           isdst, (long long)r, t.tm_year + 1900, t.tm_mon + 1, t.tm_mday, t.tm_hour, t.tm_min, t.tm_isdst);
}

int main(void) {
    int k;
    for (k = -1; k <= 1; k++) one(2025, 1, 15, 12, 0, k);     /* winter: CST */
    for (k = -1; k <= 1; k++) one(2025, 7, 15, 12, 0, k);     /* summer: CDT */
    /* 01:30 on 2025-11-02 happens twice: first CDT, then CST */
    one(2025, 11, 2, 1, 30, 1);
    one(2025, 11, 2, 1, 30, 0);
    /* a positive value other than 1 */
    one(2025, 7, 15, 12, 0, 5);
    return 0;
}
