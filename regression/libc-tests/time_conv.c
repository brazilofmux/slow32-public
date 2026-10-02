/* HOSTLEG -- <time.h>: broken-down time in both directions, the text
 * forms, every strftime conversion C99 names.  Local time is the zone
 * the harness sets (one with daylight time), asked of the host through
 * the emulator; the host's own library is the third opinion. */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void show(const char *what, const struct tm *t) {
    printf("%s %04d-%02d-%02d %02d:%02d:%02d wday=%d yday=%d isdst=%d\n", what,
           t->tm_year + 1900, t->tm_mon + 1, t->tm_mday, t->tm_hour, t->tm_min, t->tm_sec,
           t->tm_wday, t->tm_yday, t->tm_isdst);
}

static void mk(int y, int mo, int d, int h, int mi, int s) {
    struct tm t;
    time_t r;
    memset(&t, 0, sizeof t);
    t.tm_year = y - 1900; t.tm_mon = mo - 1; t.tm_mday = d; t.tm_hour = h; t.tm_min = mi; t.tm_sec = s;
    t.tm_isdst = -1;
    r = mktime(&t);
    printf("mktime(%d-%d-%d %d:%d:%d) = %lld -> ", y, mo, d, h, mi, s, (long long)r);
    show("", &t);
}

int main(void) {
    static long long stamps[] = {0, 1, 59, 86399, 86400, 68169600, 951782399, 951782400, 951868800,
                                 978307200, 1078012800, 1709164800, 1719792000, 1735689599, 1741507199,
                                 1741507200,
                                 1762065000, 1762066799, 1762066800, 1762068600, 2147483647};
    static const char *fmts[] = {"%a|%A|%b|%B|%h", "%c", "%C|%y|%Y|%G|%g", "%d|%e|%j", "%D|%F", "%H|%I|%M|%S|%p",
                                 "%r|%R|%T", "%u|%w|%U|%V|%W", "%x|%X", "%n|%t|%%", "plain text", ""};
    char buf[128];
    struct tm t, *g;
    time_t tt, back;
    int i, j;
    unsigned int n;

    for (i = 0; i < (int)(sizeof stamps / sizeof stamps[0]); i++) {
        tt = (time_t)stamps[i];
        g = gmtime(&tt);
        printf("%lld ", stamps[i]);
        show("gmtime", g);
        printf("  asctime %s", asctime(g));
        g = localtime(&tt);
        show("  localtime", g);
        n = (unsigned int)strftime(buf, sizeof buf, "%Z %z", g);
        printf("  zone [%s] %u\n", buf, n);
        printf("  ctime %s", ctime(&tt));
        t = *g;
        back = mktime(&t);
        printf("  mktime of it %lld\n", (long long)back);
    }

    /* every conversion, on days that tell the week-number rules apart */
    {
        static long long days[] = {1104537600 /* Sat 2005-01-01 */, 1136073599 /* Sat 2005-12-31 */,
                                   1167609600 /* Mon 2007-01-01 */, 1230768000 /* Thu 2009-01-01 */,
                                   1293753600 /* Fri 2010-12-31 */, 1325376000 /* Sun 2012-01-01 */,
                                   1709201106 /* Thu 2024-02-29 10:05:06 */, 1735603200 /* Tue 2024-12-31 */,
                                   1609372800 /* Thu 2020-12-31: week 53 of a leap year begun on a Wednesday */,
                                   1609459200 /* Fri 2021-01-01: still that week */,
                                   43200 /* noon */, 46799};
        for (i = 0; i < (int)(sizeof days / sizeof days[0]); i++) {
            tt = (time_t)days[i];
            g = gmtime(&tt);
            for (j = 0; j < (int)(sizeof fmts / sizeof fmts[0]); j++) {
                memset(buf, '#', sizeof buf);
                n = (unsigned int)strftime(buf, sizeof buf, fmts[j], g);
                printf("strftime[%s] = %u [%s]\n", fmts[j], n, n ? buf : "");
            }
        }
    }

    /* a result that does not fit, terminator included, is zero */
    tt = 1709201106;
    g = gmtime(&tt);
    printf("strftime into 11 bytes: %u\n", (unsigned int)strftime(buf, 11, "%Y-%m-%d", g));
    printf("strftime into 10 bytes: %u\n", (unsigned int)strftime(buf, 10, "%Y-%m-%d", g));
    printf("strftime into 1 byte: %u\n", (unsigned int)strftime(buf, 1, "%Y", g));
    printf("strftime into 0 bytes: %u\n", (unsigned int)strftime(buf, 0, "%Y", g));

    /* mktime brings fields into range and fills in the rest */
    mk(2024, 2, 29, 12, 0, 0);
    mk(2023, 2, 29, 12, 0, 0);          /* no such day: March 1 */
    mk(2024, 14, 1, 0, 0, 0);           /* month 14: February of the next year */
    mk(2024, 0, 15, 8, 0, 0);           /* month 0: December of the year before */
    mk(2024, -10, 15, 8, 0, 0);
    mk(2024, 3, 0, 0, 0, 0);            /* day 0: the last of February */
    mk(2024, 1, 366, 0, 0, 0);          /* day 366 of January: December 31 */
    mk(2024, 1, 1, 0, 0, -1);           /* one second before the year */
    mk(2024, 6, 15, 25, 61, 61);
    mk(2024, 6, 15, -1, -1, -1);
    mk(2024, 1, 15, 12, 0, 0);          /* standard time */
    mk(2024, 7, 15, 12, 0, 0);          /* daylight time */
    mk(1970, 1, 2, 0, 0, 0);
    mk(2000, 1, 1, 0, 0, 100000);
    mk(2038, 1, 18, 0, 0, 0);

    printf("difftime %.1f %.1f %.1f\n", difftime((time_t)1000, (time_t)400), difftime((time_t)400, (time_t)1000),
           difftime((time_t)86400, (time_t)86400));

    tt = time(0);
    printf("time: %s, and through its argument: %s\n", tt > (time_t)1700000000 ? "after 2023" : "EARLY",
           (back = 0, time(&back), difftime(back, tt) >= 0.0 && difftime(back, tt) < 5.0) ? "the same" : "DIFFERENT");
    printf("clock: %s\n", clock() != (clock_t)-1 ? "available" : "not available");
    return 0;
}
