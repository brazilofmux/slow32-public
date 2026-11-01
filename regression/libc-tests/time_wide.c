/* time_t is signed and 64 bits in both libraries: local times before
 * 1901, in the harness's zone (America/Chicago), whose local mean time
 * (-5:50:36) ran until noon on 1883-11-18.  The host is not asked: macOS's
 * mktime refuses a local time before 1901 (-1), and its %z gives the
 * zone's standard offset instead of tm_gmtoff (see time_zone.c), so what
 * this prints is written down beside it -- the zone's own rules, as
 * glibc and the host's localtime_r report them. */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void show(const char *what, const struct tm *t) {
    printf("%s %04d-%02d-%02d %02d:%02d:%02d wday=%d yday=%d isdst=%d gmtoff=%ld\n", what,
           t->tm_year + 1900, t->tm_mon + 1, t->tm_mday, t->tm_hour, t->tm_min, t->tm_sec,
           t->tm_wday, t->tm_yday, t->tm_isdst, (long)t->tm_gmtoff);
}

int main(void) {
    static long long stamps[] = {-2208988800LL /* 1900 */, -2717647201LL /* LMT's last second */,
                                 -2717646600LL /* 12:10 CST (12:00-12:09:23 came twice) */,
                                 -4294967294LL /* 1833 */, -62135596800LL /* year 1 */};
    char buf[64];
    struct tm t, *g;
    time_t tt, back;
    int i;

    for (i = 0; i < (int)(sizeof stamps / sizeof stamps[0]); i++) {
        tt = (time_t)stamps[i];
        printf("%lld ", stamps[i]);
        show("gmtime", gmtime(&tt));
        g = localtime(&tt);
        show("  localtime", g);
        strftime(buf, sizeof buf, "%Z %z|%c", g);
        printf("  [%s]\n", buf);
        t = *g;
        back = mktime(&t);
        printf("  mktime of it %lld\n", (long long)back);
    }

    /* and mktime from fields: a local mean time, seconds off the hour */
    memset(&t, 0, sizeof t);
    t.tm_year = 1833 - 1900; t.tm_mon = 10; t.tm_mday = 24; t.tm_hour = 11; t.tm_min = 41; t.tm_sec = 10;
    t.tm_isdst = -1;
    tt = mktime(&t);
    printf("mktime(1833-11-24 11:41:10) = %lld -> ", (long long)tt);
    show("", &t);
    return 0;
}
