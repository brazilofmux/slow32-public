/* 7.27.3.5p5 strftime: %G and %g for week-based years from 1 to 2000, unpadded and two-digit */
#include <stdio.h>
#include <string.h>
#include <time.h>

/* the date fields of y-m-d, wday and yday computed here, not by the library */
static long long days_from_civil(long long y, int m, int d) {
    long long era, yoe, doy, doe;
    y -= m <= 2;
    era = (y >= 0 ? y : y - 399) / 400;
    yoe = y - era * 400;
    doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
}

static void fill(struct tm *t, long long y, int m, int d) {
    long long z = days_from_civil(y, m, d);
    memset(t, 0, sizeof *t);
    t->tm_year = (int)(y - 1900);
    t->tm_mon = m - 1;
    t->tm_mday = d;
    t->tm_yday = (int)(z - days_from_civil(y, 1, 1));
    t->tm_wday = (int)(((z + 4) % 7 + 7) % 7);
}

int main(void) {
    static const long long years[] = {2000, 1000, 999, 100, 10, 1};
    struct tm t;
    char buf[64];
    int i;
    for (i = 0; i < (int)(sizeof years / sizeof years[0]); i++) {
        fill(&t, years[i], 1, 1);
        strftime(buf, sizeof buf, "Jan 1 %a V%V G%G g%g", &t);
        printf("%lld: %s\n", years[i], buf);
        fill(&t, years[i], 6, 15);
        strftime(buf, sizeof buf, "Jun 15 %a V%V G%G g%g", &t);
        printf("%lld: %s\n", years[i], buf);
    }
    return 0;
}
