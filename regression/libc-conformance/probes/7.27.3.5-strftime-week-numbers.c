/* 7.27.3.5p5 strftime: %U %W %V %G %g %j %u %w from Dec 25 to Jan 7 around year ends of every January 1 weekday */
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
    /* Jan 1 falls on each weekday, leap and common years both */
    static const int years[] = {1997, 1998, 2003, 2004, 2008, 2009, 2010, 2014, 2015, 2020, 2026, 2027, 2032, 2036};
    struct tm t;
    char buf[64];
    int i, d;
    for (i = 0; i < (int)(sizeof years / sizeof years[0]); i++) {
        for (d = 25; d <= 31 + 7; d++) {
            if (d <= 31) fill(&t, years[i], 12, d);
            else fill(&t, years[i] + 1, 1, d - 31);
            strftime(buf, sizeof buf, "%F %a U%U W%W V%V G%G g%g j%j u%u w%w", &t);
            printf("%s\n", buf);
        }
    }
    return 0;
}
