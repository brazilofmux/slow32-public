/* 7.27.3.5p7 strftime: every conversion in the "C" locale, on one hand-built time */
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
    static const char *conv[] = {"%a", "%A", "%b", "%B", "%c", "%C", "%d", "%D", "%e", "%F", "%g", "%G", "%h",
                                 "%H", "%I", "%j", "%m", "%M", "%n", "%p", "%r", "%R", "%S", "%t", "%T", "%u",
                                 "%U", "%V", "%w", "%W", "%x", "%X", "%y", "%Y", "%%", "a%%b%%"};
    struct tm t;
    char buf[100];
    int i, j;
    static const int dates[][3] = {{2026, 11, 1}, {1999, 1, 2}, {2024, 2, 29}};
    for (j = 0; j < 3; j++) {
        fill(&t, dates[j][0], dates[j][1], dates[j][2]);
        t.tm_hour = 7 + 8 * j; t.tm_min = 5 + j; t.tm_sec = 9 + 25 * j;
        for (i = 0; i < (int)(sizeof conv / sizeof conv[0]); i++) {
            size_t n = strftime(buf, sizeof buf, conv[i], &t);
            printf("%-6s %2u [%s]\n", conv[i], (unsigned)n, buf);
        }
    }
    return 0;
}
