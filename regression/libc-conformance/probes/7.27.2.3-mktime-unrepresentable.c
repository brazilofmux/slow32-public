/* 7.27.2.3p3 mktime: a time whose normalized year does not fit tm_year cannot be represented: (time_t)-1 */
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    time_t r;
    memset(&t, 0, sizeof t);
    t.tm_year = INT_MAX; t.tm_mon = 12; t.tm_mday = 1; t.tm_isdst = -1;   /* the year after tm_year's last */
    r = mktime(&t);
    printf("year INT_MAX+1901: %s\n", r == (time_t)-1 ? "-1" : "a value");
    memset(&t, 0, sizeof t);
    t.tm_year = INT_MIN; t.tm_mon = -1; t.tm_mday = 1; t.tm_isdst = -1;   /* the year before tm_year's first */
    r = mktime(&t);
    printf("year INT_MIN+1899: %s\n", r == (time_t)-1 ? "-1" : "a value");
    memset(&t, 0, sizeof t);
    t.tm_year = INT_MAX; t.tm_mon = 11; t.tm_mday = 31; t.tm_hour = 23; t.tm_min = 59; t.tm_sec = 59 + 3600 * 24;
    t.tm_isdst = -1;
    r = mktime(&t);
    printf("a day past the last day: %s\n", r == (time_t)-1 ? "-1" : "a value");
    return 0;
}
