/* 7.27.2.3 mktime: the instant one second before the epoch is a real time; mktime returns it and still sets wday/yday */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    time_t r;
    memset(&t, 0, sizeof t);
    /* 1969-12-31 18:59:59 in America/Chicago (CST, UTC-6) is time_t -1 */
    t.tm_year = 69; t.tm_mon = 11; t.tm_mday = 31; t.tm_hour = 18; t.tm_min = 59; t.tm_sec = 59; t.tm_isdst = -1;
    t.tm_wday = 9; t.tm_yday = 9;
    r = mktime(&t);
    printf("r=%lld wday=%d yday=%d isdst=%d\n", (long long)r, t.tm_wday, t.tm_yday, t.tm_isdst);
    return 0;
}
